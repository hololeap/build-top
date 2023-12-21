{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
{-# Language StandaloneDeriving #-}
{-# Language TupleSections #-}
{-# Language TypeFamilies #-}

module BuildTop.Types where

import Control.Applicative ((<|>), empty)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Either (partitionEithers)
import Data.Function
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Kind
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.These
import System.Directory
import System.FilePath
import qualified System.Linux.Inotify as Inotify
import Reflex
import Witherable

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types

import BuildTop.Util

-- | Useful for debugging, may be replaced with BuildTopEvent in the future
type MyEvent = These Inotify.Event BuildTopEvent

data WatchType
    = WatchDirectory
    | WatchFile
    deriving (Show, Eq, Ord, Bounded, Enum)

data WatchLayer
    = RootLayer
    | CategoryLayer
    | PackageLayer
    | TempDirLayer
    | LogFileLayer
    deriving (Show, Eq, Ord, Bounded, Enum)

type LockFileExists = Bool

class HasChildren (l :: WatchLayer) where
    type ChildContainer l :: Type -> Type
    type ChildContainerKey l :: Type
    type ChildLayer l :: WatchLayer
    getChildren :: Watcher l t a -> ChildContainer l (Watcher (ChildLayer l) t a)

    -- | Map a @Watcher@'s children to another value. If the inner function
    --   returns @Nothing@, the child will be deleted.
    --
    --   This will never delete the root.
    --
    --   (Inspired by @witherM@ from the @witherable@ package)
    witherWatcher :: Monad m
        => (forall x. (IsWatcher x, Eq (Watcher x t a))
                => Watcher x t a -> m (Maybe (Watcher x t a))
           )
        -> Watcher l t a
        -> m (Watcher l t a)

    -- | Very generic way to alter a specific part of the watcher tree. This
    --   is able to insert, delete, or update.
    --
    --   See 'AlterPayload' for more information on how to use it.
    alterWatcher
        :: Monad m
        => AlterPayload t m a
        -> Watcher l t a
        -> m (Watcher l t a)

-- | Insert a child for the given parent
--
--   See 'InsertPayload' for more information on how to use it.
insertWatcher
    :: HasChildren l
    => InsertPayload t a
    -> Watcher l t a
    -> Watcher l t a
insertWatcher (InsertPayload key child) =
    let f = case key of
                RootKey -> \(RootWatcher cs fp d) ->
                    -- We need to index the child by its 'Category'
                    let CategoryWatcher _ cat _ = child
                    in RootWatcher (M.insert cat child cs) fp d
                CategoryKey _ -> \(CategoryWatcher cs cat d) ->
                    -- We need to index the child by its 'Package'
                    let PackageWatcher _ pkg _ = child
                    in CategoryWatcher (M.insert pkg (False, child) cs) cat d
                PackageKey _ -> \(PackageWatcher _ pkg d) ->
                    PackageWatcher (Just child) pkg d
                TempDirKey _ -> \(TempDirWatcher _ pkg d) ->
                    TempDirWatcher (Just child) pkg d
                LogFileKey _ ->
                    -- log-file level does not have any children
                    error "LogFileKey should not be used with insertWatcher"

    in runIdentity
        . alterWatcher
            (AlterPayload key (pure . fmap f))

-- | Delete a watcher at the given location
--
--   See 'DeletePayload' for more information on how to use it.
deleteWatcher
    :: HasChildren l
    => DeletePayload
    -> Watcher l t a
    -> Watcher l t a
deleteWatcher (DeletePayload key) =
              -- delete the watcher with the matching key
    let f w | watcherKey w == key = empty
            | otherwise = pure w
    in runIdentity
        . alterWatcher
            (AlterPayload key (runMaybeT . (f <=< liftMaybe)))

-- | Update the state of the lock file for a given package.
updateLockFile
    :: HasChildren l
    => LockFileExists
    -> Package
    -> Watcher l t a
    -> Watcher l t a
updateLockFile lfe pkg =
    let cat = getCategory pkg
    in runIdentity
        . alterWatcher
            (AlterPayload (CategoryKey cat) (pure . fmap f))
  where
    f (CategoryWatcher cs c d) =
        let adj (_,w) = (lfe,w)
        in CategoryWatcher (M.adjust adj pkg cs) c d

-- | Holds a 'WatcherKey' (used to find the Watcher) and a function to update
--   it. Used by 'alterWatcher'.
--
--   There are a few things to note:
--
--   * 'alterWatcher' behaves differently on the 'RootLayer' than on the other
--     layers. If the root watcher were to be deleted, it will instead be
--     returned without any modification.
--   * The input given to the inner function will be @Just watcher@ if the
--     watcher specified by the key exists. Otherwise the input will be
--     @Nothing@.
--   * If the inner function's /output/ is @Nothing@, the matching watcher
--     will be deleted (except for 'RootWatcher' which will remain unchanged).
--
--   Hides 'WatchLayer' information from top-level. This allows for passing
--   the data down through the tree in a uniform way.
data AlterPayload t m a where
    AlterPayload
        :: WatcherKey l
        -> (Maybe (Watcher l t a) -> m (Maybe (Watcher l t a)))
        -> AlterPayload t m a

-- | Holds the key of the parent and the new child. Used by 'insertWatcher'.
data InsertPayload t a where
    InsertPayload
        :: HasChildren l
        => WatcherKey l
        -> Watcher (ChildLayer l) t a
        -> InsertPayload t a

-- | Holds the key of the watcher to be deleted. Used by 'deleteWatcher'
data DeletePayload where
    DeletePayload :: IsWatcher l => WatcherKey l -> DeletePayload

instance HasChildren 'RootLayer where
    type ChildContainer 'RootLayer = HashMap Category
    type ChildContainerKey 'RootLayer = Category
    type ChildLayer 'RootLayer = 'CategoryLayer
    getChildren = rootWatcher_Children

    witherWatcher f (RootWatcher cs0 p d) = do
        cs <- witherM (f <=< witherWatcher f) cs0
        pure $ RootWatcher cs p d

    alterWatcher (AlterPayload RootKey f) w = do
        -- Special case: Normally we may delete the node, but here we may only
        -- modify it or return the original.
        -- Only really useful for inserting new categories.
        mw <- f (Just w)
        pure $ fromMaybe w mw
    alterWatcher up (RootWatcher cs fp d) = do
        cs' <- case up of
            AlterPayload (CategoryKey cat) f
                -> M.alterF f cat cs
            AlterPayload (PackageKey pkg) _
                -> sendDownstream (getCategory pkg)
            AlterPayload (TempDirKey pkg) _
                -> sendDownstream (getCategory pkg)
            AlterPayload (LogFileKey pkg) _
                -> sendDownstream (getCategory pkg)
        pure $ RootWatcher cs' fp d
      where
        sendDownstream cat =
            M.alterF
                (maybe (pure Nothing) (fmap Just . alterWatcher up))
                cat
                cs

instance HasChildren 'CategoryLayer where
    type ChildContainer 'CategoryLayer = HashMap Package
    type ChildContainerKey 'CategoryLayer = Package
    type ChildLayer 'CategoryLayer = 'PackageLayer
    getChildren = fmap snd . categoryWatcher_Children

    witherWatcher f (CategoryWatcher cs0 c d) = do
        cs <- witherM
            (\(lfe, cs) -> do
                cs' <- f =<< witherWatcher f cs
                pure $ (lfe,) <$> cs'
            )
            cs0
        pure $ CategoryWatcher cs c d

    alterWatcher up (CategoryWatcher cs cat d) = do
        cs' <- case up of
            AlterPayload (PackageKey pkg) f ->
                let alt (b,w) = (b,) <$> MaybeT (f (Just w))
                in M.alterF (runMaybeT . (alt <=< liftMaybe)) pkg cs
            AlterPayload (TempDirKey pkg) _ -> sendDownstream pkg
            AlterPayload (LogFileKey pkg) _ -> sendDownstream pkg
            _ -> pure cs
        pure $ CategoryWatcher cs' cat d
      where
        sendDownstream pkg =
            let alt (b,w) = (b,) <$> lift (alterWatcher up w)
            in M.alterF (runMaybeT . (alt <=< liftMaybe)) pkg cs

instance HasChildren 'PackageLayer where
    type ChildContainer 'PackageLayer = Maybe
    type ChildContainerKey 'PackageLayer = ()
    type ChildLayer 'PackageLayer = 'TempDirLayer
    getChildren = packageWatcher_Children

    witherWatcher f (PackageWatcher cs0 p d) = do
        cs <- witherM (f <=< witherWatcher f) cs0
        pure $ PackageWatcher cs p d

    alterWatcher up (PackageWatcher cs pkg d) = do
        cs' <- case up of
            AlterPayload (TempDirKey _) f -> f cs
            AlterPayload (LogFileKey _) _ ->
                mapM (alterWatcher up) cs
            _ -> pure cs
        pure $ PackageWatcher cs' pkg d

instance HasChildren 'TempDirLayer where
    type ChildContainer 'TempDirLayer = Maybe
    type ChildContainerKey 'TempDirLayer = ()
    type ChildLayer 'TempDirLayer = 'LogFileLayer
    getChildren = tempDirWatcher_Children

    witherWatcher f (TempDirWatcher cs0 p d) = do
        cs <- witherM f cs0
        pure $ TempDirWatcher cs p d

    alterWatcher up (TempDirWatcher cs p d) = do
        cs' <- case up of
            AlterPayload (LogFileKey _) f -> f cs
            _ -> pure cs
        pure $ TempDirWatcher cs' p d

class IsWatcher (l :: WatchLayer) where
    watcherType :: proxy l -> WatchType
    default watcherType :: HasChildren l => proxy l -> WatchType
    watcherType _ = WatchDirectory

    -- | Return the @a@ from the data structure
    getWatcher :: Watcher l t a -> a

    -- | Return the @a@ from the data structure as well as all @a@s from the
    --   child tree
    getWatchers :: Watcher l t a -> [a]
    default getWatchers
        :: (HasChildren l, IsWatcher (ChildLayer l), Foldable (ChildContainer l))
        => Watcher l t a -> [a]
    getWatchers w = getWatcher w : foldMap getWatchers (getChildren w)

    lookupWatcher
        :: WatcherKey l
        -> Watcher 'RootLayer t a
        -> Maybe (Watcher l t a)

    watcherKey :: Watcher l t a -> WatcherKey l

    -- | Apply an operation on every watcher in the tree, updating each one
    --   using the given function.
    updateAllWatcher
        :: Monad m
        => (forall x. IsWatcher x => Watcher x t a -> m (Watcher x t a))
        -> Watcher l t a
        -> m (Watcher l t a)

instance IsWatcher 'RootLayer where
    getWatcher = rootWatcher_Data
    lookupWatcher _ w0 = Just w0
    watcherKey _ = RootKey

    updateAllWatcher f w = f w >>= \case -- update self first
        RootWatcher cs fp d -> do
            cs' <- traverse (updateAllWatcher f) cs -- update children
            pure $ RootWatcher cs' fp d


instance IsWatcher 'CategoryLayer where
    getWatcher = categoryWatcher_Data
    lookupWatcher (CategoryKey c) w0 =
        M.lookup c (rootWatcher_Children w0)
    watcherKey = CategoryKey . categoryWatcher_Category

    updateAllWatcher f w0 = f w0 >>= \case
        CategoryWatcher cs cat d -> do
            cs' <- traverse (\(b,w) -> (b,) <$> f w) cs
            pure $ CategoryWatcher cs' cat d

instance IsWatcher 'PackageLayer where
    getWatcher = packageWatcher_Data
    lookupWatcher (PackageKey p) w0 = do
        w <- lookupWatcher (CategoryKey (getCategory p)) w0
        snd <$> M.lookup p (categoryWatcher_Children w)
    watcherKey = PackageKey . packageWatcher_Package

    updateAllWatcher f w = f w >>= \case
        PackageWatcher cs pkg d -> do
            cs' <- traverse f cs
            pure $ PackageWatcher cs' pkg d

instance IsWatcher 'TempDirLayer where
    getWatcher = tempDirWatcher_Data
    lookupWatcher (TempDirKey p) w0 = do
        w <- lookupWatcher (PackageKey p) w0
        packageWatcher_Children w
    watcherKey = TempDirKey . tempDirWatcher_Package

    updateAllWatcher f w = f w >>= \case
        TempDirWatcher cs cat d -> do
            cs' <- traverse f cs
            pure $ TempDirWatcher cs' cat d

instance IsWatcher 'LogFileLayer where
    watcherType _ = WatchFile
    getWatcher = logFileWatcher_Data
    getWatchers = (:[]) . getWatcher
    lookupWatcher (LogFileKey p) w0 = do
        w <- lookupWatcher (TempDirKey p) w0
        tempDirWatcher_Children w
    watcherKey = LogFileKey . logFileWatcher_Package

    updateAllWatcher f w = f w -- no children

data WatcherData t = WatcherData
    { getWatch :: Inotify.Watch
    , getEvent :: Event t MyEvent
    }

data Watcher (l :: WatchLayer) t a where
    RootWatcher ::
        { rootWatcher_Children :: HashMap Category (Watcher 'CategoryLayer t a)
        , rootWatcher_Path :: FilePath
        , rootWatcher_Data :: a
        } -> Watcher 'RootLayer t a
    CategoryWatcher ::
        { categoryWatcher_Children :: HashMap Package (LockFileExists, Watcher 'PackageLayer t a)
        , categoryWatcher_Category :: Category
        , categoryWatcher_Data :: a
        } -> Watcher 'CategoryLayer t a
    PackageWatcher ::
        { packageWatcher_Children :: Maybe (Watcher 'TempDirLayer t a)
        , packageWatcher_Package :: Package
        , packageWatcher_Data :: a
        } -> Watcher 'PackageLayer t a
    TempDirWatcher ::
        { tempDirWatcher_Children :: Maybe (Watcher 'LogFileLayer t a)
        , tempDirWatcher_Package :: Package
        , tempDirWatcher_Data :: a
        } -> Watcher 'TempDirLayer t a
    LogFileWatcher ::
        { logFileWatcher_Package :: Package
        , logFileWatcher_Data :: a
        } -> Watcher 'LogFileLayer t a

deriving instance Functor (Watcher l t)
deriving instance IsWatcher l => Traversable (Watcher l t)

-- TODO: Is this necessary or does the derived Foldable instance work as
--       intended? Set up a test case to see if the derived Foldable matches
--       this.
instance IsWatcher l => Foldable (Watcher l t) where
    foldMap f = foldMap f . getWatchers

instance Eq (Watcher 'RootLayer t a) where
    (==) = (==) `on` rootWatcher_Path

instance Eq (Watcher 'CategoryLayer t a) where
    (==) = (==) `on` categoryWatcher_Category

instance Eq (Watcher 'PackageLayer t a) where
    (==) = (==) `on` packageWatcher_Package

instance Eq (Watcher 'TempDirLayer t a) where
    (==) = (==) `on` tempDirWatcher_Package

instance Eq (Watcher 'LogFileLayer t a) where
    (==) = (==) `on` logFileWatcher_Package

-- | Used to lookup a particular part of a Watcher tree
data WatcherKey (l :: WatchLayer) where
    RootKey :: WatcherKey 'RootLayer
    CategoryKey :: Category -> WatcherKey 'CategoryLayer
    PackageKey :: Package -> WatcherKey 'PackageLayer
    TempDirKey :: Package -> WatcherKey 'TempDirLayer
    LogFileKey :: Package -> WatcherKey 'LogFileLayer

deriving instance Show (WatcherKey l)
deriving instance Eq (WatcherKey l)
deriving instance Ord (WatcherKey l)

-- | Representation of @WatcherKey@ with type-level tag removed. This allows
--   for e.g. comparing keys from different levels.
data SimpleKey
    = RootSimple
    | CategorySimple Category
    | PackageSimple Package
    | TempDirSimple Package
    | LogFileSimple Package
    deriving (Show, Eq, Ord)

toSimpleKey :: WatcherKey l -> SimpleKey
toSimpleKey RootKey = RootSimple
toSimpleKey (CategoryKey c) = CategorySimple c
toSimpleKey (PackageKey p) = PackageSimple p
toSimpleKey (TempDirKey p) = TempDirSimple p
toSimpleKey (LogFileKey p) = LogFileSimple p

data EventType
    = AddEvent
    | RemoveEvent
    deriving (Show, Eq, Ord, Bounded, Enum)

data BuildTopEvent
    = CategoryEvent EventType Category
    | PackageEvent EventType Package
    | LockFileEvent EventType Package
    | TempDirEvent EventType Package
    | LogFileEvent EventType Package
    | LogFileWriteEvent Package
    deriving (Show, Eq, Ord)

-- | Trigger a 'BuildTopEvent' either with an inotify event or a manual
-- | event passed as a FilePath (the name of a file/directory).
type EventFilter =
    ( Inotify.Event -> Maybe BuildTopEvent
    , FilePath -> IO (Maybe BuildTopEvent)
    )

-- | A class for layers that have a directory that needs to be scanned to find
--   relevant children.
class HasDirectory (l :: WatchLayer) where
    type ScanInput l :: Type
    type ScanInput l = ()
    type ScanData l :: Type
    processDir
        :: MonadIO m
        => Proxy l
        -> ScanInput l
        -> [FilePath] -- ^ real path, not just the base name
        -> m (ScanData l)

-- | Scan a directory given a layer with a @HasDirectory@ instance, its
--   input, and a path to the given directory. Sends the full path of the file
--   to @processDir@.
scanDirectory
    :: (HasDirectory l, MonadIO m)
    => Proxy l
    -> ScanInput l
    -> FilePath -- ^ Directory to scan
    -> m (ScanData l)
scanDirectory proxy i p0 = do
    fs <- lenientListDirectory p0
    processDir proxy i (map (p0 </>) fs)

instance HasDirectory 'RootLayer where
    type ScanData 'RootLayer = [Category]
    processDir _ _ = witherM $ \f -> runMaybeT $ do
        c <- liftMaybe $ parseMaybe $ takeFileName f
        liftIO (doesDirectoryExist f) >>= guard
        pure c

-- | Scan a list of FilePaths, sorting them into a set of lockfiles and a
--   list of packages (directories).
instance HasDirectory 'CategoryLayer where
    type ScanInput 'CategoryLayer = Category
    type ScanData 'CategoryLayer = (Set Package, [Package])
    processDir _ c fs = do
        let checkLockFile f = do
                p <- liftMaybe $ lockFilePackage c $ takeFileName f
                liftIO (doesFileExist f) >>= guard
                pure p
            checkPkgDir f = do
                p <- liftMaybe $ parseMaybe $ toString c </> takeFileName f
                liftIO (doesDirectoryExist f) >>= guard
                pure p
            identifyPath f = runMaybeT $
                    Left <$> checkLockFile f
                <|> Right <$> checkPkgDir f
        (lockPs, pkgPs) <- partitionEithers <$> witherM identifyPath fs
        pure (S.fromList lockPs, pkgPs)

instance HasDirectory 'PackageLayer where
    type ScanData 'PackageLayer = Maybe FilePath
    processDir _ _ =
        let go = witherM $ \f -> runMaybeT $ do
                guard (takeFileName f == "temp")
                liftIO (doesDirectoryExist f) >>= guard
                pure f
        in fmap listToMaybe . go

instance HasDirectory 'TempDirLayer where
    type ScanData 'TempDirLayer = Maybe FilePath
    processDir _ _ =
        let go = witherM $ \f -> runMaybeT $ do
                guard (takeFileName f == "build.log")
                liftIO (doesFileExist f) >>= guard
                pure f
        in fmap listToMaybe . go
