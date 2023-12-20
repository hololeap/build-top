{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
{-# Language StandaloneDeriving #-}
{-# Language TupleSections #-}
{-# Language TypeFamilies #-}

module BuildTop.Types where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Either (partitionEithers)
import Data.Function
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Kind
import Data.Maybe (listToMaybe)
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
    witherWatcher :: Monad m
        => (forall x. (IsWatcher x, Eq (Watcher x t a))
                => Watcher x t a -> m (Maybe (Watcher x t a))
           )
        -> Watcher l t a
        -> m (Watcher l t a)

instance HasChildren 'RootLayer where
    type ChildContainer 'RootLayer = HashMap Category
    type ChildContainerKey 'RootLayer = Category
    type ChildLayer 'RootLayer = 'CategoryLayer
    getChildren = rootWatcher_Children

    witherWatcher f (RootWatcher cs0 p d) = do
        cs <- witherM (f <=< witherWatcher f) cs0
        pure $ RootWatcher cs p d

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

instance HasChildren 'PackageLayer where
    type ChildContainer 'PackageLayer = Maybe
    type ChildContainerKey 'PackageLayer = ()
    type ChildLayer 'PackageLayer = 'TempDirLayer
    getChildren = packageWatcher_Children

    witherWatcher f (PackageWatcher cs0 p d) = do
        cs <- witherM (f <=< witherWatcher f) cs0
        pure $ PackageWatcher cs p d

instance HasChildren 'TempDirLayer where
    type ChildContainer 'TempDirLayer = Maybe
    type ChildContainerKey 'TempDirLayer = ()
    type ChildLayer 'TempDirLayer = 'LogFileLayer
    getChildren = tempDirWatcher_Children

    witherWatcher f (TempDirWatcher cs0 p d) = do
        cs <- witherM f cs0
        pure $ TempDirWatcher cs p d

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

instance IsWatcher 'RootLayer where
    getWatcher = rootWatcher_Data
    lookupWatcher _ w0 = Just w0
    watcherKey _ = RootKey

instance IsWatcher 'CategoryLayer where
    getWatcher = categoryWatcher_Data
    lookupWatcher (CategoryKey c) w0 =
        M.lookup c (rootWatcher_Children w0)
    watcherKey = CategoryKey . categoryWatcher_Category

instance IsWatcher 'PackageLayer where
    getWatcher = packageWatcher_Data
    lookupWatcher (PackageKey p) w0 = do
        w <- lookupWatcher (CategoryKey (getCategory p)) w0
        snd <$> M.lookup p (categoryWatcher_Children w)
    watcherKey = PackageKey . packageWatcher_Package

instance IsWatcher 'TempDirLayer where
    getWatcher = tempDirWatcher_Data
    lookupWatcher (TempDirKey p) w0 = do
        w <- lookupWatcher (PackageKey p) w0
        packageWatcher_Children w
    watcherKey = TempDirKey . tempDirWatcher_Package

instance IsWatcher 'LogFileLayer where
    watcherType _ = WatchFile
    getWatcher = logFileWatcher_Data
    getWatchers = (:[]) . getWatcher
    lookupWatcher (LogFileKey p) w0 = do
        w <- lookupWatcher (TempDirKey p) w0
        tempDirWatcher_Children w
    watcherKey = LogFileKey . logFileWatcher_Package

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
