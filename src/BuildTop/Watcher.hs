{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language KindSignatures #-}
{-# Language LambdaCase #-}
{-# Language RankNTypes #-}
{-# Language StandaloneDeriving #-}
{-# Language TupleSections #-}
{-# Language TypeFamilies #-}

module BuildTop.Watcher where

import Control.Applicative (empty)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Function (on)
import Data.Functor.Identity
import Data.Kind
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe)
import Reflex
import qualified System.Linux.Inotify as Inotify
import Witherable (witherM)

import Distribution.Portage.Types

import BuildTop.Filesystem
import BuildTop.Types
import BuildTop.Util

import Prelude hiding (lookup)

-- | A class with methods specific to layers that have children (e.g. all
--   but 'LogFileLayer').
class ParentLayer (l :: WatchLayer) where
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
    wither :: Monad m
        => (forall x. (BasicLayer x, Eq (Watcher x t a))
                => Watcher x t a -> m (Maybe (Watcher x t a))
           )
        -> Watcher l t a
        -> m (Watcher l t a)

    -- | Very generic way to alter a specific part of the watcher tree. This
    --   is able to insert, delete, or update.
    --
    --   See 'AlterPayload' for more information on how to use it.
    alter
        :: Monad m
        => AlterPayload t m a
        -> Watcher l t a
        -> m (Watcher l t a)

-- | Insert a child for the given parent
--
--   See 'InsertPayload' for more information on how to use it.
insert
    :: ParentLayer l
    => InsertPayload t a
    -> Watcher l t a
    -> Watcher l t a
insert (InsertPayload key child) =
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
        . alter
            (AlterPayload key (pure . fmap f))

-- | Delete a watcher at the given location
--
--   See 'DeletePayload' for more information on how to use it.
delete
    :: ParentLayer l
    => DeletePayload
    -> Watcher l t a
    -> Watcher l t a
delete (DeletePayload key) =
              -- delete the watcher with the matching key
    let f w | watcherKey w == key = empty
            | otherwise = pure w
    in runIdentity
        . alter
            (AlterPayload key (runMaybeT . (f <=< liftMaybe)))

-- | Update the state of the lock file for a given package.
updateLockFile
    :: ParentLayer l
    => LockFileExists
    -> Package
    -> Watcher l t a
    -> Watcher l t a
updateLockFile lfe pkg =
    let cat = getCategory pkg
    in runIdentity
        . alter
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
        :: ParentLayer l
        => WatcherKey l
        -> Watcher (ChildLayer l) t a
        -> InsertPayload t a

-- | Holds the key of the watcher to be deleted. Used by 'deleteWatcher'
data DeletePayload where
    DeletePayload :: BasicLayer l => WatcherKey l -> DeletePayload

instance ParentLayer 'RootLayer where
    type ChildContainer 'RootLayer = HashMap Category
    type ChildContainerKey 'RootLayer = Category
    type ChildLayer 'RootLayer = 'CategoryLayer
    getChildren = rootWatcher_Children

    wither f (RootWatcher cs0 p d) = do
        cs <- witherM (f <=< wither f) cs0
        pure $ RootWatcher cs p d

    alter (AlterPayload RootKey f) w = do
        -- Special case: Normally we may delete the node, but here we may only
        -- modify it or return the original.
        -- Only really useful for inserting new categories.
        mw <- f (Just w)
        pure $ fromMaybe w mw
    alter up (RootWatcher cs fp d) = do
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
                (maybe (pure Nothing) (fmap Just . alter up))
                cat
                cs

instance ParentLayer 'CategoryLayer where
    type ChildContainer 'CategoryLayer = HashMap Package
    type ChildContainerKey 'CategoryLayer = Package
    type ChildLayer 'CategoryLayer = 'PackageLayer
    getChildren = fmap snd . categoryWatcher_Children

    wither f (CategoryWatcher cs0 c d) = do
        cs <- witherM
            (\(lfe, cs) -> do
                cs' <- f =<< wither f cs
                pure $ (lfe,) <$> cs'
            )
            cs0
        pure $ CategoryWatcher cs c d

    alter up (CategoryWatcher cs cat d) = do
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
            let alt (b,w) = (b,) <$> lift (alter up w)
            in M.alterF (runMaybeT . (alt <=< liftMaybe)) pkg cs

instance ParentLayer 'PackageLayer where
    type ChildContainer 'PackageLayer = Maybe
    type ChildContainerKey 'PackageLayer = ()
    type ChildLayer 'PackageLayer = 'TempDirLayer
    getChildren = packageWatcher_Children

    wither f (PackageWatcher cs0 p d) = do
        cs <- witherM (f <=< wither f) cs0
        pure $ PackageWatcher cs p d

    alter up (PackageWatcher cs pkg d) = do
        cs' <- case up of
            AlterPayload (TempDirKey _) f -> f cs
            AlterPayload (LogFileKey _) _ ->
                mapM (alter up) cs
            _ -> pure cs
        pure $ PackageWatcher cs' pkg d

instance ParentLayer 'TempDirLayer where
    type ChildContainer 'TempDirLayer = Maybe
    type ChildContainerKey 'TempDirLayer = ()
    type ChildLayer 'TempDirLayer = 'LogFileLayer
    getChildren = tempDirWatcher_Children

    wither f (TempDirWatcher cs0 p d) = do
        cs <- witherM f cs0
        pure $ TempDirWatcher cs p d

    alter up (TempDirWatcher cs p d) = do
        cs' <- case up of
            AlterPayload (LogFileKey _) f -> f cs
            _ -> pure cs
        pure $ TempDirWatcher cs' p d

-- | A class with methods for all layers of the 'Watcher' tree.
class BasicLayer (l :: WatchLayer) where
    watcherType :: proxy l -> WatchType
    default watcherType :: ParentLayer l => proxy l -> WatchType
    watcherType _ = WatchDirectory

    -- | Return the @a@ from the data structure
    extract :: Watcher l t a -> a

    -- | Return the @a@ from the data structure as well as all @a@s from the
    --   child tree
    toList :: Watcher l t a -> [a]
    default toList
        :: (ParentLayer l, BasicLayer (ChildLayer l), Foldable (ChildContainer l))
        => Watcher l t a -> [a]
    toList w = extract w : foldMap toList (getChildren w)

    lookup
        :: WatcherKey l
        -> Watcher 'RootLayer t a
        -> Maybe (Watcher l t a)

    watcherKey :: Watcher l t a -> WatcherKey l

    -- | Apply an operation on every watcher in the tree, updating each one
    --   using the given function.
    --
    --   Note that the forest of children will be traversed /after/ the parent
    --   has been updated. This means that if you modify the children during
    --   the parent's update, the update will be applied directly to each
    --   /modified/ child when the forest is traversed.
    updateAll
        :: Monad m
        => (forall x. BasicLayer x => Watcher x t a -> m (Watcher x t a))
        -> Watcher l t a
        -> m (Watcher l t a)

instance BasicLayer 'RootLayer where
    extract = rootWatcher_Data
    lookup _ w0 = Just w0
    watcherKey _ = RootKey

    updateAll f w = f w >>= \case -- update self first
        RootWatcher cs fp d -> do
            cs' <- traverse (updateAll f) cs -- update children
            pure $ RootWatcher cs' fp d


instance BasicLayer 'CategoryLayer where
    extract = categoryWatcher_Data
    lookup (CategoryKey c) w0 =
        M.lookup c (rootWatcher_Children w0)
    watcherKey = CategoryKey . categoryWatcher_Category

    updateAll f w0 = f w0 >>= \case
        CategoryWatcher cs cat d -> do
            cs' <- traverse (\(b,w) -> (b,) <$> f w) cs
            pure $ CategoryWatcher cs' cat d

instance BasicLayer 'PackageLayer where
    extract = packageWatcher_Data
    lookup (PackageKey p) w0 = do
        w <- lookup (CategoryKey (getCategory p)) w0
        snd <$> M.lookup p (categoryWatcher_Children w)
    watcherKey = PackageKey . packageWatcher_Package

    updateAll f w = f w >>= \case
        PackageWatcher cs pkg d -> do
            cs' <- traverse f cs
            pure $ PackageWatcher cs' pkg d

instance BasicLayer 'TempDirLayer where
    extract = tempDirWatcher_Data
    lookup (TempDirKey p) w0 = do
        w <- lookup (PackageKey p) w0
        packageWatcher_Children w
    watcherKey = TempDirKey . tempDirWatcher_Package

    updateAll f w = f w >>= \case
        TempDirWatcher cs cat d -> do
            cs' <- traverse f cs
            pure $ TempDirWatcher cs' cat d

instance BasicLayer 'LogFileLayer where
    watcherType _ = WatchFile
    extract = logFileWatcher_Data
    toList = (:[]) . extract
    lookup (LogFileKey p) w0 = do
        w <- lookup (TempDirKey p) w0
        tempDirWatcher_Children w
    watcherKey = LogFileKey . logFileWatcher_Package

    updateAll f w = f w -- no children

data WatcherData t = WatcherData
    { getWatch :: Inotify.Watch
    , getEvent :: Event t MyEvent
    }

data Watcher (l :: WatchLayer) t a where
    RootWatcher ::
        { rootWatcher_Children :: HashMap Category (Watcher 'CategoryLayer t a)
        , rootWatcher_Path :: RealPath
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
deriving instance BasicLayer l => Traversable (Watcher l t)

-- TODO: Is this necessary or does the derived Foldable instance work as
--       intended? Set up a test case to see if the derived Foldable matches
--       this.
instance BasicLayer l => Foldable (Watcher l t) where
    foldMap f = foldMap f . toList

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
