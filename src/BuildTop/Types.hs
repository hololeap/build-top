{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeFamilies #-}

module BuildTop.Types where

import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Kind
import Data.These
import qualified System.Linux.Inotify as Inotify
import Reflex

import Distribution.Portage.Types

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
    deleteChild :: MonadIO m
        => (a -> m ()) -- ^ cleanup function to run on data
        -> ChildContainerKey l
        -> Watcher l t a
        -> m (Watcher l t a)

instance HasChildren 'RootLayer where
    type ChildContainer 'RootLayer = HashMap Category
    type ChildContainerKey 'RootLayer = Category
    type ChildLayer 'RootLayer = 'CategoryLayer
    getChildren = rootWatcher_Children
    deleteChild f k w0 = do
        c <- M.alterF go k (rootWatcher_Children w0)
        pure w0 { rootWatcher_Children = c }
      where
        go = deleteHelper f $ \w -> do
            let ps = M.keys $ snd <$> categoryWatcher_Children w
            mapM_ (\p -> deleteChild f p w) ps

instance HasChildren 'CategoryLayer where
    type ChildContainer 'CategoryLayer = HashMap Package
    type ChildContainerKey 'CategoryLayer = Package
    type ChildLayer 'CategoryLayer = 'PackageLayer
    getChildren = fmap snd . categoryWatcher_Children

    deleteChild f k w0 = do
        let go = deleteHelper f (deleteChild f ()) . fmap snd
        c <- M.alterF go k (categoryWatcher_Children w0)
        pure w0 { categoryWatcher_Children = c }

instance HasChildren 'PackageLayer where
    type ChildContainer 'PackageLayer = Maybe
    type ChildContainerKey 'PackageLayer = ()
    type ChildLayer 'PackageLayer = 'TempDirLayer
    getChildren = packageWatcher_Children
    deleteChild f _ w0 = do
        c <- deleteHelper f (deleteChild f ()) (getChildren w0)
        pure w0 { packageWatcher_Children = c }

instance HasChildren 'TempDirLayer where
    type ChildContainer 'TempDirLayer = Maybe
    type ChildContainerKey 'TempDirLayer = ()
    type ChildLayer 'TempDirLayer = 'LogFileLayer
    getChildren = tempDirWatcher_Children
    deleteChild f _ w0 = do
        c <- deleteHelper f (\_ -> pure ()) (getChildren w0)
        pure w0 { tempDirWatcher_Children = c }

deleteHelper :: (IsWatcher l, Monad m)
    => (a -> m ())
    -> (Watcher l t a -> m b)
    -> (Maybe (Watcher l t a))
    -> m (Maybe c)
deleteHelper f1 f2 m = Nothing <$ mapM_ go m
    where go w = f1 (getWatcher w) *> f2 w

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

instance IsWatcher 'RootLayer where
    getWatcher = rootWatcher_Data
    lookupWatcher _ w0 = Just w0

instance IsWatcher 'CategoryLayer where
    getWatcher = categoryWatcher_Data
    lookupWatcher (CategoryKey c) w0 =
        M.lookup c (rootWatcher_Children w0)

instance IsWatcher 'PackageLayer where
    getWatcher = packageWatcher_Data
    lookupWatcher (PackageKey p) w0 = do
        w <- lookupWatcher (CategoryKey (getCategory p)) w0
        snd <$> M.lookup p (categoryWatcher_Children w)

instance IsWatcher 'TempDirLayer where
    getWatcher = tempDirWatcher_Data
    lookupWatcher (TempDirKey p) w0 = do
        w <- lookupWatcher (PackageKey p) w0
        packageWatcher_Children w

instance IsWatcher 'LogFileLayer where
    watcherType _ = WatchFile
    getWatcher = logFileWatcher_Data
    getWatchers = (:[]) . getWatcher
    lookupWatcher (LogFileKey p) w0 = do
        w <- lookupWatcher (TempDirKey p) w0
        tempDirWatcher_Children w

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
        { logFileWatcherWatcher_Package :: Package
        , logFileWatcher_Data :: a
        } -> Watcher 'LogFileLayer t a

deriving instance Functor (Watcher l t)
deriving instance Foldable (Watcher l t)
deriving instance Traversable (Watcher l t)

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

-- | A function to fire an Event and a filter to decide which events get fired
type WatchMap = HashMap Inotify.Watch
    ( MyEvent -> IO ()
    , Inotify.Event -> Maybe BuildTopEvent
    )

type BuildTopState t = (Watcher 'RootLayer t (WatcherData t), WatchMap)

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
