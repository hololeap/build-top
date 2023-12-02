{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language TypeFamilies #-}

module BuildTop.Types where

import Data.HashMap.Strict (HashMap)
import Data.Kind
import qualified System.Linux.Inotify as Inotify
import Reflex

import Distribution.Portage.Types

-- | Useful for debugging, may be replaced with BuildTopEvent in the future
type MyEvent = (Inotify.Event, Maybe BuildTopEvent)

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
    type ChildLayer l :: WatchLayer
    getChildren :: Watcher l t -> ChildContainer l (Watcher (ChildLayer l) t)

instance HasChildren 'RootLayer where
    type ChildContainer 'RootLayer = HashMap Category
    type ChildLayer 'RootLayer = 'CategoryLayer
    getChildren = rootWatcher_Children

instance HasChildren 'CategoryLayer where
    type ChildContainer 'CategoryLayer = HashMap Package
    type ChildLayer 'CategoryLayer = 'PackageLayer
    getChildren = fmap snd . categoryWatcher_Children

instance HasChildren 'PackageLayer where
    type ChildContainer 'PackageLayer = Maybe
    type ChildLayer 'PackageLayer = 'TempDirLayer
    getChildren = packageWatcher_Children

instance HasChildren 'TempDirLayer where
    type ChildContainer 'TempDirLayer = Maybe
    type ChildLayer 'TempDirLayer = 'LogFileLayer
    getChildren = tempDirWatcher_Children

class IsWatcher (l :: WatchLayer) where
    watcherType :: proxy l -> WatchType
    default watcherType :: HasChildren l => proxy l -> WatchType
    watcherType _ = WatchDirectory

    -- | Return the @a@ from the data structure
    getEvent :: Watcher l t -> Event t MyEvent

    -- | Return the @a@ from the data structure as well as all @a@s from the
    --   child tree
    getEvents :: Watcher l t -> [Event t MyEvent]
    default getEvents
        :: (HasChildren l, IsWatcher (ChildLayer l), Foldable (ChildContainer l))
        => Watcher l t -> [Event t MyEvent]
    getEvents w = getEvent w : foldMap getEvents (getChildren w)

    -- | Return the 'Inotify.Watch' from the data structure
    getWatch :: Watcher l t -> Inotify.Watch

    -- | Return the 'Inotify.Watch' from the data structure as well as all
    --   'Inotify.Watch' instances from the child tree
    getWatches :: Watcher l t -> [Inotify.Watch]
    default getWatches
        :: (HasChildren l, IsWatcher (ChildLayer l), Foldable (ChildContainer l))
        => Watcher l t -> [Inotify.Watch]
    getWatches w = getWatch w : foldMap getWatches (getChildren w)

instance IsWatcher 'RootLayer where
    getEvent = rootWatcher_Event
    getWatch = rootWatcher_Watch

instance IsWatcher 'CategoryLayer where
    getEvent = categoryWatcher_Event
    getWatch = categoryWatcher_Watch

instance IsWatcher 'PackageLayer where
    getEvent = packageWatcher_Event
    getWatch = packageWatcher_Watch

instance IsWatcher 'TempDirLayer where
    getEvent = tempDirWatcher_Event
    getWatch = tempDirWatcher_Watch

instance IsWatcher 'LogFileLayer where
    watcherType _ = WatchFile
    getEvent = logFileWatcher_Event
    getEvents = (:[]) . getEvent
    getWatch = logFileWatcher_Watch
    getWatches = (:[]) . getWatch

data Watcher (l :: WatchLayer) t where
    RootWatcher ::
        { rootWatcher_Children :: HashMap Category (Watcher 'CategoryLayer t)
        , rootWatcher_Path :: FilePath
        , rootWatcher_Watch :: Inotify.Watch
        , rootWatcher_Event :: Event t MyEvent
        } -> Watcher 'RootLayer t
    CategoryWatcher ::
        { categoryWatcher_Children :: HashMap Package (LockFileExists, Watcher 'PackageLayer t)
        , categoryWatcher_Category :: Category
        , categoryWatcher_Watch :: Inotify.Watch
        , categoryWatcher_Event :: Event t MyEvent
        } -> Watcher 'CategoryLayer t
    PackageWatcher ::
        { packageWatcher_Children :: Maybe (Watcher 'TempDirLayer t)
        , packageWatcher_Package :: Package
        , packageWatcher_Watch :: Inotify.Watch
        , packageWatcher_Event :: Event t MyEvent
        } -> Watcher 'PackageLayer t
    TempDirWatcher ::
        { tempDirWatcher_Children :: Maybe (Watcher 'LogFileLayer t)
        , tempDirWatcher_Package :: Package
        , tempDirWatcher_Watch :: Inotify.Watch
        , tempDirWatcher_Event :: Event t MyEvent
        } -> Watcher 'TempDirLayer t
    LogFileWatcher ::
        { logFileWatcher_Package :: Package
        , logFileWatcher_Watch :: Inotify.Watch
        , logFileWatcher_Event :: Event t MyEvent
        } -> Watcher 'LogFileLayer t

-- | A function to fire an Event and a filter to decide which events get fired
type WatchMap = HashMap Inotify.Watch
    ( MyEvent -> IO ()
    , Inotify.Event -> Maybe BuildTopEvent
    )

type BuildTopState t = (Watcher 'RootLayer t, WatchMap)

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
