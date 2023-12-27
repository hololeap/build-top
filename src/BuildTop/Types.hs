{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language MultiParamTypeClasses #-}

module BuildTop.Types where

import Control.Monad.Reader
import Data.These
import qualified System.Linux.Inotify as Inotify

import Distribution.Portage.Types

-- | Useful for debugging, may be replaced with BuildTopEvent in the future
type MyEvent = These Inotify.Event BuildTopEvent

-- | An action to fire a reflex @Event@.
type FireReflexEvent = MyEvent -> IO ()

-- | Carries the top-level 'Inotify.Inotify' and an action to fire the
--   top-level reflex @Event@.
type MonadBuildTop = MonadReader (Inotify.Inotify, FireReflexEvent)

askInotify :: MonadBuildTop m => m Inotify.Inotify
askInotify = asks fst

askFireEvent :: MonadBuildTop m => m (FireReflexEvent)
askFireEvent = asks snd

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
