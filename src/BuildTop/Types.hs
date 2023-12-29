
module BuildTop.Types where

import Distribution.Portage.Types

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
