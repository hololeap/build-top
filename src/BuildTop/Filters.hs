{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeFamilies #-}

module BuildTop.Filters where

import System.Linux.Inotify

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Parsable
import Distribution.Portage.Types
import System.Directory
import System.FilePath

import BuildTop.Types
import BuildTop.Util

-- | A rule list to potentially convert an 'Event' to a 'BuildTopEvent'.
type InotifyFilter = Event -> Maybe BuildTopEvent

-- | A rule list to potentially convert a 'FilePath' to a 'BuildTopEvent'. The
--   result is wrapped in the 'IO' monad in case an IO check needs to be made,
--   for instance a supplementary existance check.
--
--   Note that the 'FilePath' is not absolute, but a file or directory name
--   e.g. directly from 'listDirectory'.
type FilePathFilter = FilePath -> IO (Maybe BuildTopEvent)

-- | Trigger a 'BuildTopEvent'. This needs to be possible with both with an
--   'Inotify.Event' as an input, and with a basic 'FilePath' (the name of a
--   newly discoverered file/directory) as an input.
data EventFilter = EventFilter
    { inotifyFilter :: InotifyFilter
    , filePathFilter :: FilePathFilter
    }

class HasFilter (l :: WatchLayer) where
    type FilterInput l
    layerMask :: proxy l -> Mask WatchFlag
    layerFilter :: proxy l -> FilterInput l -> EventFilter

instance HasFilter 'RootLayer where
    type FilterInput 'RootLayer = ()
    layerMask _ = in_CREATE <> in_DELETE
    layerFilter _ _ = EventFilter
        ( \(Event _ m _ n) -> case
            ( isSubset in_ISDIR m
            , isSubset in_CREATE m
            , isSubset in_DELETE m
            , parseMaybe (decodeString n)
            ) of
                (True, True, _, Just c) -> Just $ CategoryEvent AddEvent c
                (True, _, True, Just c) -> Just $ CategoryEvent RemoveEvent c
                _ -> Nothing
        )
        ( \fp -> runMaybeT $ do
            c <- MaybeT $ pure $ parseMaybe fp
            liftIO (doesDirectoryExist fp) >>= guard
            pure $ CategoryEvent AddEvent c
        )

instance HasFilter 'CategoryLayer where
    type FilterInput 'CategoryLayer = Category
    layerMask _ = in_CREATE <> in_DELETE
    layerFilter _ c = EventFilter
        ( \(Event _ m _ n) -> case
            ( isSubset in_ISDIR m
            , isSubset in_CREATE m
            , isSubset in_DELETE m
            , parseMaybe $ fullS $ decodeString n
            , lockFilePackage c (decodeString n)
            ) of
                (True, True, _, Just p, _) -> Just $ PackageEvent AddEvent p
                (True, _, True, Just p, _) -> Just $ PackageEvent RemoveEvent p
                (False, True, _, _, Just p) -> Just $ LockFileEvent AddEvent p
                (False, _, True, _, Just p) -> Just $ LockFileEvent RemoveEvent p
                _ -> Nothing
        )
        ( \fp -> runMaybeT $ do
            isD <- liftIO $ doesDirectoryExist fp
            isF <- liftIO $ doesFileExist fp
            case
                ( isD
                , isF
                , parseMaybe $ fullS fp
                , lockFilePackage c fp
                ) of
                    (True, _, Just p, _) -> pure $ PackageEvent AddEvent p
                    (_, True, _, Just p) -> pure $ LockFileEvent AddEvent p
                    _ -> mzero
        )
      where
        fullS = (toString c </>)

instance HasFilter 'PackageLayer where
    type FilterInput 'PackageLayer = Package
    layerMask _ = in_CREATE <> in_DELETE
    layerFilter _ p = EventFilter
        ( \(Event _ m _ n) -> case
            ( decodeString n == "temp"
            , isSubset in_ISDIR m
            , isSubset in_CREATE m
            , isSubset in_DELETE m
            ) of
                (True, True, True, _) -> Just $ TempDirEvent AddEvent p
                (True, True, _, True) -> Just $ TempDirEvent RemoveEvent p
                _ -> Nothing
        )
        ( \fp -> runMaybeT $ do
            guard $ fp == "temp"
            liftIO (doesDirectoryExist fp) >>= guard
            pure $ TempDirEvent AddEvent p
        )

instance HasFilter 'TempDirLayer where
    type FilterInput 'TempDirLayer = Package
    layerMask _ = in_CREATE <> in_DELETE
    layerFilter _ p = EventFilter
        ( \(Event _ m _ n) -> case
            ( decodeString n == "build.log"
                , isSubset in_ISDIR m
                , isSubset in_CREATE m
                , isSubset in_DELETE m
                ) of
                    (True, False, True, _) -> Just $ LogFileEvent AddEvent p
                    (True, False, _, True) -> Just $ LogFileEvent RemoveEvent p
                    _ -> Nothing
        )
        ( \fp -> runMaybeT $ do
            guard $ fp == "build.log"
            liftIO (doesFileExist fp) >>= guard
            pure $ LogFileEvent AddEvent p
        )

instance HasFilter 'LogFileLayer where
    type FilterInput 'LogFileLayer = Package
    layerMask _ = in_CLOSE_WRITE <> in_MODIFY <> in_ACCESS
    layerFilter _ p = EventFilter
          -- The mask _should_ only deliver relevant events
        ( \_ -> Just $ LogFileWriteEvent p
        )
        ( \_ -> pure Nothing -- Nothing can be done with just a "build.log" string
        )
