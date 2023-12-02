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

class HasFilter (l :: WatchLayer) where
    type FilterInput l
    layerMask :: proxy l -> Mask WatchFlag
    layerFilter :: proxy l -> FilterInput l -> EventFilter

instance HasFilter 'RootLayer where
    type FilterInput 'RootLayer = ()
    layerMask _ = in_CREATE <> in_DELETE
    layerFilter _ _ =
        ( \(Event _ m _ n) -> case
            ( isSubset in_ISDIR m
            , isSubset in_CREATE m
            , isSubset in_DELETE m
            , parseMaybe (decodeString n)
            ) of
                (True, True, _, Just c) -> Just $ CategoryEvent AddEvent c
                (True, _, True, Just c) -> Just $ CategoryEvent RemoveEvent c
                _ -> Nothing
        , \fp -> runMaybeT $ do
            c <- MaybeT $ pure $ parseMaybe fp
            liftIO (doesDirectoryExist fp) >>= guard
            pure $ CategoryEvent AddEvent c
        )

instance HasFilter 'CategoryLayer where
    type FilterInput 'CategoryLayer = Category
    layerMask _ = in_CREATE <> in_DELETE
    layerFilter _ c =
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
        , \fp -> runMaybeT $ do
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
    layerFilter _ p =
        ( \(Event _ m _ n) -> case
            ( decodeString n == "temp"
            , isSubset in_ISDIR m
            , isSubset in_CREATE m
            , isSubset in_DELETE m
            ) of
                (True, True, True, _) -> Just $ TempDirEvent AddEvent p
                (True, True, _, True) -> Just $ TempDirEvent RemoveEvent p
                _ -> Nothing
        , \fp -> runMaybeT $ do
            guard $ fp == "temp"
            liftIO (doesDirectoryExist fp) >>= guard
            pure $ TempDirEvent AddEvent p
        )

instance HasFilter 'TempDirLayer where
    type FilterInput 'TempDirLayer = Package
    layerMask _ = in_CREATE <> in_DELETE
    layerFilter _ p =
        ( \(Event _ m _ n) -> case
            ( decodeString n == "build.log"
                , isSubset in_ISDIR m
                , isSubset in_CREATE m
                , isSubset in_DELETE m
                ) of
                    (True, False, True, _) -> Just $ LogFileEvent AddEvent p
                    (True, False, _, True) -> Just $ LogFileEvent RemoveEvent p
                    _ -> Nothing
        , \fp -> runMaybeT $ do
            guard $ fp == "build.log"
            liftIO (doesFileExist fp) >>= guard
            pure $ LogFileEvent AddEvent p
        )

instance HasFilter 'LogFileLayer where
    type FilterInput 'LogFileLayer = Package
    layerMask _ = in_CLOSE_WRITE
    layerFilter _ p =
        ( \(Event _ m _ _) -> case
            ( isSubset in_CLOSE_WRITE m
            ) of
                (True) -> Just $ LogFileWriteEvent p
                _ -> Nothing
        , \_ -> pure Nothing -- Nothing can be done with just a "build.log" string
        )
