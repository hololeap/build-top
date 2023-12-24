{-# Language DataKinds #-}
{-# Language TypeFamilies #-}

module BuildTop.Filesystem where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Either (partitionEithers)
import Data.Kind
import Data.Maybe (listToMaybe)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import System.Directory
import System.FilePath
import Witherable

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types

import BuildTop.Types
import BuildTop.Util

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

-- | If an IO error is thrown, do not raise the error but instead return an
--   empty list.
lenientListDirectory :: MonadIO m => FilePath -> m [FilePath]
lenientListDirectory fp = liftIO $ listDirectory fp <|> pure []
