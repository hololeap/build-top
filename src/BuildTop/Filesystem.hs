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

-- | Wrapper that "proves" a path is an absolute path, not a relative one.
newtype RealPath = RealPath FilePath
    deriving (Show, Eq, Ord)

append :: RealPath -> FilePath -> RealPath
append (RealPath p1) p2 = RealPath $ p1 </> p2

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
        -> [RealPath] -- ^ real path, not just the base name
        -> m (ScanData l)

instance HasDirectory 'RootLayer where
    type ScanData 'RootLayer = [Category]
    processDir _ _ = witherM $ \(RealPath f) -> runMaybeT $ do
        c <- liftMaybe $ parseMaybe $ takeFileName f
        liftIO (doesDirectoryExist f) >>= guard
        pure c

-- | Scan a list of FilePaths, sorting them into a set of lockfiles and a
--   list of packages (directories).
instance HasDirectory 'CategoryLayer where
    type ScanInput 'CategoryLayer = Category
    type ScanData 'CategoryLayer = (Set Package, [Package])
    processDir _ c fs = do
        let checkLockFile (RealPath f) = do
                p <- liftMaybe $ lockFilePackage c $ takeFileName f
                liftIO (doesFileExist f) >>= guard
                pure p
            checkPkgDir (RealPath f) = do
                p <- liftMaybe $ parseMaybe $ toString c </> takeFileName f
                liftIO (doesDirectoryExist f) >>= guard
                pure p
            identifyPath f = runMaybeT $
                    Left <$> checkLockFile f
                <|> Right <$> checkPkgDir f
        (lockPs, pkgPs) <- partitionEithers <$> witherM identifyPath fs
        pure (S.fromList lockPs, pkgPs)

instance HasDirectory 'PackageLayer where
    type ScanData 'PackageLayer = Maybe RealPath
    processDir _ _ =
        let go = witherM $ \rp@(RealPath f) -> runMaybeT $ do
                guard (takeFileName f == "temp")
                liftIO (doesDirectoryExist f) >>= guard
                pure rp
        in fmap listToMaybe . go

instance HasDirectory 'TempDirLayer where
    type ScanData 'TempDirLayer = Maybe RealPath
    processDir _ _ =
        let go = witherM $ \rp@(RealPath f) -> runMaybeT $ do
                guard (takeFileName f == "build.log")
                liftIO (doesFileExist f) >>= guard
                pure rp
        in fmap listToMaybe . go

-- | Scan a directory given a layer with a @HasDirectory@ instance, its
--   input, and a path to the given directory. Sends the full path of the file
--   to @processDir@.
scanDirectory
    :: (HasDirectory l, MonadIO m)
    => Proxy l
    -> ScanInput l
    -> RealPath -- ^ Directory to scan
    -> m (ScanData l)
scanDirectory proxy i p0 = do
    fs <- lenientListDirectory p0
    processDir proxy i (map (append p0) fs)

-- | If an IO error is thrown, do not raise the error but instead return an
--   empty list.
lenientListDirectory :: MonadIO m => RealPath -> m [FilePath]
lenientListDirectory (RealPath fp) = liftIO $ listDirectory fp <|> pure []
