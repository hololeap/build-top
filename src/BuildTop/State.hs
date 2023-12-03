-- {-# Language ApplicativeDo #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# Language TupleSections #-}
{-# Language TypeApplications #-}

module BuildTop.State where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Either (partitionEithers)
import Data.Foldable
import Data.Hashable
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Reflex
import System.Directory
import System.FilePath
import qualified System.Linux.Inotify as Inotify

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types

import BuildTop
import BuildTop.Filters
import BuildTop.Types
import BuildTop.Util

import Debug.Pretty.Simple

-- | Scan the portage temp directory, building the current @BuildTopState@.
--   Will return @Nothing@ if the portage temp directory does not exist.
scanState :: (Reflex t, TriggerEvent t m, MonadIO m)
    => Inotify.Inotify
    -> FilePath
    -> m (Maybe (BuildTopState t))
scanState inot path0 = finish $ flip runStateT M.empty $ runMaybeT $ do
    -- Create the watcher for the root temp portage directory
    data0 <- watcherHelper (Proxy @'RootLayer) () path0

    -- Gather a list of categories with existing directories
    cs :: [Category] <- mapMaybe parseMaybe
        <$> lenientListDirectory path0

    child0 <- buildMapFromKeys cs $ \c -> do
        -- Create the watcher for the category directory
        let path1 = path0 </> toString c
        data1 <- watcherHelper (Proxy @'CategoryLayer) c path1

        -- Gather a set of packages with existing lockfiles and a
        -- list of packages with existing directories
        (ls,ps) <- scanPackages c <$> lenientListDirectory path1

        -- Create the HashMap containing LockFilePresents and PackageWatches
        child1 <- buildMapFromKeys ps $ \p -> do
            -- Create the watcher for the package dir
            let path2 = path0 </> toString p
            data2 <- watcherHelper (Proxy @'PackageLayer) p path2

            -- does the lock file exist?
            let lockFilePresent = p `S.member` ls

            -- does the temp dir exist?
            fs2 <- lenientListDirectory path2
            let t = "temp"
            child2 <- whenFound t fs2 $ do
                let path3 = path2 </> t
                data3 <- watcherHelper (Proxy @'TempDirLayer) p path3

                -- does the log file exist?
                fs3 <- lenientListDirectory path3
                let l = "build.log"
                child3 <- whenFound l fs3 $ do
                    let path4 = path3 </> l
                    data4 <- watcherHelper (Proxy @'LogFileLayer) p path4
                    pure $ LogFileWatcher p data4

                pure $ TempDirWatcher child3 p data3

            pure $ (lockFilePresent, PackageWatcher child2 p data2)

        pure $ CategoryWatcher child1 c data1

    pure $ RootWatcher child0 path0 data0
  where
    watcherHelper
        :: ( IsWatcher l
           , HasFilter l
           , Reflex t
           , TriggerEvent t m
           , MonadIO m
           , MonadState WatchMap m
           )
        => Proxy l
        -> FilterInput l
        -> FilePath
        -> MaybeT m (WatcherData t)
    watcherHelper proxy filtIn path = do
        let check = case watcherType proxy of
                WatchDirectory -> doesDirectoryExist
                WatchFile -> doesFileExist
        liftIO (check path) >>= guard

        (iWatch, rEvent, eAct) <- initWatcher proxy filtIn inot path
        modify $ M.insert iWatch (eAct, fst $ layerFilter proxy filtIn)

        pTraceMForceColor $ unwords ["watcher created for", path, "(" ++ show iWatch ++ ")"]

        pure $ WatcherData iWatch rEvent

    buildMapFromKeys
        :: forall m k v. (Monad m, Hashable k)
        => [k] -> (k -> MaybeT m v) -> m (M.HashMap k v)
    buildMapFromKeys ks f =
        let mkTuple k = runMaybeT $ (k,) <$> f k
        in  M.fromList . catMaybes <$> traverse mkTuple ks

    whenFound :: (Monad m, Foldable t, Eq a) => a -> t a -> m b -> m (Maybe b)
    whenFound x xs = forM (find (== x) xs) . const

    -- Scan a list of FilePaths, sorting them into a set of lockfiles and a
    -- list of packages (directories).
    scanPackages :: Category -> [FilePath] -> (Set Package, [Package])
    scanPackages c fs =
        let (lockPs, pkgPs) = partitionEithers (mapMaybe identifyPath fs)
        in (S.fromList lockPs, pkgPs)
      where
        identifyPath :: FilePath -> Maybe (Either Package Package)
        identifyPath f
            =   Left <$> lockFilePackage c f
            <|> Right <$> parseMaybe (toString c </> f)

    finish :: Functor f => f (Maybe a, b) -> f (Maybe (a, b))
    finish = fmap $ \(ma, b) -> (,b) <$> ma

-- | If an IO error is thrown, do not raise the error but instead return an
--   empty list.
lenientListDirectory :: MonadIO m => FilePath -> m [FilePath]
lenientListDirectory fp = liftIO $ listDirectory fp <|> pure []
