-- {-# Language ApplicativeDo #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module BuildTop.State where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Bifunctor (first)
import Data.Bitraversable
import Data.Either (partitionEithers)
import Data.Foldable
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

scanState :: (Reflex t, TriggerEvent t m, MonadIO m)
    => Inotify.Inotify
    -> FilePath
    -> m (Maybe (BuildTopState t))
scanState inot path0 = finish $ flip runStateT M.empty $ runMaybeT $ do
    -- Create the watcher for the root temp portage directory
    (iWatch0, rEvent0) <-
        watcherHelper
            (Proxy @'RootLayer)
            ()
            path0

    -- Gather a list of categories with existing directories
    cs :: [Category] <- mapMaybe parseMaybe
        <$> lenientListDirectory path0

    child0 <- fmap (M.fromList . zip cs . catMaybes) $ forM cs $ \c -> runMaybeT $ do
        -- Create the watcher for the category directory
        let path1 = path0 </> toString c
        (iWatch1, rEvent1) <-
            watcherHelper
                (Proxy @'CategoryLayer)
                c
                path1

        -- Gather a set of packages with existing lockfiles and a
        -- list of packages with existing directories
        fs1 <- lenientListDirectory path1
        let (ls, ps) :: (Set Package, [Package]) =
                first mconcat $ partitionEithers $ mapMaybe
                    (\f ->
                        (Left . S.singleton <$> lockFilePackage c f)
                        <|> (Right <$> parseMaybe (toString c </> f))
                    )
                    fs1

        -- Create the HashMap containing LockFilePresents and PackageWatches
        child1 <- fmap (M.fromList . zip ps . catMaybes) $ forM ps $ \p -> runMaybeT $ do
            -- Create the watcher for the package dir
            let path2 = path0 </> toString p
            (iWatch2, rEvent2) <-
                watcherHelper
                    (Proxy @'PackageLayer)
                    p
                    path2

            -- does the lock file exist?
            let lockFilePresent = p `S.member` ls

            -- does the temp dir exist?
            fs2 <- lenientListDirectory path2
            child2 <- forM (find (== "temp") fs2) $ \t -> do
                let path3 = path2 </> t
                (iWatch3, rEvent3) <-
                    watcherHelper
                        (Proxy @'TempDirLayer)
                        p
                        path3

                -- does the log file exist?
                fs3 <- lenientListDirectory path3
                child3 <- forM (find (== "build.log") fs3) $ \l -> do
                    (iWatch4, rEvent4) <-
                        watcherHelper
                            (Proxy @'LogFileLayer)
                            p
                            (path3 </> l)

                    pure $ LogFileWatcher p iWatch4 rEvent4

                pure $ TempDirWatcher child3 p iWatch3 rEvent3

            pure $ (lockFilePresent, PackageWatcher child2 p iWatch2 rEvent2)

        pure $ CategoryWatcher child1 c iWatch1 rEvent1

    pure $ RootWatcher child0 path0 iWatch0 rEvent0
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
        -> MaybeT m (Inotify.Watch, Event t MyEvent)
    watcherHelper proxy filtIn path = do
        let check = case watcherType proxy of
                WatchDirectory -> doesDirectoryExist
                WatchFile -> doesFileExist
        liftIO (check path) >>= guard

        (iWatch, rEvent, eAct) <- initWatcher proxy filtIn inot path
        modify $ M.insert iWatch (eAct, fst $ layerFilter proxy filtIn)

        pTraceMForceColor $ unwords ["watcher created for", path, "(" ++ show iWatch ++ ")"]

        pure (iWatch, rEvent)

    finish :: Functor f => f (Maybe a, b) -> f (Maybe (a, b))
    finish = fmap (bitraverse id Just)

lenientListDirectory :: MonadIO m => FilePath -> m [FilePath]
lenientListDirectory fp = liftIO $ listDirectory fp <|> pure []
