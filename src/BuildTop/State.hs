{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language KindSignatures #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# Language TupleSections #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}

module BuildTop.State where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Either (partitionEithers)
import Data.Hashable
import qualified Data.HashMap.Strict as M
import Data.Kind
import Data.Maybe (listToMaybe)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Reflex
import System.Directory
import System.FilePath
import qualified System.Linux.Inotify as Inotify
import Witherable

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types

import BuildTop.Filters
import BuildTop.Types
import BuildTop.Util

import Debug.Pretty.Simple

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

-- | Scan the portage temp directory, building the current @BuildTopState@.
--   Will return @Nothing@ if the portage temp directory does not exist.
scanState :: (Reflex t, TriggerEvent t m, MonadIO m)
    => Inotify.Inotify
    -> FilePath
    -> m (Maybe (BuildTopState t))
scanState inot path0 = finish $ flip runStateT M.empty $ runMaybeT $ do
    let proxy0 = Proxy @'RootLayer

    -- Create the watcher for the root temp portage directory
    data0 <- watcherHelper proxy0 () path0

    -- Gather a list of categories with existing directories
    cs :: [Category] <- scanDirectory proxy0 () path0

    child0 <- buildMapFromKeys cs $ \c -> do
        let proxy1 = Proxy @'CategoryLayer

        -- Create the watcher for the category directory
        let path1 = path0 </> toString c
        data1 <- watcherHelper proxy1 c path1

        -- Gather a set of packages with existing lockfiles and a
        -- list of packages with existing directories
        (ls,ps) <- scanDirectory proxy1 c path1

        -- Create the HashMap containing LockFilePresents and PackageWatches
        child1 <- buildMapFromKeys ps $ \p -> do
            let proxy2 = Proxy @'PackageLayer

            -- Create the watcher for the package dir
            let path2 = path0 </> toString p
            data2 <- watcherHelper proxy2 p path2

            -- does the lock file exist?
            let lockFilePresent = p `S.member` ls

            -- does the temp dir exist?
            mt <- scanDirectory proxy2 () path2
            child2 <- forM mt $ \path3 -> do
                let proxy3 = Proxy @'TempDirLayer

                -- Create the watcher for the temp dir
                data3 <- watcherHelper proxy3 p path3

                -- does the log file exist?
                ml <- scanDirectory proxy3 () path3
                child3 <- forM ml $ \path4 -> do
                    -- Create the watcher for the build log file
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

    finish :: Functor f => f (Maybe a, b) -> f (Maybe (a, b))
    finish = fmap $ \(ma, b) -> (,b) <$> ma

initWatcher :: (IsWatcher l, HasFilter l, Reflex t, TriggerEvent t m, MonadIO m)
    => Proxy l
    -> FilterInput l
    -> Inotify.Inotify
    -> FilePath
    -> MaybeT m (Inotify.Watch, Event t MyEvent, MyEvent -> IO ())
initWatcher proxy filtIn i p = do
    let check = case watcherType proxy of
            WatchDirectory -> doesDirectoryExist
            WatchFile -> doesFileExist
    liftIO (check p) >>= guard
    pTraceMForceColor $ "directory " ++ p ++ " exists"
    let m = layerMask proxy
    w <- liftIO $ Inotify.addWatch i p (layerMask proxy)
    pTraceMForceColor $ unwords ["Added watcher to inotify:", show i, show p, show m]
    (e, eIO) <- newTriggerEvent

    -- Race condition: interesting contents may have been created in the
    -- directory _after_ the 'Inotify.Watch' was initialized. Scan the
    -- directory now and fire off events for any interesting contents we find.
    -- NOTE: This may create duplicate events if the 'Inotify.Watch' also
    -- catches the interesting content. This must be handled by the
    -- state-modifying code.



    pure (w, e, eIO)

-- When certain events fire, we need to update the state of the Watcher
updateWatchState
    :: Inotify.Inotify
    -> Inotify.Event
    -> Watcher 'RootLayer t (WatcherData t)
    -> Watcher 'RootLayer t (WatcherData t)
updateWatchState i e rw = case
    ( check Inotify.in_ISDIR
    , check Inotify.in_CREATE
    , check Inotify.in_DELETE
    , check Inotify.in_CLOSE_WRITE
    ) of
        -- A directory was created
        (True, True, _, _) ->
            let eventPath = T.unpack $ T.decodeUtf8 $ Inotify.name e
            in undefined
        _ -> undefined
  where
    check = Inotify.isSubset (Inotify.mask e)
