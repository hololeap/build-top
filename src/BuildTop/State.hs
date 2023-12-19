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
import Data.Hashable
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.Maybe (isJust)
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.These
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

-- | A function to fire an Event and a filter to decide which events get fired
type WatchMap = HashMap Inotify.Watch
    ( MyEvent -> IO ()
    , Inotify.Event -> Maybe BuildTopEvent
    )

type BuildTopState t = (Watcher 'RootLayer t (WatcherData t), WatchMap)


-- | Scan the portage temp directory, building the current @BuildTopState@.
--   Will return @Nothing@ if the portage temp directory does not exist.
scanState :: forall t m0. (Reflex t, TriggerEvent t m0, MonadIO m0)
    => Inotify.Inotify
    -> FilePath
    -> Maybe (BuildTopState t)
    -> m0 (Maybe (BuildTopState t))
scanState inot path0 oldState = finish $ flip runStateT M.empty $ runMaybeT $ do
    let proxy0 = Proxy @'RootLayer
        key0 = RootKey

    -- Create the watcher for the root temp portage directory
    data0 <- watcherHelper key0 () path0

    -- Gather a list of categories with existing directories
    cs :: [Category] <- scanDirectory proxy0 () path0

    child0 <- buildMapFromKeys cs $ \c -> do
        let proxy1 = Proxy @'CategoryLayer
            key1 = CategoryKey c

        -- Create the watcher for the category directory
        let path1 = path0 </> toString c
        data1 <- watcherHelper key1 c path1

        -- Gather a set of packages with existing lockfiles and a
        -- list of packages with existing directories
        (ls,ps) <- scanDirectory proxy1 c path1

        -- Create the HashMap containing LockFilePresents and PackageWatches
        child1 <- buildMapFromKeys ps $ \p -> do
            let proxy2 = Proxy @'PackageLayer
                key2 = PackageKey p

            -- Create the watcher for the package dir
            let path2 = path0 </> toString p
            data2 <- watcherHelper key2 p path2

            -- does the lock file exist?
            let lockFilePresent = p `S.member` ls

            -- does the temp dir exist?
            mt <- scanDirectory proxy2 () path2
            child2 <- forM mt $ \path3 -> do
                let proxy3 = Proxy @'TempDirLayer
                    key3 = TempDirKey p

                -- Create the watcher for the temp dir
                data3 <- watcherHelper key3 p path3

                -- does the log file exist?
                ml <- scanDirectory proxy3 () path3
                child3 <- forM ml $ \path4 -> do
                    -- Create the watcher for the build log file
                    data4 <- watcherHelper (LogFileKey p) p path4

                    pure $ LogFileWatcher p data4

                pure $ TempDirWatcher child3 p data3

            pure $ (lockFilePresent, PackageWatcher child2 p data2)

        pure $ CategoryWatcher child1 c data1

    let rw = RootWatcher child0 path0 data0
    cleanupOldState rw
    pure rw
  where
    watcherHelper
        :: forall l m.
           ( IsWatcher l
           , HasFilter l
           , Reflex t
           , TriggerEvent t m
           , MonadIO m
           , MonadState WatchMap m
           )
        => WatcherKey l
        -> FilterInput l
        -> FilePath
        -> MaybeT m (WatcherData t)
    watcherHelper key filtIn path = do
        let proxy = Proxy @l
            check = case watcherType proxy of
                WatchDirectory -> doesDirectoryExist
                WatchFile -> doesFileExist
        liftIO (check path) >>= guard

        (iWatch, rEvent, eAct) <- recycleState <|> createState proxy
        modify $ M.insert iWatch (eAct, fst $ layerFilter proxy filtIn)

        pTraceMForceColor $ unwords ["watcher created for", path, "(" ++ show iWatch ++ ")"]

        pure $ WatcherData iWatch rEvent
      where
        recycleState
            :: MaybeT m (Inotify.Watch, Event t MyEvent, MyEvent -> IO ())
        recycleState = do
            (oldWatcher, oldWatchMap) <- liftMaybe oldState
            w <- liftMaybe $ lookupWatcher key oldWatcher
            let (WatcherData iWatch rEvent) = getWatcher w
            case M.lookup iWatch oldWatchMap of
                Just (eAct, _) -> pure (iWatch, rEvent, eAct)
                Nothing -> liftIO
                    $ error "iWatch found but it's not in oldWatchMap!"

        createState
            :: Proxy l
            -> MaybeT m (Inotify.Watch, Event t MyEvent, MyEvent -> IO ())
        createState proxy = initWatcher proxy filtIn inot path

    buildMapFromKeys
        :: forall m k v. (Monad m, Hashable k)
        => [k] -> (k -> MaybeT m v) -> m (M.HashMap k v)
    buildMapFromKeys ks f =
        let mkTuple k = runMaybeT $ (k,) <$> f k
        in  M.fromList . catMaybes <$> traverse mkTuple ks

    finish :: Functor f => f (Maybe a, b) -> f (Maybe (a, b))
    finish = fmap $ \(ma, b) -> (,b) <$> ma

    cleanupOldState :: forall m. MonadIO m
        => Watcher 'RootLayer t (WatcherData t) -> m ()
    cleanupOldState newWatcher
        = forM_ oldState $ \(oldWatcher, _) -> do
            filterWatcher go oldWatcher
      where
        go :: forall x. IsWatcher x => Watcher x t (WatcherData t) -> m Bool
        go w = do
            -- If the current node doesn't exist in the new tree, we run a
            -- cleanup action.
            unless (isJust (lookupWatcher (watcherKey w) newWatcher)) $
                liftIO $ Inotify.rmWatch inot $ getWatch $ getWatcher w
            -- We use filterWatcher to traverse the old tree, but nothing
            -- actually needs to get deleted.
            pure True

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
    when (watcherType proxy == WatchDirectory) $ liftIO $ do
        let (_, checkFile) = layerFilter proxy filtIn
        fs <- lenientListDirectory p
        es <- witherM checkFile fs
        mapM_ eIO (That <$> es)

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
