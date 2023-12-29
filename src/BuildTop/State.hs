{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language KindSignatures #-}
{-# Language LambdaCase #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TupleSections #-}
{-# Language TypeApplications #-}

module BuildTop.State
    ( WatchMap
    , BuildTopState
    , WatchMapData
    , EventSource(..)
    , EventWithSource(..)
    , SomeEvent(..)
    , FireReflexEvent
    , MonadBuildTop
    , askInotify
    , askFireEvent
    , withSomeEvent
    , withWatchMapData
    , scanState
    , updateWatchState
    ) where

import Control.Applicative ((<|>), Alternative, empty)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Data.DList (DList)
import qualified Data.DList as D
import Data.Hashable
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.Maybe (isJust)
import Data.Proxy
import qualified Data.Set as S
import System.Directory
import qualified System.Linux.Inotify as Inotify
import Witherable

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types

import BuildTop.Filesystem
import qualified BuildTop.Filesystem as FS
import BuildTop.Filters
import BuildTop.Types
import BuildTop.Util
import BuildTop.Watcher
    ( Watcher(..)
    , WatcherData(..)
    , BasicLayer
    , WatcherKey(..)
    , InsertPayload(..)
    , AlterPayload(..)
    )
import qualified BuildTop.Watcher as W

import Debug.Pretty.Simple

-- | The inotify events don't carry any state except the 'Inotify.Watch' that
--   they are associated with, so this holds any state associated with any
--   'Inotify.Watch' that are created.
type WatchMap = HashMap Inotify.Watch WatchMapData

type BuildTopState = (Watcher 'RootLayer WatcherData, WatchMap)

-- | A GADT which holds all the data needed whenever a @Inotify.Event@ fires.
--   This hides the 'WatchLayer' type-level information which is carried by
--   'WatcherKey' and 'FilterInput'.
data WatchMapData where
    WatchMapData :: (BasicLayer l, HasFilter l)
        => InotifyFilter -- ^ filter
        -> WatcherKey l  -- ^ key for this node                          -- ^
        -> FilterInput l -- ^ needed input for 'layerFilter'
        -> RealPath      -- ^ path associated with this node
        -> WatchMapData

-- | A tag signifying the source of a 'BuildTopEvent'
data EventSource
    = IgnoredInotify -- ^ An inotify event that has been ignored
    | Inotify        -- ^ An inotify event which triggers a 'BuildTopEvent'
    | InitWatchScan  -- ^ A 'BuildTopEvent' from the @initWatch@ scan
    deriving (Show, Eq, Ord, Bounded, Enum)

-- | An event tagged with its source alongside any relevant environment
data EventWithSource (s :: EventSource) where
    IgnoredInotifyEvent
        :: Inotify.Event
        -> RealPath
        -> EventWithSource 'IgnoredInotify
    InotifyEvent
        :: Inotify.Event -- FireReflexEvent MonadBuildTop askInotify askFireEvent
        -> RealPath
        -> BuildTopEvent
        -> EventWithSource 'Inotify
    InitWatchScanEvent
        :: RealPath
        -> BuildTopEvent
        -> EventWithSource 'InitWatchScan

deriving instance Show (EventWithSource s)
deriving instance Eq (EventWithSource s)

data SomeEvent where
    SomeEvent :: EventWithSource s -> SomeEvent

withSomeEvent
    :: SomeEvent
    -> (forall s. EventWithSource s -> r)
    -> r
withSomeEvent (SomeEvent e) f = f e

-- | An action to fire a reflex @Event@.
type FireReflexEvent = SomeEvent -> IO ()

-- | Carries the top-level 'Inotify.Inotify' and an action to fire the
--   top-level reflex @Event@.
type MonadBuildTop = MonadReader (Inotify.Inotify, FireReflexEvent)

askInotify :: MonadBuildTop m => m Inotify.Inotify
askInotify = asks fst

askFireEvent :: MonadBuildTop m => m (FireReflexEvent)
askFireEvent = asks snd

-- | Run a function using 'WatchMapData'. The function /must not/ return
--   anything that mentions the inner @l@ type.
withWatchMapData
    :: WatchMapData
    -> ( forall l. (BasicLayer l, HasFilter l)
         => (Inotify.Event -> Maybe BuildTopEvent)
         -> WatcherKey l
         -> FilterInput l
         -> RealPath
         -> r
       )
    -> r
withWatchMapData (WatchMapData filt key input path) f
    = f filt key input path

-- | Scan the portage temp directory, building the current @BuildTopState@.
--   Will return @Nothing@ if the portage temp directory does not exist.
scanState :: forall m0.
    ( MonadIO m0
    , MonadBuildTop m0
    )
    => Maybe BuildTopState
    -> RealPath
    -> m0 (Maybe BuildTopState)
scanState s0 path0 = finish $ flip runStateT M.empty $ runMaybeT $ do
    let proxy0 = Proxy @'RootLayer
        key0 = RootKey

    (rw, es) <- runWriterT $ do
        -- Create the watcher for the root temp portage directory
        data0 <- createWatcherData s0 key0 () path0

        -- Gather a list of categories with existing directories
        cs :: [Category] <- scanDirectory proxy0 () path0

        child0 <- buildMapFromKeys cs $ \c -> do
            let proxy1 = Proxy @'CategoryLayer
                key1 = CategoryKey c

            -- Create the watcher for the category directory
            let path1 = path0 `FS.append` toString c
            data1 <- createWatcherData s0 key1 c path1

            -- Gather a set of packages with existing lockfiles and a
            -- list of packages with existing directories
            (ls,ps) <- scanDirectory proxy1 c path1

            -- Create the HashMap containing LockFilePresents and PackageWatches
            child1 <- buildMapFromKeys ps $ \p -> do
                let proxy2 = Proxy @'PackageLayer
                    key2 = PackageKey p

                -- Create the watcher for the package dir
                let path2 = path0 `FS.append` toString p
                data2 <- createWatcherData s0 key2 p path2

                -- does the lock file exist?
                let lockFilePresent = p `S.member` ls

                -- does the temp dir exist?
                mt <- scanDirectory proxy2 () path2
                child2 <- forM mt $ \path3 -> do
                    let proxy3 = Proxy @'TempDirLayer
                        key3 = TempDirKey p

                    -- Create the watcher for the temp dir
                    data3 <- createWatcherData s0 key3 p path3

                    -- does the log file exist?
                    ml <- scanDirectory proxy3 () path3
                    child3 <- forM ml $ \path4 -> do
                        -- Create the watcher for the build log file
                        data4 <- createWatcherData s0 (LogFileKey p) p path4

                        pure $ LogFileWatcher p data4

                    pure $ TempDirWatcher child3 p data3

                pure $ (lockFilePresent, PackageWatcher child2 p data2)

            pure $ CategoryWatcher child1 c data1

        pure $ RootWatcher child0 path0 data0

    wm <- get
    (rw',wm') <- foldM (\s e -> updateWatchState (SomeEvent e) s) (rw,wm) es
    put wm'

    cleanupOldState rw'
    pure rw'
  where
    buildMapFromKeys
        :: forall m k v. (Monad m, Hashable k)
        => [k] -> (k -> MaybeT m v) -> m (M.HashMap k v)
    buildMapFromKeys ks f =
        let mkTuple k = runMaybeT $ (k,) <$> f k
        in  M.fromList . catMaybes <$> traverse mkTuple ks

    finish
        :: m0 (Maybe a, b)
        -> m0 (Maybe (a, b))
    finish f = fmap (\(ma, b) -> (,b) <$> ma) f

    cleanupOldState :: forall m.
        (MonadIO m, MonadBuildTop m)
        => Watcher 'RootLayer WatcherData -> m ()
    cleanupOldState newWatcher =
        forM_ s0 $ \(oldWatcher, _) -> W.updateAll go oldWatcher
      where
        go :: forall x. BasicLayer x
            => Watcher x WatcherData
            -> m (Watcher x WatcherData)
        go w = do
            inot <- askInotify
            -- If the current node doesn't exist in the new tree, we run a
            -- cleanup action.
            unless (isJust (W.lookup (W.watcherKey w) newWatcher)) $
                liftIO $ Inotify.rmWatch inot $ getWatch $ W.extract w
            -- We use updateAllWatcher to traverse the old tree, but nothing
            -- actually needs to get modified.
            pure w

-- | When certain events fire, we need to update the state of the Watcher
updateWatchState
    :: forall m0. (MonadIO m0, MonadBuildTop m0)
    => SomeEvent
    -> BuildTopState
    -> m0 BuildTopState
updateWatchState se0 = withSomeEvent se0 $ \case
    IgnoredInotifyEvent _ _ -> pure
    InotifyEvent _ rp e -> go rp e
    InitWatchScanEvent rp e -> go rp e
  where
    go :: RealPath -> BuildTopEvent -> BuildTopState -> m0 BuildTopState
    go rp e0 s0@(w0,wm0) = finish $ case e0 of
        CategoryEvent AddEvent cat -> do
            let key = RootKey
            (d, es) <- mkData key ()
            ins es $ InsertPayload key $ CategoryWatcher M.empty cat d
        CategoryEvent RemoveEvent cat -> del $ CategoryKey cat
        PackageEvent AddEvent pkg -> do
            let cat = getCategory pkg
                key = CategoryKey cat
            (d, es) <- mkData key cat
            ins es $ InsertPayload key $ PackageWatcher Nothing pkg d
        PackageEvent RemoveEvent pkg -> del $ PackageKey pkg
        LockFileEvent e pkg ->
            let b = case e of
                        AddEvent -> True
                        RemoveEvent -> False
            in pure $ W.updateLockFile b pkg w0
        TempDirEvent AddEvent pkg -> do
            let key = PackageKey pkg
            (d, es) <- mkData key pkg
            ins es $ InsertPayload key $ TempDirWatcher Nothing pkg d
        TempDirEvent RemoveEvent pkg -> del $ TempDirKey pkg
        LogFileEvent AddEvent pkg -> do
            let key = TempDirKey pkg
            (d, es) <- mkData key pkg
            ins es $ InsertPayload key $ LogFileWatcher pkg d
        LogFileEvent RemoveEvent pkg -> del $ LogFileKey pkg
        LogFileWriteEvent _ -> pure w0
      where
        ins ::
            ( MonadIO m, MonadBuildTop m, MonadState WatchMap m)
            => InitEvents
            -> InsertPayload WatcherData
            -> m (Watcher 'RootLayer WatcherData)
        ins es payload = do
            let w = W.insert payload w0 -- Insert the payload
            wm <- get -- Get the fresh WatchMap
            let update s e = updateWatchState (SomeEvent e) s
            (w',wm') <- foldM update (w,wm) es -- update for each event
            put wm' -- Put the fresh WatchMap
            pure w' -- Return the Watcher

        del :: (BasicLayer l, MonadIO m, MonadBuildTop m)
            => WatcherKey l
            -> m (Watcher 'RootLayer WatcherData)
        del key = do
            inot <- askInotify
            let f w = do
                    liftIO $ Inotify.rmWatch inot $ getWatch $ W.extract w
                    empty
                payload = AlterPayload key (alterMaybeT f)
            W.alter payload w0

        mkData :: forall l m.
            ( BasicLayer l
            , HasFilter l
            , MonadIO m
            , MonadState WatchMap m
            , MonadBuildTop m
            )
            => WatcherKey l
            -> FilterInput l
            -> MaybeT m (WatcherData, InitEvents)
        mkData key filtIn =
            runWriterT $ createWatcherData (Just s0) key filtIn rp

        finish
            :: MaybeT
                (StateT WatchMap m0)
                (Watcher 'RootLayer WatcherData)
            -> m0 BuildTopState
        finish m = do
            -- Create the environment needed for watchHelper
            (mw, wm) <- runStateT (runMaybeT m) wm0
            -- Return the new state if a new Watcher was created, otherwise
            -- return the old state.
            pure $ maybe s0 (,wm) mw


type InitEvents = DList (EventWithSource 'InitWatchScan)

-- | Generate 'WatcherData' from needed information. For convenience, the
--   'WatchMap' is carried in 'MonadState'. This also returns any events that
--   were generated by 'initWatch', which need to be handled elsewhere.
createWatcherData
    :: forall l m.
        ( BasicLayer l
        , HasFilter l
        , Alternative m
        , MonadWriter InitEvents m
        , MonadIO m
        , MonadState WatchMap m
        , MonadBuildTop m
        )
    => Maybe BuildTopState -- ^ The old state, if any
    -> WatcherKey l        -- ^ The key for the 'Watcher' node
    -> FilterInput l       -- ^ Needed input for 'layerFilter'
    -> RealPath            -- ^ path of the 'Watcher' node
    -> m WatcherData
createWatcherData s0 key filtIn rp = do
    let proxy = Proxy @l
        check = case W.watcherType proxy of
            WatchDirectory -> doesDirectoryExist
            WatchFile -> doesFileExist
        filt = inotifyFilter (layerFilter proxy filtIn)
        (RealPath path) = rp
    liftIO (check path) >>= guard

    iWatch <- recycleState <|> createState proxy path
    let mapData = WatchMapData filt key filtIn rp
    modify $ M.insert iWatch mapData

    pure $ WatcherData iWatch
  where
    recycleState
        :: m Inotify.Watch
    recycleState = do
        (oldWatcher, _) <- liftMaybe s0
        w <- liftMaybe $ W.lookup key oldWatcher
        let (WatcherData iWatch) = W.extract w
        pure iWatch

    createState
        :: Proxy l
        -> FilePath
        -> m Inotify.Watch
    createState proxy path = do
        iWatch <- initWatch proxy filtIn rp
        pTraceMForceColor
            $ unwords
                [ "watcher created for"
                , path
                , "(" ++ show iWatch ++ ")"
                ]
        pure iWatch

-- | Initialize an 'Inotify.Watch'.
--
--   After the initialization, a scan is performed on the directory (if
--   applicable) to look for interesting files/subdirectories that may not have
--   been caught by the 'Inotify.Watch'. These are returned via 'MonadWriter'.
initWatch
    :: ( BasicLayer l
       , HasFilter l
       , Alternative m
       , MonadWriter InitEvents m
       , MonadIO m
       , MonadBuildTop m
       )
    => Proxy l          -- ^ Layer to act on
    -> FilterInput l    -- ^ Any needed input for 'layerFilter'
    -> RealPath         -- ^ Full path to initialize the 'Inotify.Watch'
    -> m Inotify.Watch
initWatch proxy filtIn rp = do
    let check = case W.watcherType proxy of
            WatchDirectory -> doesDirectoryExist
            WatchFile -> doesFileExist
        (RealPath p) = rp
    liftIO (check p) >>= guard
    pTraceMForceColor $ "directory " ++ p ++ " exists"
    i <- askInotify
    let m = layerMask proxy
    w <- liftIO $ Inotify.addWatch i p (layerMask proxy)
    pTraceMForceColor $ unwords ["Added watcher to inotify:", show i, show p, show m]

    -- Race condition: interesting contents may have been created in the
    -- directory _after_ the 'Inotify.Watch' was initialized. Scan the
    -- directory now and fire off events for any interesting contents we find.
    -- NOTE: This may create duplicate events if the 'Inotify.Watch' also
    -- catches the interesting content. This must be handled by the
    -- state-modifying code.
    when (W.watcherType proxy == WatchDirectory) $ do
        fs <- lenientListDirectory rp
        forM_ fs $ \fp -> do
            let checkFile = filePathFilter (layerFilter proxy filtIn)
                rp' = rp `FS.append` fp
            liftIO (checkFile fp) >>=
                mapM_ (tell . D.singleton . InitWatchScanEvent rp')

    pure w
