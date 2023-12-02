{-# Language DataKinds #-}
-- {-# Language KindSignatures #-}

module BuildTop where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as M
import Data.IORef
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Reflex
import System.Directory
import System.FilePath
import qualified System.Linux.Inotify as Inotify

import Data.Parsable

import BuildTop.Filters
import BuildTop.Types

import Debug.Pretty.Simple

import Prelude hiding (filter)

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

eventLoop :: MonadIO m
    => Inotify.Inotify
    -> IORef WatchMap
    -> m (Async a)
eventLoop i wmRef = liftIO $ async $ forever $ do
    wm <- readIORef wmRef
    e@(Inotify.Event w _ _ _) <- Inotify.getEvent i
    forM_ (M.lookup w wm) $ \(eIO, filt) -> eIO (e, filt e)

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
