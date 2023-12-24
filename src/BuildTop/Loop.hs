
module BuildTop.Loop where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as M
import Data.IORef
import Data.These
import qualified System.Linux.Inotify as Inotify

import BuildTop.State

eventLoop :: MonadIO m
    => Inotify.Inotify
    -> IORef WatchMap
    -> m (Async a)
eventLoop i wmRef = liftIO $ async $ forever $ do
    wm <- readIORef wmRef
    e@(Inotify.Event w _ _ _) <- Inotify.getEvent i
    forM_ (M.lookup w wm) $ \d ->
        withWatchMapData d $ \eIO filt _ _ _ -> eIO $
            case filt e of
                Just bte -> These e bte
                Nothing -> This e
