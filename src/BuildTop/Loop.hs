{-# Language FlexibleContexts #-}

module BuildTop.Loop where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as M
import Data.IORef
import Data.These
import qualified System.Linux.Inotify as Inotify

import BuildTop.State
import BuildTop.Types

eventLoop :: (MonadIO m, MonadBuildTop m)
    => IORef WatchMap
    -> m (Async a)
eventLoop wmRef = do
    i <- askInotify
    fireE <- askFireEvent
    liftIO $ async $ forever $ do
        wm <- readIORef wmRef
        e@(Inotify.Event w _ _ _) <- Inotify.getEvent i
        forM_ (M.lookup w wm) $ \d ->
            withWatchMapData d $ \filt _ _ _ -> fireE $
                case filt e of
                    Just bte -> These e bte
                    Nothing -> This e
