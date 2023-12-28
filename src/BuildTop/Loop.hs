{-# Language FlexibleContexts #-}

module BuildTop.Loop
    ( eventLoop
    , scanLoop
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.HashMap.Strict as M
import Data.These
import qualified System.Linux.Inotify as Inotify

-- import BuildTop.Paths
import BuildTop.Filesystem (RealPath)
import BuildTop.State
import BuildTop.Types
import BuildTop.Util

import Debug.Pretty.Simple

-- | Infinitely wait for inotify events, firing the reflex @Event@ whenever
--   one comes in.
eventLoop :: (MonadIO m, MonadBuildTop m)
    => MVar BuildTopState
    -> m (Async ())
eventLoop mvar = do
    i <- askInotify
    fireE <- askFireEvent
    foreverThread $ do
        e@(Inotify.Event w _ _ _) <- Inotify.getEvent i

        modifyMVar_ mvar $ \s0@(_,wm0) -> do
            let err = error -- FIXME: This may not be a wise idea inside a thread
                    $ "could not find Watch " ++ show w ++ " in WatchMap"
            d <- maybe err pure $ M.lookup w wm0

            -- updateWatchState requires a MonadBuildTop (e.g. ReaderT)
            flip runReaderT (i,fireE) $ withWatchMapData d $ \filt _ _ _ ->
                case filt e of
                    Nothing -> do
                        liftIO $ fireE $ This e
                        pure s0
                    Just bte -> do
                        liftIO $ fireE $ These e bte
                        -- We can run this if the filter returned a BuildTopState
                        updateWatchState e bte s0

-- | Repeatedly scan the portage temp dir, firing events and updating state.
--
--   This repeats every 3s
scanLoop :: (MonadIO m, MonadBuildTop m)
    => RealPath
    -> MVar BuildTopState
    -> m (Async ())
scanLoop path mvar = do
    i <- askInotify
    fireE <- askFireEvent
    foreverThread $ do
        pTraceMForceColor "scanLoop fired"

        modifyMVar_ mvar $ \s0 -> do
            flip runReaderT (i, fireE) $ do
                let err = error -- FIXME: This may not be a wise idea inside a thread
                        $ "portage temp dir not found? " ++ show path
                maybe err pure =<< scanState (Just s0) path

        threadDelay 3000000
