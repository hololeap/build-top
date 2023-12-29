{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module BuildTop.Loop
    ( eventLoop
    , scanLoop
    ) where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import qualified Data.HashMap.Strict as M
import Data.These
import qualified System.Linux.Inotify as Inotify
import Text.Pretty.Simple
import UnliftIO.Async (Async)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwString)
import UnliftIO.MVar (MVar, modifyMVar_)

import BuildTop.Filesystem (RealPath)
import BuildTop.State
import BuildTop.Types
import BuildTop.Util

-- | Infinitely wait for inotify events, firing the reflex @Event@ whenever
--   one comes in.
eventLoop :: forall m. (MonadUnliftIO m, MonadBuildTop m)
    => MVar BuildTopState
    -> m (Async ())
eventLoop mvar = do
    i <- askInotify
    fireE <- askFireEvent
    foreverThread $ do
        e@(Inotify.Event w _ _ _) <- liftIO $ Inotify.getEvent i

        modifyMVar_ mvar $ \s0@(_,wm0) -> case M.lookup w wm0 of
            -- It's possible that the inotify event came in before the MVar was
            -- updated, meaning the Watch may have been deleted from the
            -- WatchMap. If this is the case, print a warning and continue.
            Nothing -> do
                liftIO $ putStrLn "warning: could not find Watch in WatchMap:"
                pPrintForceColor w
                liftIO $ fireE $ This e
                pure s0
            Just d ->
                withWatchMapData d $ \filt _ _ _ ->
                    case filt e of
                        Nothing -> do
                            liftIO $ fireE $ This e
                            pure s0
                        Just bte -> do
                            liftIO $ fireE $ These e bte
                            -- We can run this if the filter returned a BuildTopEvent
                            updateWatchState e bte s0

-- | Repeatedly scan the portage temp dir, firing events and updating state.
--
--   This repeats every 3s
scanLoop :: (MonadUnliftIO m, MonadBuildTop m)
    => RealPath
    -> MVar BuildTopState
    -> m (Async ())
scanLoop path mvar = foreverThread $ do
    modifyMVar_ mvar $ \s0 -> do
        let err = throwString
                $ "portage temp dir not found? " ++ show path
        maybe err pure =<< scanState (Just s0) path

    threadDelay 3000000
