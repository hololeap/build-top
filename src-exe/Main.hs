{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor
import Reflex
import Reflex.Host.Headless
import qualified System.Linux.Inotify as Inotify
import Text.Pretty.Simple
import UnliftIO.Async
import UnliftIO.Exception
import UnliftIO.MVar

import BuildTop.Debug
import BuildTop.Loop
import BuildTop.Paths
import BuildTop.State
import BuildTop.Types

-- | The 'MVar' is needed to get the 'Async's out
app :: MonadHeadlessApp t m => MVar (Async (), Async ()) -> m (Event t ())
app asyncMVar = do
    i <- liftIO $ Inotify.init
    (e, fireE) <- newTriggerEvent

    sMVar <- liftIO $ flip runReaderT (i, fireE) $ do
        let err = throwString
                $ "portage temp dir not found? " ++ show portageTempDir
        s <- maybe err pure =<< scanState Nothing portageTempDir
        sMVar <- newMVar s
        scanAsync <- scanLoop portageTempDir sMVar
        eventAsync <- eventLoop sMVar
        putMVar asyncMVar (scanAsync, eventAsync)
        pure sMVar

    let debugEvent :: MyEvent -> IO ()
        debugEvent myE = do
            (rw,_) <- readMVar sMVar
            pPrintForceColor $ first (printEvent rw) myE
    _ <- performEvent $ liftIO . debugEvent <$> e

    liftIO $ pPrintForceColor "end of 'app'"
    pure never

main :: IO ()
main = do
    asyncMVar <- newEmptyMVar

    -- 'runHeadlessApp' blocks, so we run it in an async
    appAsync <- async $ runHeadlessApp (app asyncMVar)

    (scanAsync, eventAsync) <- catchAny (readMVar asyncMVar) $ \e -> do
        -- If the readMVar operation fails, first check to see if 'app' threw
        -- an exception. If it did, raise that instead.
        appStatus <- poll appAsync
        case appStatus of
            Just (Left e') -> do
                -- Print the original exception, but don't raise it
                pPrintForceColor e
                throwIO e'

            _ -> throwIO e -- raise the original exception

    void $ waitAny [appAsync, scanAsync, eventAsync]
