{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor
import Reflex
import Reflex.Host.Headless
import qualified System.Linux.Inotify as Inotify
import Text.Pretty.Simple

import BuildTop.Debug
import BuildTop.Loop
import BuildTop.Paths
import BuildTop.State
import BuildTop.Types

app :: MonadHeadlessApp t m => m (Event t ())
app = mdo

    i <- liftIO $ Inotify.init
    (e, fireE) <- newTriggerEvent
    _ <- flip runReaderT (i, fireE) $ do
        let err = error
                $ "portage temp dir not found? " ++ show portageTempDir
        s@(rw,_) <- maybe err pure =<< scanState Nothing portageTempDir
        wmRef <- liftIO $ newMVar s
        let debugEvent :: MyEvent -> IO ()
            debugEvent = pPrintForceColor . first (printEvent rw)
        _ <- performEvent $ liftIO . debugEvent <$> e
        ea <- eventLoop wmRef
        sa <- scanLoop portageTempDir wmRef
        pure (ea,sa)
    liftIO $ putStrLn "end of 'app'"
    pure never

main :: IO ()
main = runHeadlessApp app
