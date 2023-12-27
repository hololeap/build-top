{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import qualified Data.HashMap.Strict as M
import Data.IORef
import Reflex
import Reflex.Host.Headless
import qualified System.Linux.Inotify as Inotify
import Text.Pretty.Simple

import BuildTop.Debug
import BuildTop.Filesystem
import BuildTop.Loop
import BuildTop.State
import qualified BuildTop.Watcher as W

app :: MonadHeadlessApp t m => m (Event t ())
app = mdo
    _ <- runMaybeT $ do
        i <- liftIO $ Inotify.init
        (e, fireE) <- newTriggerEvent
        flip runReaderT (i, fireE) $ do
            Just (rw,wm) <- scanState Nothing (RealPath "/var/tmp/portage")
            wmRef <- liftIO $ newIORef wm
            _ <- eventLoop wmRef
            liftIO $ print $ M.size $ W.rootWatcher_Children rw
            performEvent $ liftIO . pPrintForceColor . first (printEvent rw) <$> e
    liftIO $ putStrLn "end of 'app'"
    pure never

main :: IO ()
main = runHeadlessApp app
