{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative
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
import BuildTop.Loop
import BuildTop.State
import BuildTop.Types
import BuildTop.Util
import qualified BuildTop.Watcher as W

app :: MonadHeadlessApp t m => m (Event t ())
app = mdo
    runMaybeT $ do
        i <- liftIO $ Inotify.init
        Just (rw, wm) <- scanState "/var/tmp/portage" i Nothing
        wmRef <- liftIO $ newIORef wm
        eventLoop i wmRef
        liftIO $ print $ M.size $ W.rootWatcher_Children rw
--         pPrintForceColor $ "Size of getEvents: " ++ show (length (getEvents rw))
        let e = mergeList $ map W.getEvent (W.toList rw)
        -- performEvent $ liftIO . (\e -> pPrintForceColor e *> pPrintForceColor (printEvent e)) <$> e
        performEvent $ liftIO . pPrintForceColor . (fmap (first (printEvent rw))) <$> e
    liftIO $ putStrLn "end of 'app'"
    pure never

main :: IO ()
main = runHeadlessApp app


-- filterIEvents i e rw = case
--     ( check Inotify.in_ISDIR
--     , check Inotify.in_CREATE
--     , check Inotify.in_DELETE
--     , check Inotify.in_CLOSE_WRITE
--     ) of
--         (True, True, _, _) -> do
--             if L.isInfixOf "-" (Inotify.name e)
--
--   where
--     check = Inotify.isSubset (Inotify.mask e)
--
--     checkCatName [] = False
--     checkCatName n@(c:_)
--         | isAsciiLower c = L.isInfixOf "-" n
--         | otherwise = False
--
--     checkPkgVerName [] = False
--     checkPkgVerName (c:_) = isLetter c
