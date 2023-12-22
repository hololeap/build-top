{-# Language DataKinds #-}
{-# Language GADTs #-}

module BuildTop.Debug where

import Data.Monoid (First(..))
import qualified System.Linux.Inotify as Inotify
import Witherable

import Distribution.Portage.Types

import BuildTop.Types
import BuildTop.Watcher (Watcher(..), WatcherData(..))

import Prelude hiding (filter)

printEvent :: Watcher 'RootLayer t (WatcherData t) -> Inotify.Event -> (String, String, [String])
printEvent rw e =
    ( show $ locateWatch rw (Inotify.wd e)
    , show e
    , (\(_,_,s) -> show s) <$> filter (\(m,_,_) -> Inotify.isSubset m (Inotify.mask e)) events
    )
  where
    events :: [(Inotify.Mask Inotify.EventFlag, String, String)]
    events =
        [ (Inotify.in_ACCESS, "ACCESS", "File was accessed")
        , (Inotify.in_ATTRIB, "ATTRIB", "Metadata  changed")
        , (Inotify.in_CLOSE_WRITE, "CLOSE_WRITE", "File opened for writing was closed")
        , (Inotify.in_CLOSE_NOWRITE, "CLOSE_NOWRITE", "File or directory not opened for writing was closed")
        , (Inotify.in_CREATE, "CREATE", "File/directory created in watched directory")
        , (Inotify.in_DELETE, "DELETE", "File/directory deleted from watched directory")
        , (Inotify.in_DELETE_SELF, "DELETE_SELF", "Watched file/directory was itself deleted")
        , (Inotify.in_MODIFY, "MODIFY", "File was modified")
        , (Inotify.in_MOVE_SELF, "MOVE_SELF", "Watched file/directory was itself moved")
        , (Inotify.in_MOVED_FROM, "MOVED_FROM"
            , "Generated for the directory containing the old filename when a file is renamed")
        , (Inotify.in_MOVED_TO, "MOVED_TO"
            , "Generated for the directory containing the new filename when a file is renamed")
        , (Inotify.in_OPEN, "OPEN", "File was opened. Includes the files of a watched directory")
        , (Inotify.in_IGNORED, "IGNORED", "Watch was removed explicitly")
        , (Inotify.in_ISDIR, "ISDIR", "Subject of this event is a directory")
        , (Inotify.in_Q_OVERFLOW, "Q_OVERFLOW", "Event queue overflowed")
        , (Inotify.in_UNMOUNT, "UNMOUNT", "Filesystem containing watched object was unmounted")
        ]

locateWatch
    :: Watcher 'RootLayer t (WatcherData t)
    -> Inotify.Watch
    -> Maybe (Maybe (Category, Maybe Package))
locateWatch (RootWatcher m0 _ (WatcherData w0 _)) w
    | w == w0 = Just Nothing
    | otherwise = getFirst $ foldMap lwCat m0
  where
    lwCat (CategoryWatcher m1 c (WatcherData w1 _))
        | w == w1 = First $ Just $ Just (c, Nothing)
        | otherwise = foldMap (lwPkg . snd) m1
      where
        lwPkg (PackageWatcher m2 p (WatcherData w2 _))
            | w == w2 = First $ Just $ Just (c, Just p)
            | otherwise = foldMap lwTD m2
          where
            lwTD (TempDirWatcher m3 _ (WatcherData w3 _))
                | w == w3 = First $ Just $ Just (c, Just p)
                | otherwise = foldMap lwLF m3
            lwLF (LogFileWatcher _ (WatcherData w4 _))
                | w == w4 = First $ Just $ Just (c, Just p)
                | otherwise = First Nothing

