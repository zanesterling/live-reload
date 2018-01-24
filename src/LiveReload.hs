module Control.LiveReload (runWithLiveReload) where

import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Monad (when)
import System.Directory (getModificationTime)


runWithLiveReload :: Read a =>
                     (a -> IO b) ->
                     (b -> IO b) ->
                     (b -> a -> IO b) ->
                     FilePath ->
                     IO ()
runWithLiveReload boot loop reload configFile = do
  config <- read <$> readFile configFile
  st <- boot config
  flag <- newEmptyMVar :: IO (MVar b)
  loopTid <- forkIO $ loopWrapper flag st
  modTime <- getModificationTime configFile
  watchFile flag loopTid modTime

  where
    loopWrapper flag st = do
      st' <- loop st
      maybeNewConfig <- tryTakeMVar flag
      st'' <- maybe (return st') (reload st') maybeNewConfig
      loopWrapper flag st''

    watchFile flag loopTid modTime = do
      modTime' <- getModificationTime configFile
      when (modTime' > modTime) $
        putMVar flag =<< getConfig configFile

    getConfig fn = read <$> readFile fn
