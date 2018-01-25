module LiveReload (runWithLiveReload) where

import Control.Concurrent.MVar
import Control.Concurrent ( forkIO
                          , threadDelay
                          )
import Control.Monad (when)
import System.Directory


-- TODO: Allow watching files or directories.

runWithLiveReload :: Read a =>
                     (a -> IO b) ->
                     (b -> IO b) ->
                     (b -> a -> IO b) ->
                     FilePath ->
                     IO ()
runWithLiveReload boot loop reload configFile = do
  config <- read <$> readFile configFile
  st <- boot config
  flag <- newEmptyMVar

  modTime <- getModificationTime configFile
  _ <- forkIO $ watchFile flag modTime
  loopWrapper flag st

  where
    loopWrapper flag st = do
      st' <- loop st
      maybeNewConfig <- tryTakeMVar flag
      st'' <- maybe (return st') (reload st') maybeNewConfig
      threadDelay 0 -- Allows watchFile to preempt.
      loopWrapper flag st''

    watchFile flag modTime = do
      exists <- doesFileExist configFile
      if not exists
        then putStrLn $ "watched file \"" ++ configFile ++ "\" was deleted"
        else
        do
          modTime' <- getModificationTime configFile
          when (modTime' > modTime) $ do
            putMVar flag =<< getConfig configFile
          threadDelay secondInMicroseconds
          watchFile flag modTime'

    secondInMicroseconds = 1000000

    getConfig fn = read <$> readFile fn
