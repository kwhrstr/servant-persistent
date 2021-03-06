{-# LANGUAGE OverloadedStrings #-}

-- | Running your app inside GHCi.
--
-- > stack ghci
--
-- To start your app, run:
--
-- > :l DevelMain
-- > DevelMain.update
--
-- You can also call @DevelMain.shutdown@ to stop the app
--
-- There is more information about this approach,
-- on the wiki: https://github.com/yesodweb/yesod/wiki/ghci

module DevelMain where

import           Prelude

import           Control.Concurrent       (MVar, ThreadId, forkIO, killThread,
                                           newEmptyMVar, putMVar, takeMVar)
import           Control.Exception        (finally)
import           Control.Monad            ((>=>))
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Foreign.Store            (Store (..), lookupStore, readStore,
                                           storeAction, withStore)
import           GHC.Word                 (Word32)
import           Init                     (runApp)

update :: IO()
update = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    Nothing -> do
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
    Just tidStore -> restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0
    
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
      killThread tid
      withStore doneStore takeMVar
      readStore doneStore >>= start
    
    start :: MVar ()  -- ^ Written to when the thread is killed.
             -> IO ThreadId
    start done =
          forkIO (finally runApp
            -- Note that this implies concurrency
            -- between shutdownApp and the next app that is starting.
            -- Normally this should be fine
            (putMVar done ()))


shutdown :: IO ()
shutdown = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    Nothing -> putStrLn "no app running"
    Just tidStore -> do
      withStore tidStore $ readIORef >=> killThread
      putStrLn "App is shutdown"


tidStoreNum :: Word32
tidStoreNum = 1


modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO()
modifyStoredIORef store f = withStore store $ \ref -> do
  v <- readIORef ref
  f v >>= writeIORef ref