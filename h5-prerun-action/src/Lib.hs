{-
-- A simple solution that does not pass the hardcore mode.
module Lib
    ( prerun
    ) where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Data.Functor

prerun :: (IO a -> IO (IO b)) -> IO (IO a -> IO b)
prerun f = do
    hole <- newEmptyMVar
    f (join $ readMVar hole) <&> \b getX' ->
        uninterruptibleMask $ \restore -> do
            putMVar hole getX'
            restore b `finally` takeMVar hole
-}

-- Combining https://www.reddit.com/r/haskell/comments/mia1fk/prerun_an_action/gt82rox
-- and https://gist.github.com/aaronallen8455/93ab337e19b6ec104453e2340f2d63b8
-- to get a solution that passes the hardcore mode.
module Lib
    ( prerun
    ) where

import           Control.Concurrent
import           Data.IORef
import qualified Data.Map           as M
import           Data.Maybe

prerun :: (IO a -> IO (IO b)) -> IO (IO a -> IO b)
prerun input = do
  threadMapRef <- newIORef mempty

  ioB <- input $ do
    tId <- myThreadId
    m <- readIORef threadMapRef
    fromMaybe (error "empty job list")
      . listToMaybe
      . fromMaybe (error "thread not registered")
      $ M.lookup tId m

  pure $ \ioA -> do
    tId <- myThreadId
    atomicModifyIORef threadMapRef (\m -> (M.insertWith (++) tId [ioA] m, ()))
    y <- ioB
    atomicModifyIORef threadMapRef (\m -> (M.adjust tail tId m, ()))
    pure y
