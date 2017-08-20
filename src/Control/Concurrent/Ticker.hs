module Control.Concurrent.Ticker
  ( newTicker
  , withTicker
  , newCPUTimeTicker
  ) where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
import System.CPUTime (getCPUTime)

-- | Create a new ticker.
--
-- >>> import Control.Concurrent.Chan (getChanContents)
-- >>> import Control.Monad (forM_)
-- >>> import System.Timeout (timeout)
-- >>> (chan, cancelTicker) <- newTicker $ 10^3 * 100
-- >>> chanStream <- getChanContents chan
-- >>> _ <- timeout (10^3 * 350) $ forM_ chanStream (\_ -> putStr "Tick!")
-- Tick!Tick!Tick!
-- >>> cancelTicker
newTicker
  :: Int                 -- ^ ticker rate by micro sec
  -> IO (Chan (), IO ()) -- ^ ticker channel and ticker stopper
newTicker microSec = do
  chan <- newChan
  thread <- async $ forever $ do
    threadDelay microSec
    writeChan chan ()
  return (chan, cancel thread)

-- | Create a new ticker and pass a @Chan ()@ of the ticker to the supplied function.
-- The ticker thread will be closed automatically.
--
-- >>> import Control.Concurrent.Chan (getChanContents)
-- >>> import Control.Monad (forM_)
-- >>> import System.Timeout (timeout)
-- >>> :{
-- withTicker (10^3 * 100) $ \chan -> do
--   chanStream <- getChanContents chan
--   _ <- timeout (10^3 * 350) $ forM_ chanStream (\_ -> putStr "Tick!")
--   return ()
-- :}
-- Tick!Tick!Tick!
withTicker
  :: Int               -- ^ ticker rate by micro sec
  -> (Chan () -> IO a) -- ^ handler function
  -> IO a              -- ^ result of handler
withTicker microSec action = bracket
                             (newTicker microSec)
                             (\(_, cancelTicker) -> cancelTicker)
                             (\(chan, _) -> action chan)

-- | A variant of the 'newTicker' that uses @getCPUTime@ instead of @threadDelay@ internally.
newCPUTimeTicker :: Int -> IO (Chan (), IO ())
newCPUTimeTicker microSec = do
  chan <- newChan
  initialTick <- getCPUTime
  let picoSec = (toInteger microSec) * 10^6
  thread <- async $ go chan $ iterate (+picoSec) initialTick
  return (chan, cancel thread)
    where
      go :: Chan () -> [Integer] -> IO ()
      go chan checkTicks = do
        threadDelay $ 10^3 -- to avoid busy loop
        now <- getCPUTime
        let (target, rest) = span (<now) checkTicks
        forM_ target $ \_ -> writeChan chan ()
        go chan rest
{-# DEPRECATED newCPUTimeTicker "It doesn't work well." #-}
