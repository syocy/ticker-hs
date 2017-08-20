module Control.Concurrent.TickerSpec where

import Test.Hspec
import Control.Concurrent.Ticker

import Data.Maybe
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.DeepSeq

spec :: Spec
spec = do
  it "newTicker 5 times" $ do
    (chan, cancelTicker) <- newTicker (10^3 * 100)
    tickStream <- getChanContents chan
    thread <- async $ do
      let x = take 5 tickStream
      x `deepseq` return ()
    running <- poll thread
    isNothing running `shouldBe` True
    threadDelay $ 10^6 * 1
    finished <- poll thread
    isJust finished `shouldBe` True
    cancelTicker
  it "withTicker 5 times" $ do
    withTicker (10^3 * 100) $ \chan -> do
      tickStream <- getChanContents chan
      thread <- async $ do
        let top5 = take 5 tickStream
        top5 `deepseq` return ()
      running <- poll thread
      isNothing running `shouldBe` True
      threadDelay $ 10^6 * 1
      finished <- poll thread
      isJust finished `shouldBe` True
