[![Hackage](https://img.shields.io/hackage/v/ticker.svg)](https://hackage.haskell.org/package/ticker)

# ticker

A utility of concurrent programming in Haskell, inspired by [Ticker](https://golang.org/pkg/time/#Ticker) in Go.


```haskell
import Control.Concurrent.Ticker (newTicker)
import Control.Concurrent.Chan (getChanContents)
import Control.Concurrent.Async (async, cancel)
import Control.Monad (forM_)

main :: IO ()
main = do
  (chan, cancelTicker) <- newTicker (10^3 * 100) -- tick rate: 100ms
  chanStream <- getChanContents chan
  thread <- async $ forM_ chanStream $ \_ -> do
    putStr "Tick!"
  threadDelay (10^3 * 350) -- wait 3 ticks
  putStrLn ""
  cancel thread
  cancelTicker
  
-- Tick!Tick!Tick!
```

More functions are defined in `src/Control/Concurrent/Ticker.hs`.
