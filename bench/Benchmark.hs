module Main where

import Control.Concurrent.Async
import Control.Monad (replicateM)
import Criterion.Main
import Data.Maybe (isNothing)
import Network.Wai.RateLimitMiddleware

doClientOps :: Int -> Int -> Rate -> Cache Int -> IO (Int, Int)
doClientOps cId numOps rate cache =
  do
    results <- replicateM numOps $ tryAllocate cId rate 1 cache
    let successCount = length $ filter isNothing results
        res = (successCount, numOps - successCount)
    -- print res
    return res

runClients :: [(Int, Int, Rate)] -> Cache Int -> IO [(Int, Int)]
runClients args cache =
  forConcurrently args $ \(cId, numOps, rate) -> doClientOps cId numOps rate cache

main :: IO ()
main = do
  let clientArgs n = map (,20,mkRate 10 (10, 1)) [1 .. n]
  defaultMain $
    [ bgroup
        "Rate limit benchmarks"
        [ bench "1 client" $ nfIO $ newCache >>= runClients (clientArgs 1),
          bench "10 clients" $ nfIO $ newCache >>= runClients (clientArgs 10),
          bench "100 clients" $ nfIO $ newCache >>= runClients (clientArgs 100),
          bench "1000 clients" $ nfIO $ newCache >>= runClients (clientArgs 1000)
        ]
    ]
