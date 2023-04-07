{-# LANGUAGE CPP #-}

module Control.Concurrent.TokenBucket
  ( Rate (..),
    mkRate,
    TokenBucket,
    newTokenBucket,
    tryAllocateTokens,
  )
where

import Control.Concurrent.MVar qualified as M
import Data.Word (Word64)
import System.Clock qualified as C

-- | Rate represents token bucket parameters.
data Rate = Rate
  { -- | Maximum number of tokens that the token bucket can hold.
    rateBurstAmount :: !Word64,
    -- | Rate at which tokens are to be added to the bucket - expressed as time
    -- in nanoseconds after which a token is added.
    rateNanosPerToken :: !Word64
  }
  deriving stock (Show, Eq)

-- | mkRate creates a 'Rate' given the burst amount, and the number of
-- operations (must be > 0) to allow per number of seconds given.
mkRate :: Word64 -> (Word64, Word64) -> Rate
mkRate burst (numOperations, numSeconds) =
  let nanos = fromIntegral $ numSeconds * C.s2ns :: Double
      perToken = round (nanos / fromIntegral numOperations)
   in Rate burst perToken

data TB = TB
  { tbTokens :: !Word64,
    -- lastCheck time is expressed in nanoseconds since some starting point.
    tbLastCheck :: !Word64,
    tbRate :: !Rate
  }
  deriving stock (Show, Eq)

newtype TokenBucket = TokenBucket (M.MVar TB)

-- | newTokenBucket creates an initially full token bucket.
newTokenBucket :: Rate -> IO TokenBucket
newTokenBucket r = do
  now <- getTimeNanos
  mv <- M.newMVar $ TB (rateBurstAmount r) now r
  return $ TokenBucket mv

#if linux_HOST_OS==1
-- On Linux we use MonotonicCoarse for better performance.
getTimeNanos :: IO Word64
getTimeNanos = do
  t <- C.getTime C.MonotonicCoarse
  return $ fromInteger $ C.toNanoSecs t
#else
getTimeNanos :: IO Word64
getTimeNanos = do
  t <- C.getTime C.Monotonic
  return $ fromInteger $ C.toNanoSecs t
#endif

-- unsigned arithmetic helpers
minus, plus :: Word64 -> Word64 -> Word64
minus a b
  | a > b = a - b
  | otherwise = 0
plus a b = let s = a + b in if a <= s then s else maxBound

-- | 'tryAllocate tb amount rate' attempts to allocate 'amount' tokens from the
-- given token bucket at the given rate. On success, it returns Nothing, and on
-- failure it returns the minimum amount of time to wait in nanoseconds before
-- the allocation can succeed.
tryAllocateTokens :: TokenBucket -> Word64 -> Rate -> IO (Maybe Word64)
tryAllocateTokens _ _ r | rateNanosPerToken r == 0 = return Nothing -- infinite token rate
tryAllocateTokens _ amountRequested r | amountRequested > rateBurstAmount r = return $ Just maxBound
tryAllocateTokens (TokenBucket mv) amountRequested r =
  M.modifyMVar mv $ \(TB lvl ts _) -> do
    ct <- getTimeNanos
    let dt = ct `minus` ts
        (dl, rt) = dt `quotRem` rateNanosPerToken r
        lt' = ct `minus` rt
        lvl'
          | lvl `plus` dl > rateBurstAmount r = rateBurstAmount r
          | otherwise = lvl `plus` dl
    if lvl' >= amountRequested
      then return (TB (lvl' - amountRequested) lt' r, Nothing)
      else do
        let wantTokens = amountRequested `minus` lvl'
            wait = wantTokens * rateNanosPerToken r `minus` rt
        return (TB lvl' lt' r, Just wait)
