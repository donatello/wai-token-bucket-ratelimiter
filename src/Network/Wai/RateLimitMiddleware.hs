-- |
-- Module     : Network.Wai.RateLimitMiddleware
--
-- WAI Rate Limiting Middleware.
--
-- Rate limiting is configured by providing a function that maps a 'Request' to
-- a 'key' and providing a 'Rate' for it. The 'Rate' configures the maximum
-- number of requests to allow after a period of inactivity (or at the
-- beginning), and an average rate at which requests should be allowed.
--
-- Note that rate limiting state is maintained in memory by this module and a
-- web server restart will reset all state. Thus, this module is more
-- appropriate for limits that apply over shorter periods of time.
module Network.Wai.RateLimitMiddleware
  ( -- * Types, constructors and setters
    Rate (..),
    mkRate,
    RateLimitSettings,
    newRateLimitSettings,
    setRateLimitExceededResponse,
    setResetInterval,

    -- * Middleware
    rateLimitMiddleware,

    -- * Other useful code
    stopResetThread,
    Cache,
    newCache,
    tryAllocate,
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar qualified as M
import Control.Concurrent.TokenBucket
import Control.Monad (forever)
import Data.HashMap.Strict qualified as H
import Data.Hashable qualified as H
import Data.Maybe (isNothing)
import Data.Word (Word64)
import Network.HTTP.Types (status429)
import Network.Wai (Middleware, Request, Response, responseLBS)

newtype CacheData key = CacheData
  { contents :: H.HashMap key TokenBucket
  }

-- | A cache that maps request 'key's to a 'TokenBucket'.
newtype Cache key = Cache (M.MVar (CacheData key))

newCache :: IO (Cache key)
newCache = Cache <$> M.newMVar (CacheData {contents = H.empty})

resetCache :: Cache key -> IO ()
resetCache (Cache mv) =
  M.modifyMVar_ mv $
    const $
      return CacheData {contents = H.empty}

-- | @RateLimitSetings@ holds settings for the rate limiting middleware.
data RateLimitSettings key = RateLimitSettings
  { getRequestKeyAndRate :: Request -> IO (key, Rate),
    limitExceededResponse :: Response,
    tbCache :: Cache key,
    resetInterval :: Maybe Int,
    resetThreadId :: Maybe ThreadId
  }

-- | Create new rate limit settings by providing a function to map a request to
-- a key and a Rate. It sets up a default response for requests that exceed the
-- rate limit that returns a 429 status code with a simple error message.
newRateLimitSettings :: (Request -> IO (key, Rate)) -> IO (RateLimitSettings key)
newRateLimitSettings keyRateFunc = do
  c <- newCache
  return $
    RateLimitSettings
      { getRequestKeyAndRate = keyRateFunc,
        limitExceededResponse = responseLBS status429 [] "Rate limit exceeded",
        tbCache = c,
        resetInterval = Nothing,
        resetThreadId = Nothing
      }

-- | Set a custom error response.
setRateLimitExceededResponse :: RateLimitSettings key -> Response -> RateLimitSettings key
setRateLimitExceededResponse s rsp = s {limitExceededResponse = rsp}

-- | @setResetInterval@ starts a thread to reset (clear) the token buckets map
-- after every given interval period of time (expressed in seconds). This is
-- useful if your webserver generates a lot of request 'key's that go idle after
-- some activity. In this situation the token bucket cache memory usage grows as
-- it contains an entry every key seen. When the cache is reset, the memory can
-- be garbage collected. Though this will cause all rate limit token buckets to
-- go "full" (i.e. allow the full burst of requests immediately), this solution
-- is acceptable as this is the case when the webserver is restarted as well. By
-- default, there is no reset thread launched (unless this function is called).
setResetInterval :: RateLimitSettings key -> Int -> IO (RateLimitSettings key)
setResetInterval s seconds = do
  stopResetThread s
  tid <- forkIO $ forever $ do
    threadDelay $ seconds * 1000000
    resetCache $ tbCache s
  return $
    s
      { resetInterval = Just seconds,
        resetThreadId = Just tid
      }

-- | @stopResetThread@ stops the thread launched by 'setResetInterval' and is
-- provided for completeness. If your application automatically restarts the
-- web-server using the rate limit middleware, call this function in your web
-- server's shutdown handler to ensure that the reset thread is killed (and does
-- not leak).
stopResetThread :: RateLimitSettings key -> IO ()
stopResetThread = maybe (return ()) killThread . resetThreadId

-- | @tryAllocate@ attempts to allocate the given @amount@ from the
-- 'TokenBucket' corresponding to the given @key@ and @Rate@ in the @Cache@. On
-- success it returns @Nothing@, otherwise it returns the minimum time (in
-- nanoseconds) to wait after which the allocation can succeed.
tryAllocate :: (H.Hashable key) => key -> Rate -> Word64 -> Cache key -> IO (Maybe Word64)
tryAllocate k r amount (Cache mv) = do
  tb <- M.modifyMVar mv $ \CacheData {..} ->
    case H.lookup k contents of
      Just tb' -> return (CacheData contents, tb')
      Nothing -> do
        tb' <- newTokenBucket r
        return (CacheData $ H.insert k tb' contents, tb')
  tryAllocateTokens tb amount r

-- | @rateLimitMiddleware@ performs rate limiting according to the given
-- settings.
rateLimitMiddleware :: (H.Hashable a) => RateLimitSettings a -> Middleware
rateLimitMiddleware RateLimitSettings {..} app req sendResponse = do
  -- get request key and configured rate
  (key, rate) <- getRequestKeyAndRate req

  -- check that the client has not exceeded
  allowRequest <- isNothing <$> tryAllocate key rate 1 tbCache

  if allowRequest
    then app req sendResponse
    else sendResponse limitExceededResponse
