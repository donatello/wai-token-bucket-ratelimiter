module RateLimiterSpec where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (bracket)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Network.HTTP.Client (Manager, defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (Status (statusCode), status200)
import Network.Wai
  ( Application,
    Request (rawPathInfo),
    responseLBS,
  )
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.RateLimitMiddleware
  ( Rate,
    infRate,
    mkRate,
    newRateLimitSettings,
    rateLimitMiddleware,
  )
import Test.Hspec (Spec, around_, context, describe, it, shouldBe)

testApp :: (Request -> IO (ByteString, Rate)) -> IO Application
testApp keyFunc = do
  let app _ respond = respond $ responseLBS status200 [] "Ok!"
  strategy <- newRateLimitSettings keyFunc
  return $ rateLimitMiddleware strategy app

testKeyFunc :: Request -> IO (ByteString, Rate)
testKeyFunc r = do
  let path = rawPathInfo r
      rate = case path of
        "/1" -> mkRate 5 (4, 1)
        "/2" -> infRate
        _ -> mkRate 1 (1, 1)
  return (path, rate)

launchApp :: IO ThreadId
launchApp = do
  app <- testApp testKeyFunc
  forkIO $ Warp.run 11222 app

tearDownApp :: ThreadId -> IO ()
tearDownApp = killThread

withApp :: IO () -> IO ()
withApp action = bracket launchApp tearDownApp (const action)

mkReq :: Manager -> String -> IO Int
mkReq mgr path = do
  req <- parseRequest $ "http://localhost:11222" <> path
  resp <- httpLbs req mgr
  return $ statusCode $ Client.responseStatus resp

spec :: Spec
spec = do
  around_ withApp $ do
    describe "rateLimitMiddleware" $ do
      context "when bursting initially" $ do
        it "succeeds when burst is not exceeded" $ do
          mgr <- newManager defaultManagerSettings
          rs <- replicateM 3 $ mkReq mgr "/1"
          rs `shouldBe` [200, 200, 200]

        it "fails when burst is exceeded" $ do
          mgr <- newManager defaultManagerSettings
          rs <- replicateM 6 $ mkReq mgr "/1"
          rs `shouldBe` [200, 200, 200, 200, 200, 429]

      context "after initial burst" $ do
        it "succeeds when requests slow down" $ do
          mgr <- newManager defaultManagerSettings
          burstResp <- replicateM 5 $ mkReq mgr "/1"
          burstResp `shouldBe` replicate 5 200

          rs <- replicateM 4 $ do
            threadDelay 260000
            mkReq mgr "/1"
          rs `shouldBe` replicate 4 200

        it "fails when requests do not slow down enough" $ do
          mgr <- newManager defaultManagerSettings
          burstResp <- replicateM 5 $ mkReq mgr "/1"
          burstResp `shouldBe` replicate 5 200

          rs <- replicateM 4 $ do
            threadDelay 200000
            mkReq mgr "/1"
          rs `shouldBe` [429, 200, 200, 200]

      context "with infRate" $ do
        it "requests never fail" $ do
          mgr <- newManager defaultManagerSettings
          rs <- replicateM 10 $ mkReq mgr "/2"
          rs `shouldBe` replicate 10 200
