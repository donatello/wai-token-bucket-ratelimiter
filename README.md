# wai-token-bucket-ratelimiter: WAI middleware to rate limit requests

This package provides a WAI middleware to perform request rate limiting using
the  [Token Bucket Algorithm](https://en.m.wikipedia.org/wiki/Token_bucket).

## Quickstart

Adding rate limiting to your WAI app is as simple as:

``` haskell
rateLimitedApp :: (Request -> IO (ByteString, Rate)) -> Application -> IO Application
rateLimitedApp keyFunc = do
  rateLimitSettings <- newRateLimitSettings keyFunc
  return $ rateLimitMiddleware rateLimitSettings app
```

A rate is specified with the `mkRate` function that takes two parameters. These are:
- **burst amount**: number of requests to allow initially and after a period of inactivity, and
- **averate rate**: for example 5 requests every 2 seconds, specified as a pair (5, 2)

The special `infRate` value specified an infinite rate or rather no rate limit.

These are specified by your "key function":

``` haskell
keyFunc :: Request -> IO (ByteString, Rate)
keyFunc r = do
  let path = rawPathInfo r
      rate = case path of
        "/1" -> mkRate 5 (4, 1) -- burst=5, avg rate of 4 reqs per 1 second
        "/2" -> infRate
        _ -> mkRate 1 (1, 1) -- burst=1, avg rate of 1 req per 1 second
  return (path, rate)
```

In a real world case, the key function would most likely dependent on the client IP and the API endpoint.
