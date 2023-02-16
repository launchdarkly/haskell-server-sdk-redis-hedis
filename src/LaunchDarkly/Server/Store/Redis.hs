-- | The public interface for the LaunchDarkly Haskell Redis integration
module LaunchDarkly.Server.Store.Redis
    ( RedisStoreConfig
    , makeRedisStoreConfig
    , redisConfigSetNamespace
    , makeRedisStore
    ) where

import LaunchDarkly.Server.Store.Redis.Internal
