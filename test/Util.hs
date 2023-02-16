module Util where

import Control.Monad (void)
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Redis as R
import LaunchDarkly.AesonCompat (KeyMap, emptyObject, insertKey, singleton)
import LaunchDarkly.Server.Store (PersistentDataStore (..), SerializedItemDescriptor (..))
import LaunchDarkly.Server.Store.Redis (makeRedisStore, makeRedisStoreConfig, redisConfigSetNamespace)

defaultPrefix :: T.Text
defaultPrefix = "launchdarkly-test-prefix"

emptyData :: KeyMap (KeyMap SerializedItemDescriptor)
emptyData =
    emptyObject
        & insertKey "features" emptyObject
        & insertKey "segments" emptyObject

initialFlag :: SerializedItemDescriptor
initialFlag = SerializedItemDescriptor {version = 1, deleted = False, item = Just $ T.encodeUtf8 "flag"}

initialSegment :: SerializedItemDescriptor
initialSegment = SerializedItemDescriptor {version = 1, deleted = False, item = Just $ T.encodeUtf8 "segment"}

initialData :: KeyMap (KeyMap SerializedItemDescriptor)
initialData =
    emptyObject
        & insertKey "features" (singleton "first-flag" initialFlag)
        & insertKey "segments" (singleton "first-segment" initialSegment)

makeDefaultRedisBackend :: T.Text -> IO PersistentDataStore
makeDefaultRedisBackend prefix = do
    con <- R.checkedConnect R.defaultConnectInfo
    void $ R.runRedis con R.flushall
    makeRedisStore $ redisConfigSetNamespace prefix $ makeRedisStoreConfig con
