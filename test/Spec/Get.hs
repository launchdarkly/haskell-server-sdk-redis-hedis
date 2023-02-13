module Spec.Get (allTests) where

import Data.Maybe (fromJust)
import LaunchDarkly.AesonCompat (lookupKey, mapValues)
import LaunchDarkly.Server.Store (PersistentDataStore (..), SerializedItemDescriptor (..))
import Test.HUnit
import Util (defaultPrefix, initialData, initialFlag, makeDefaultRedisBackend)

testCanExistingItem :: Test
testCanExistingItem = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    _ <- persistentDataStoreInitialize backend initialData
    flag <- persistentDataStoreGetFeature backend "flags" "first-flag"

    assertEqual "" (Right $ Just expectedFlag) flag
  where
    expectedFlag = initialFlag {version = 0, deleted = False}

testHandlesNonExistentData :: Test
testHandlesNonExistentData = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    _ <- persistentDataStoreInitialize backend initialData
    flag <- persistentDataStoreGetFeature backend "flags" "does-not-exist"

    assertEqual "" (Right Nothing) flag

testAllFlagsReturnsEveryFlag :: Test
testAllFlagsReturnsEveryFlag = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    _ <- persistentDataStoreInitialize backend initialData
    flags <- persistentDataStoreAllFeatures backend "flags"

    assertEqual "" (Right expectedFlags) flags
  where
    expectedFlags = mapValues (\flag -> flag {version = 0, deleted = False}) $ fromJust $ lookupKey "flags" initialData

allTests :: Test
allTests =
    TestList
        [ testCanExistingItem
        , testHandlesNonExistentData
        , testAllFlagsReturnsEveryFlag
        ]
