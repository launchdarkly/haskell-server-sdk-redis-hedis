module Spec.Get (allTests) where

import LaunchDarkly.Server.Store (PersistentDataStore (..), SerializedItemDescriptor (..))
import Test.HUnit
import Util (initialFlag, makeDefaultRedisBackend, defaultPrefix, initialData)
import LaunchDarkly.AesonCompat (lookupKey, mapValues)
import Data.Maybe (fromJust)

testCanExistingItem :: Test
testCanExistingItem = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    _ <- persistentDataStoreInitialize backend initialData
    flag <- persistentDataStoreGetFeature backend "flags" "first-flag"

    assertEqual "" (Right $ Just expectedFlag) flag

    where

    expectedFlag = initialFlag { version = 0, deleted = False }

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

    expectedFlags = mapValues (\flag -> flag { version = 0, deleted = False }) $ fromJust $ lookupKey "flags" initialData


allTests :: Test
allTests =
    TestList
        [ testCanExistingItem
        , testHandlesNonExistentData
        , testAllFlagsReturnsEveryFlag
        ]
