module Spec.Upsert (allTests) where

import Data.ByteString.Char8 (pack)
import Data.Char (toLower)
import GHC.Natural (Natural)
import qualified LaunchDarkly.Server.Store as Store
import Test.HUnit
import Text.Printf
import Util (defaultPrefix, emptyData, makeDefaultRedisBackend)

createTestFlag :: Natural -> Bool -> Store.SerializedItemDescriptor
createTestFlag version deleted =
    Store.SerializedItemDescriptor
        { Store.item = Just $ pack $ printf "{\"deleted\":%s,\"version\":%d}" (map toLower $ show deleted) version
        , Store.version = version
        , Store.deleted = deleted
        }

testHandlesVersionsAppropriately :: Natural -> Natural -> Natural -> Test
testHandlesVersionsAppropriately initialVersion secondVersion expectedVersion = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    _ <- Store.persistentDataStoreInitialize backend emptyData
    _ <- Store.persistentDataStoreUpsertFeature backend "flags" "first-flag" $ createTestFlag initialVersion False
    _ <- Store.persistentDataStoreUpsertFeature backend "flags" "first-flag" $ createTestFlag secondVersion False
    flag <- Store.persistentDataStoreGetFeature backend "flags" "first-flag"

    assertEqual "" (Right $ Just expectedFlag) flag
  where
    expectedFlag = (createTestFlag expectedVersion False) {Store.version = 0, Store.deleted = False}

testUpsertionAfterDeletionHandlesVersionsAppropriately :: Natural -> Natural -> Natural -> Bool -> Test
testUpsertionAfterDeletionHandlesVersionsAppropriately initialVersion secondVersion expectedVersion expectedDeletionStatus = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    _ <- Store.persistentDataStoreInitialize backend emptyData
    _ <- Store.persistentDataStoreUpsertFeature backend "flags" "first-flag" $ createTestFlag initialVersion True
    _ <- Store.persistentDataStoreUpsertFeature backend "flags" "first-flag" $ createTestFlag secondVersion False
    flag <- Store.persistentDataStoreGetFeature backend "flags" "first-flag"

    assertEqual "" (Right $ Just expectedFlag) flag
  where
    expectedFlag = (createTestFlag expectedVersion expectedDeletionStatus) {Store.version = 0, Store.deleted = False}

testUpsertionToADeletionHandlesVersionsAppropriately :: Natural -> Natural -> Natural -> Bool -> Test
testUpsertionToADeletionHandlesVersionsAppropriately initialVersion secondVersion expectedVersion expectedDeletionStatus = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    _ <- Store.persistentDataStoreInitialize backend emptyData
    _ <- Store.persistentDataStoreUpsertFeature backend "flags" "first-flag" $ createTestFlag initialVersion False
    _ <- Store.persistentDataStoreUpsertFeature backend "flags" "first-flag" $ createTestFlag secondVersion True
    flag <- Store.persistentDataStoreGetFeature backend "flags" "first-flag"

    assertEqual "" (Right $ Just expectedFlag) flag
  where
    expectedFlag = (createTestFlag expectedVersion expectedDeletionStatus) {Store.version = 0, Store.deleted = False}

allTests :: Test
allTests =
    TestList
        [ TestLabel "Newer versions replace older" $ testHandlesVersionsAppropriately 1 2 2
        , TestLabel "Older versions do not replace newer" $ testHandlesVersionsAppropriately 2 1 2
        , TestLabel "Newer versions replace older" $ testUpsertionAfterDeletionHandlesVersionsAppropriately 1 2 2 False
        , TestLabel "Older versions do not replace newer" $ testUpsertionAfterDeletionHandlesVersionsAppropriately 2 1 2 True
        , TestLabel "Newer versions replace older" $ testUpsertionToADeletionHandlesVersionsAppropriately 1 2 2 True
        , TestLabel "Older versions do not replace newer" $ testUpsertionToADeletionHandlesVersionsAppropriately 2 1 2 False
        ]
