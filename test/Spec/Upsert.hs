module Spec.Upsert (allTests) where

import Data.ByteString (isInfixOf)
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
    _ <- Store.persistentDataStoreUpsertFeature backend "features" "first-flag" $ createTestFlag initialVersion False
    _ <- Store.persistentDataStoreUpsertFeature backend "features" "first-flag" $ createTestFlag secondVersion False
    flag <- Store.persistentDataStoreGetFeature backend "features" "first-flag"

    assertEqual "" (Right $ Just expectedFlag) flag
  where
    expectedFlag = (createTestFlag expectedVersion False) {Store.version = 0, Store.deleted = False}

testUpsertionAfterDeletionHandlesVersionsAppropriately :: Natural -> Natural -> Natural -> Bool -> Test
testUpsertionAfterDeletionHandlesVersionsAppropriately initialVersion secondVersion expectedVersion expectedDeletionStatus = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    _ <- Store.persistentDataStoreInitialize backend emptyData
    _ <- Store.persistentDataStoreUpsertFeature backend "features" "first-flag" $ createTestFlag initialVersion True
    _ <- Store.persistentDataStoreUpsertFeature backend "features" "first-flag" $ createTestFlag secondVersion False
    flag <- Store.persistentDataStoreGetFeature backend "features" "first-flag"

    case flag of
        Right (Just Store.SerializedItemDescriptor {Store.item = Just item, Store.version = 0, Store.deleted = False}) -> do
            assertBool "" (expectedVersionString `isInfixOf` item)
            assertBool "" (expectedDeletionString `isInfixOf` item)
        _ -> assertFailure "failed to restore deleted file"
  where
    expectedVersionString = pack $ printf "\"version\":%d" expectedVersion
    expectedDeletionString = pack $ printf "\"deleted\":%s" (map toLower $ show expectedDeletionStatus)

testUpsertionToADeletionHandlesVersionsAppropriately :: Natural -> Natural -> Natural -> Bool -> Test
testUpsertionToADeletionHandlesVersionsAppropriately initialVersion secondVersion expectedVersion expectedDeletionStatus = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    _ <- Store.persistentDataStoreInitialize backend emptyData
    _ <- Store.persistentDataStoreUpsertFeature backend "features" "first-flag" $ createTestFlag initialVersion False
    _ <- Store.persistentDataStoreUpsertFeature backend "features" "first-flag" $ createTestFlag secondVersion True
    flag <- Store.persistentDataStoreGetFeature backend "features" "first-flag"

    case flag of
        Right (Just Store.SerializedItemDescriptor {Store.item = Just item, Store.version = 0, Store.deleted = False}) -> do
            assertBool "" (expectedVersionString `isInfixOf` item)
            assertBool "" (expectedDeletionString `isInfixOf` item)
        _ -> assertFailure "failed to restore deleted file"
  where
    expectedVersionString = pack $ printf "\"version\":%d" expectedVersion
    expectedDeletionString = pack $ printf "\"deleted\":%s" (map toLower $ show expectedDeletionStatus)

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
