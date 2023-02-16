module Spec.Initialization (allTests) where

import LaunchDarkly.Server.Store (PersistentDataStore (..))
import Test.HUnit
import Util (defaultPrefix, emptyData, initialData, makeDefaultRedisBackend)

testClientIsNotInitializedByDefault :: Test
testClientIsNotInitializedByDefault = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    isInitialized <- persistentDataStoreIsInitialized backend

    assertEqual "" (Right False) isInitialized

testClientCanBeMarkedAsInitialized :: Test
testClientCanBeMarkedAsInitialized = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    initResult <- persistentDataStoreInitialize backend initialData
    isInitialized <- persistentDataStoreIsInitialized backend

    assertEqual "" (Right ()) initResult
    assertEqual "" (Right True) isInitialized

testClientCanSeeInitializiationFromAnotherClient :: Test
testClientCanSeeInitializiationFromAnotherClient = TestCase $ do
    activeBackend <- makeDefaultRedisBackend defaultPrefix
    passiveBackend <- makeDefaultRedisBackend defaultPrefix
    _ <- persistentDataStoreInitialize activeBackend initialData
    isActiveBackendInitialized <- persistentDataStoreIsInitialized activeBackend
    isPassiveBackendInitialized <- persistentDataStoreIsInitialized passiveBackend

    assertEqual "" (Right True) isActiveBackendInitialized
    assertEqual "" (Right True) isPassiveBackendInitialized

testDifferentPrefixesAreIndependent :: Test
testDifferentPrefixesAreIndependent = TestCase $ do
    initializedBackend <- makeDefaultRedisBackend "initialized"
    uninitializedBackend <- makeDefaultRedisBackend "uninitialized"
    _ <- persistentDataStoreInitialize initializedBackend initialData
    isInitializedBackendInitialized <- persistentDataStoreIsInitialized initializedBackend
    isUninitializedBackendInitialized <- persistentDataStoreIsInitialized uninitializedBackend

    assertEqual "" (Right True) isInitializedBackendInitialized
    assertEqual "" (Right False) isUninitializedBackendInitialized

testInitializationReplacesAllPreviousData :: Test
testInitializationReplacesAllPreviousData = TestCase $ do
    backend <- makeDefaultRedisBackend defaultPrefix
    _ <- persistentDataStoreInitialize backend initialData
    _ <- persistentDataStoreIsInitialized backend
    flagResult <- persistentDataStoreGetFeature backend "features" "first-flag"
    segmentResult <- persistentDataStoreGetFeature backend "segments" "first-segment"

    case flagResult of Right (Just _) -> pure (); _ -> assertFailure "first-flag was not present"
    case segmentResult of Right (Just _) -> pure (); _ -> assertFailure "first-segment was not present"

    _ <- persistentDataStoreInitialize backend emptyData
    flagResult' <- persistentDataStoreGetFeature backend "features" "first-flag"
    segmentResult' <- persistentDataStoreGetFeature backend "segments" "first-segment"

    case flagResult' of Right Nothing -> pure (); _ -> assertFailure "first-flag was not present"
    case segmentResult' of Right Nothing -> pure (); _ -> assertFailure "first-segment was not present"

allTests :: Test
allTests =
    TestList
        [ testClientIsNotInitializedByDefault
        , testClientCanBeMarkedAsInitialized
        , testClientCanSeeInitializiationFromAnotherClient
        , testDifferentPrefixesAreIndependent
        , testInitializationReplacesAllPreviousData
        ]
