{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import Test.HUnit (Counts (..), Test (TestLabel, TestList), runTestTT)

import qualified Spec.Get
import qualified Spec.Initialization
import qualified Spec.Upsert

import System.Exit (ExitCode (ExitFailure), exitWith)

main :: IO ()
main = do
    Counts {..} <-
        runTestTT $
            TestList
                [ TestLabel "Get" Spec.Get.allTests
                , TestLabel "Initialization" Spec.Initialization.allTests
                , TestLabel "Upsert" Spec.Upsert.allTests
                ]
    when (errors + failures > 0) $ exitWith (ExitFailure 1)
