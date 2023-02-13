{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (void, when)
import Test.HUnit (Counts (..), Test (TestLabel, TestList), runTestTT)

import qualified Spec.Initialization

import System.Exit (ExitCode (ExitFailure), exitWith)

main :: IO ()
main = do
    Counts {..} <-
        runTestTT $
            TestList
                [ TestLabel "Initialization" Spec.Initialization.allTests
                ]
    when (errors + failures > 0) $ exitWith (ExitFailure 1)
