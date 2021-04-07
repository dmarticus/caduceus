{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Arrow
import Data.Either
import Main
import Test.Tasty
import Test.Tasty.HUnit

import qualified IBANRegistryExamples as R
import qualified Data.Text as T

main :: IO ()
main = defaultMain $ testGroup "all tests"
  [ testGroup "IBAN Registry Examples validate" registryTests
  , testGroup "German legacy account transformation" germanLegacyTests
  ]

registryTests = map mkTestCase R.examples
  where
    mkTestCase ex = testCase ("iban " ++ show ex) $ assertRight (parseIBAN ex)

assertRight :: Show a => Either a b -> Assertion
assertRight (Right _) = return ()
assertRight (Left a)  = assertFailure $ show a
