{-# LANGUAGE OverloadedStrings #-}

import Wires.IBAN
import Wires.SWIFT
import Test.Tasty
import Test.Tasty.HUnit

import qualified ExampleIBANs as I
import qualified ExampleSWIFTs as S

main :: IO ()
main = defaultMain $ testGroup "all tests"
  [ testGroup "IBAN Examples validate" ibanTests
  , testGroup "SWIFT Example validate" swiftTests
  ]

ibanTests :: [TestTree]
ibanTests = map mkTestCase I.examples
  where
    mkTestCase ex = testCase ("iban " ++ show ex) $ assertRight (parseIBAN ex)

swiftTests :: [TestTree]
swiftTests = map mkTestCase S.examples
    where
      mkTestCase ex = testCase ("swift " ++ show ex) $ assertRight (parseSWIFT ex)

assertRight :: Show a => Either a b -> Assertion
assertRight (Right _) = return ()
assertRight (Left a)  = assertFailure $ show a
