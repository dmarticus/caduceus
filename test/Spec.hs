{-# LANGUAGE OverloadedStrings #-}

import Wires.IBAN
import Test.Tasty
import Test.Tasty.HUnit

import qualified ExampleIBANs as I

main :: IO ()
main = defaultMain $ testGroup "all tests"
  [ testGroup "IBAN Registry Examples validate" registryTests
  ]

registryTests :: [TestTree]
registryTests = map mkTestCase I.examples
  where
    mkTestCase ex = testCase ("iban " ++ show ex) $ assertRight (parseIBAN ex)

assertRight :: Show a => Either a b -> Assertion
assertRight (Right _) = return ()
assertRight (Left a)  = assertFailure $ show a
