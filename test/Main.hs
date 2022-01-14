module Main (main) where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import PropUnit (TestLimit, TestTree, forAll, testGroup, testMain, testProp, testUnit, (===))

testAsUnit :: TestTree
testAsUnit = testUnit "as unit" $ do
    let actual = (1 + 1) :: Int
        expected = 2 :: Int
    actual === expected

testAsProp :: TestLimit -> TestTree
testAsProp lim = testProp "as gen" lim $ do
  x <- forAll (Gen.int (Range.constant 1 10))
  abs x * abs x === x * x

main :: IO ()
main = testMain $ \lim ->
  testGroup "PropUnit"
    [ testAsUnit
    , testAsProp lim
    ]
