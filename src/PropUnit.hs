module PropUnit
  ( DependencyType (..)
  , Gen
  , MonadTest
  , Property
  , PropertyT
  , Range
  , TestLimit
  , TestName
  , TestTree
  , (===)
  , (/==)
  , after
  , assert
  , forAll
  , testProp
  , testUnit
  , defaultTestLimit
  , setupTests
  , testGroup
  , testMain
  ) where

import Control.Monad (when)
import Hedgehog (DiscardLimit, Gen, MonadTest, Property, PropertyT, Range, ShrinkLimit, ShrinkRetries, TestLimit,
                 assert, forAll, property, withDiscards, withRetries, withShrinks, withTests, (/==), (===))
import System.Environment (lookupEnv, setEnv)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Test.Tasty (DependencyType (..), TestName, TestTree, after, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

unitProperty :: PropertyT IO () -> Property
unitProperty =
  withTests (1 :: TestLimit) .
  withDiscards (1 :: DiscardLimit) .
  withShrinks (0 :: ShrinkLimit) .
  withRetries (0 :: ShrinkRetries) .
  property

testUnit :: TestName -> PropertyT IO () -> TestTree
testUnit name = testProperty name . unitProperty

testProp :: TestName -> TestLimit -> PropertyT IO () -> TestTree
testProp name lim = testProperty name . withTests lim . property

-- 100 is Hedgehog's defaultMinTests
defaultTestLimit :: TestLimit
defaultTestLimit = 100

setupTests :: IO TestLimit
setupTests = do
  mayDebugStr <- lookupEnv "PROP_UNIT_DEBUG"
  let debug = Just "1" == mayDebugStr
  when debug $ do
    setEnv "TASTY_NUM_THREADS" "1"
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
  mayLimStr <- lookupEnv "PROP_UNIT_LIMIT"
  pure (maybe defaultTestLimit (fromInteger . read) mayLimStr)

testMain :: (TestLimit -> TestTree) -> IO ()
testMain f = do
  lim <- setupTests
  defaultMain (f lim)
