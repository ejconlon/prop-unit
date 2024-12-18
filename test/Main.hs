{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Monad (replicateM)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import GHC.Exts (IsList, IsString)
import GHC.Generics (Generic)
import PropUnit
  ( GenDefault (..)
  , StdTag
  , TestLimit
  , TestTree
  , assert
  , forAll
  , genDefaultEnum
  , genDefaultGeneric
  , genDefaultList
  , genDefaultString
  , genDefaultTag
  , testGroup
  , testMain
  , testProp
  , testUnit
  , (===)
  )
import PropUnit.Hedgehog (Gen)
import qualified PropUnit.Hedgehog.Gen as Gen
import qualified PropUnit.Hedgehog.Range as Range

testAsUnit :: TestTree
testAsUnit = testUnit "as unit" $ do
  let actual = (1 + 1) :: Int
      expected = 2 :: Int
  actual === expected

testAsProp :: TestLimit -> TestTree
testAsProp lim = testProp "as gen" lim $ do
  x <- forAll (Gen.int (Range.constant 1 10))
  abs x * abs x === x * x

testBasic :: TestLimit -> TestTree
testBasic lim =
  testGroup
    "basic"
    [ testAsUnit
    , testAsProp lim
    ]

data Tag

instance GenDefault Tag Int where genDefault = genDefaultTag (Proxy @StdTag)

instance GenDefault Tag Char where genDefault = genDefaultTag (Proxy @StdTag)

newtype AList a = AList [a]
  deriving newtype (Eq, Ord, Show, IsList)

instance (GenDefault Tag a) => GenDefault Tag (AList a) where genDefault = genDefaultList (Proxy @0) (Proxy @2)

newtype AString = AString String
  deriving newtype (Eq, Ord, Show, IsString)

instance GenDefault Tag AString where genDefault = genDefaultString (Proxy @0) (Proxy @2)

data Choice = ChoiceA | ChoiceB
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance GenDefault Tag Choice where genDefault = genDefaultEnum

instance (GenDefault Tag a) => GenDefault Tag (Maybe a) where genDefault = genDefaultGeneric

data Record = Record !Int !(Maybe Record)
  deriving stock (Eq, Ord, Show, Generic)

instance GenDefault Tag Record where genDefault = genDefaultGeneric

data GenCase where
  GenCase :: (Ord a) => String -> Gen a -> GenCase

genDefaultByProxy :: (GenDefault Tag a) => Proxy a -> Gen a
genDefaultByProxy _ = genDefault (Proxy @Tag)

mkGenCase :: (Ord a, GenDefault Tag a) => String -> Proxy a -> GenCase
mkGenCase name = GenCase name . genDefaultByProxy

genCases :: [GenCase]
genCases =
  [ mkGenCase "Int" (Proxy @Int)
  , mkGenCase "Char" (Proxy @Char)
  , mkGenCase "Choice" (Proxy @Choice)
  , mkGenCase "AList" (Proxy @(AList Char))
  , mkGenCase "AString" (Proxy @AString)
  , mkGenCase "Record" (Proxy @Record)
  ]

testGenCase :: GenCase -> TestTree
testGenCase (GenCase name gen) = testUnit name $ do
  xs <- fmap Set.fromList (replicateM 10 (Gen.sample gen))
  assert (Set.size xs > 1)

testDerive :: TestTree
testDerive = testGroup "derive" (fmap testGenCase genCases)

main :: IO ()
main = testMain $ \lim ->
  testGroup
    "PropUnit"
    [ testBasic lim
    , testDerive
    ]
