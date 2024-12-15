{-# LANGUAGE UndecidableInstances #-}

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
  , withResource
  , GenDefault (..)
  , genDefaultTag
  , genDefaultIntegral
  , genDefaultEnum
  , genDefaultList
  , genDefaultString
  , genDefaultGeneric
  , Std
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Exts (IsList (..), IsString (..))
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), (:*:) (..), (:+:) (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import Hedgehog
  ( DiscardLimit
  , Gen
  , MonadTest
  , Property
  , PropertyT
  , Range
  , ShrinkLimit
  , ShrinkRetries
  , TestLimit
  , assert
  , forAll
  , property
  , withDiscards
  , withRetries
  , withShrinks
  , withTests
  , (/==)
  , (===)
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Environment (lookupEnv, setEnv)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Test.Tasty (DependencyType (..), TestName, TestTree, after, defaultMain, testGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)

unitProperty :: PropertyT IO () -> Property
unitProperty =
  withTests (1 :: TestLimit)
    . withDiscards (1 :: DiscardLimit)
    . withShrinks (0 :: ShrinkLimit)
    . withRetries (0 :: ShrinkRetries)
    . property

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

class GenDefault tag a where
  -- | Default generator for @a@
  --
  -- The type-level @tag@ allows types @a@ to have multiple defaults.
  genDefault :: Proxy tag -> Gen a

-- | DerivingVia wrapper for types with default instances under other tags
newtype ViaTag tag' a = ViaTag {unViaTag :: a}

instance (GenDefault tag' a) => GenDefault tag (ViaTag tag' a) where
  genDefault _ = fmap ViaTag (genDefault @tag' Proxy)

genDefaultTag :: forall tag a tag'. (GenDefault tag' a) => Proxy tag' -> Proxy tag -> Gen a
genDefaultTag _ _ = fmap (unViaTag @tag' @a) (genDefault (Proxy @tag))

-- | DerivingVia wrapper for Integral types
newtype ViaIntegral a = ViaIntegral {unViaIntegral :: a}

instance (Integral a, Bounded a) => GenDefault tag (ViaIntegral a) where
  genDefault _ = fmap ViaIntegral (Gen.integral (Range.constantFrom 0 minBound maxBound))

genDefaultIntegral :: forall tag a. (Integral a, Bounded a) => Proxy tag -> Gen a
genDefaultIntegral _ = fmap (unViaIntegral @a) (genDefault (Proxy @tag))

-- | DerivingVia wrapper for Enum types
newtype ViaEnum a = ViaEnum {unViaEnum :: a}

instance (Enum a, Bounded a) => GenDefault tag (ViaEnum a) where
  genDefault _ = fmap ViaEnum Gen.enumBounded

genDefaultEnum :: forall tag a. (Enum a, Bounded a) => Proxy tag -> Gen a
genDefaultEnum _ = fmap (unViaEnum @a) (genDefault (Proxy @tag))

-- | DerivingVia wrapper for FromList types
newtype ViaList a (mn :: Nat) (mx :: Nat) = ViaList {unViaList :: a}

instance (IsList a, GenDefault tag (Item a), KnownNat mn, KnownNat mx) => GenDefault tag (ViaList a mn mx) where
  genDefault p =
    let bn = fromInteger (natVal (Proxy @mn))
        bx = fromInteger (natVal (Proxy @mx))
    in  fmap (ViaList . fromList) (Gen.list (Range.constant bn bx) (genDefault p))

genDefaultList
  :: forall tag a mn mx
   . (IsList a, KnownNat mn, KnownNat mx, GenDefault tag (Item a))
  => Proxy mn
  -> Proxy mx
  -> Proxy tag
  -> Gen a
genDefaultList _ _ _ = fmap (unViaList @a @mn @mx) (genDefault (Proxy @tag))

-- | DerivingVia wrapper for FromString types
newtype ViaString s (mn :: Nat) (mx :: Nat) = ViaString {unViaString :: s}

instance (IsString s, GenDefault tag Char, KnownNat mn, KnownNat mx) => GenDefault tag (ViaString s mn mx) where
  genDefault p =
    let bn = fromInteger (natVal (Proxy @mn))
        bx = fromInteger (natVal (Proxy @mx))
    in  fmap (ViaString . fromString) (Gen.list (Range.constant bn bx) (genDefault p))

genDefaultString
  :: forall tag a mn mx
   . (IsString a, KnownNat mn, KnownNat mx, GenDefault tag Char)
  => Proxy mn
  -> Proxy mx
  -> Proxy tag
  -> Gen a
genDefaultString _ _ _ = fmap (unViaString @a @mn @mx) (genDefault (Proxy @tag))

class GGenDefault tag f where
  ggenDefault :: Proxy tag -> Gen (f a)

instance GGenDefault tag U1 where
  ggenDefault _ = pure U1

instance (GGenDefault tag a) => GGenDefault tag (M1 i c a) where
  ggenDefault = fmap M1 . ggenDefault

instance (GGenDefault tag a, GGenDefault tag b) => GGenDefault tag (a :*: b) where
  ggenDefault p = liftA2 (:*:) (ggenDefault p) (ggenDefault p)

instance (GGenDefault tag a, GGenDefault tag b) => GGenDefault tag (a :+: b) where
  ggenDefault p = fmap L1 (ggenDefault p) <|> fmap R1 (ggenDefault p)

instance (GenDefault tag a) => GGenDefault tag (K1 i a) where
  ggenDefault = fmap K1 . genDefault

-- | DerivingVia wrapper for Generic types
newtype ViaGeneric tag a = ViaGeneric {unViaGeneric :: a}

instance (Generic a, GGenDefault tag (Rep a)) => GenDefault tag (ViaGeneric tag a) where
  genDefault = fmap (ViaGeneric . to) . ggenDefault

genDefaultGeneric :: forall tag a. (Generic a, GGenDefault tag (Rep a)) => Proxy tag -> Gen a
genDefaultGeneric _ = fmap (unViaGeneric @tag @a) (genDefault (Proxy @tag))

-- | Type tag for these "standard" default generators.
-- You can use this tag directly or choose type-by-type with 'ViaTag'.
data Std

instance GenDefault Std () where genDefault = genDefaultEnum

instance GenDefault Std Bool where genDefault = genDefaultEnum

instance GenDefault Std Char where genDefault = genDefaultEnum

instance GenDefault Std Int where genDefault = genDefaultIntegral

instance GenDefault Std Int8 where genDefault = genDefaultIntegral

instance GenDefault Std Int16 where genDefault = genDefaultIntegral

instance GenDefault Std Int32 where genDefault = genDefaultIntegral

instance GenDefault Std Int64 where genDefault = genDefaultIntegral

instance GenDefault Std Word where genDefault = genDefaultIntegral

instance GenDefault Std Word8 where genDefault = genDefaultIntegral

instance GenDefault Std Word16 where genDefault = genDefaultIntegral

instance GenDefault Std Word32 where genDefault = genDefaultIntegral

instance GenDefault Std Word64 where genDefault = genDefaultIntegral

instance (GenDefault Std a) => GenDefault Std (Maybe a) where genDefault = genDefaultGeneric

instance (GenDefault Std a, GenDefault Std b) => GenDefault Std (Either a b) where genDefault = genDefaultGeneric

instance (GenDefault Std a, GenDefault Std b) => GenDefault Std (a, b) where genDefault = genDefaultGeneric

instance (GenDefault Std a, GenDefault Std b, GenDefault Std c) => GenDefault Std (a, b, c) where
  genDefault = genDefaultGeneric

instance (GenDefault Std a, GenDefault Std b, GenDefault Std c, GenDefault Std d) => GenDefault Std (a, b, c, d) where
  genDefault = genDefaultGeneric

instance
  (GenDefault Std a, GenDefault Std b, GenDefault Std c, GenDefault Std d, GenDefault Std e)
  => GenDefault Std (a, b, c, d, e)
  where
  genDefault = genDefaultGeneric
