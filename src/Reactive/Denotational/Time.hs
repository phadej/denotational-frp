module Reactive.Denotational.Time (Time(..), maxTime, moments) where

import Control.Applicative
import Test.QuickCheck

newtype Time = Time { unTime :: Integer }
  deriving (Eq, Ord, Show, Read)

instance Arbitrary Time where
  arbitrary = (Time . getNonNegative) <$> arbitrary
  shrink (Time t) = Time . getNonNegative <$> shrink (NonNegative t)

maxTime :: Time -> Time -> Time
maxTime (Time a) (Time b) = Time $ max a b

moments :: [Time]
moments = map Time [0..]