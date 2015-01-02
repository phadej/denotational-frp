{-# LANGUAGE DeriveFunctor #-}

module Reactive.Denotational.Events where

import Reactive.Denotational.Time

import Control.Applicative
import Data.Monoid
import Data.Function (on)
import Data.List (sortBy)
import Test.QuickCheck

newtype Events a = Events { fromEvents :: [(Time, a)] }
  deriving (Eq, Ord, Show, Functor)

instance Monoid (Events a) where
  mempty = Events []
  Events xs `mappend` Events ys = normalizeEvents . Events $ xs ++ ys

instance Applicative Events where
  pure x = Events $ map f moments
    where f t = (t, x)
  as <*> bs = timeZipWith ($) as bs

instance Arbitrary a => Arbitrary (Events a) where
  arbitrary = toEvents <$> arbitrary
  shrink (Events es) = normalizeEvents . Events <$> shrink es

normalizeEvents :: Events a -> Events a
normalizeEvents = Events . sortBy (compare `on` fst) . fromEvents

pprEventsChar :: Events Char -> String
pprEventsChar = pprEventsChar' 0 . fromEvents
  where pprEventsChar' _ []                    = ""
        pprEventsChar' n ((Time t , x) : es)
          | n > t                              = pprEventsChar' (t + 1) es
          | otherwise                          = replicate (fromInteger $ t - n) '.' ++ [x] ++ pprEventsChar' (t + 1) es

toEvents :: [(Integer, a)] -> Events a
toEvents = normalizeEvents . Events . map f
  where f (t, a) = (Time t, a)

merge :: Events a -> Events a -> Events a
merge = mappend

mergeAll :: [Events a] -> Events a
mergeAll = mconcat

mergeMap :: (a -> Events b) -> Events a -> Events b
mergeMap f = mergeAll . map (f . snd) . fromEvents

mergeTimeMap :: (Time -> a -> Events b) -> Events a -> Events b
mergeTimeMap f = mergeAll . map (uncurry f) . fromEvents

endTime :: Events a -> Time
endTime (Events []) = Time 0 -- This is a bit hand wavy
endTime (Events es) = fst $ last es

concatTimeMap :: (Time -> a -> Events b) -> Events a -> Events b
concatTimeMap f = Events . impl (Time 0) . fromEvents
  where impl _ []             = []
        impl t ((t', x) : es) = let ys  = f (maxTime t t') x
                                    t'' = maxTime (maxTime t t') (endTime ys)
                                in fromEvents ys ++ impl t'' es

zip :: Events a -> Events b -> Events (a, b)
zip (Events as) (Events bs) = Events $ Prelude.zipWith f as bs
  where f (ta, a) (tb, b) = (maxTime ta tb , (a , b))

timeZipWith :: (a -> b -> c) -> Events a -> Events b -> Events c
timeZipWith f (Events xs) (Events ys) = Events $ impl xs ys
  where impl [] _      = []
        impl _ []      = []
        impl as@((ta, a):as') bs@((tb, b):bs')
          | ta == tb   = (ta, f a b) : impl as' bs'
          | ta < tb    = impl as' bs
          | otherwise  = impl as bs'
