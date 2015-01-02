import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Reactive.Denotational

main :: IO ()
main = defaultMain properties

properties :: TestTree
properties = testGroup "Properties" [ timeProperties
                                    -- , eventsProperties
                                    ]
timeProperties :: TestTree
timeProperties = testGroup "Time"
  [ QC.testProperty "Time 0 <= t" $ \t -> Time 0 <= t
  ]

{-
eventsProperties :: TestTree
eventsProperties = testGroup "Events"
  [ eventsMonadProperties
  ]

monadLeftIdentity :: (Monad m, Arbitrary (m Int), Eq (m Int), Show (m Int)) => m () -> Int -> (Fun Int (m Int)) -> Property
monadLeftIdentity _ x (Fun _ f) = traceShow (f x) ((return x >>= f) === f x)

monadProperties :: (Monad m, Arbitrary (m Int), Eq (m Int), Show (m Int)) => m () -> TestTree
monadProperties m = testGroup "Monad"
  [ QC.testProperty "left identity" $ monadLeftIdentity m
  ]

eventsMonadProperties :: TestTree
eventsMonadProperties = monadProperties m
  where m = return () :: Events ()
-}