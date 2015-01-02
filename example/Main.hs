module Main where

import Reactive.Denotational
import Prelude hiding (zip)

promisedF :: Time -> Char -> Events Char
promisedF (Time t) c = Events [(Time $ t + 4, c)]

zipped :: Events Char -> Events Char -> Events Char
zipped s1 s2 = mergeTimeMap f $ zip s1 s2
  where f (Time t) (a, b)  = Events [(Time t, a), (Time $ t + 1, b)]

zipExample :: Events Char -> Events Char -> IO ()
zipExample s1 s2 = do
  putStrLn "concatMap . flatMap . zip"
  putStrLn $ "s1    = " ++ pprEventsChar s1
  putStrLn $ "s2    = " ++ pprEventsChar s2
  putStrLn $ "zip   = " ++ pprEventsChar z
  putStrLn $ "      = " ++ pprEventsChar (concatTimeMap promisedF z)
  where z = zipped s1 s2

mergeExample :: Events Char -> Events Char -> IO ()
mergeExample s1 s2 = do
  putStrLn "concatMap . merge"
  putStrLn $ "s1    = " ++ pprEventsChar s1
  putStrLn $ "s2    = " ++ pprEventsChar s2
  putStrLn $ "merge = " ++ pprEventsChar m
  putStrLn $ "      = " ++ pprEventsChar (concatTimeMap promisedF m)
  where m = merge s1 s2

fairExample :: Events Char -> Events Char -> IO ()
fairExample s1 s2 = do
  putStrLn "fairScheduler"
  putStrLn $ "s1    = " ++ pprEventsChar s1
  putStrLn $ "s2    = " ++ pprEventsChar s2
  putStrLn $ "fair  = " ++ pprEventsChar (fairScheduler promisedF s1 s2)

fairScheduler :: (Time -> a -> Events b) -> Events a -> Events a -> Events b
fairScheduler f (Events xs) (Events ys) = Events $ impl (Time 0) 0 0 xs ys
  where impl t _ _ [] x = concatMapImpl t x
        impl t _ _ x [] = concatMapImpl t x
        impl t ca cb as@((ta, a):as') bs@((tb, b):bs')
          | ta < t && tb < t && ca <= cb = pickA
          | ta < t && tb < t             = pickB
          | ta < t                       = pickA
          | tb < t                       = pickB
          | ta < tb                      = pickA
          | ta > tb                      = pickB
          | ta == tb && ca < cb          = pickA
          | otherwise                    = pickB
          where pickA = let zs  = f (maxTime t ta) a
                            t'  = maxTime (maxTime t ta) (endTime zs)
                            c   = unTime t' - unTime t
                        in fromEvents zs ++ impl t' (ca + c) cb as' bs
                pickB = let zs  = f (maxTime t tb) b
                            t'  = maxTime (maxTime t tb) (endTime zs)
                            c   = unTime t' - unTime t
                        in fromEvents zs ++ impl t' ca (cb + c) as bs'
        concatMapImpl _ [] = []
        concatMapImpl t ((t', x) : es) = let zs  = f (maxTime t t') x
                                             t'' = maxTime (maxTime t t') (endTime zs)
                                         in fromEvents zs ++ concatMapImpl t'' es

main :: IO ()
main = do
  zipExample s1 s2
  mergeExample s1 s2
  fairExample s1 s2
  where s1 = toEvents [(0, 'a'), (6, 'b'), (21, 'c')]
        s2 = toEvents [(2, '1'), (4, '2'), (6, '3')]

{-
a.....b..............c.......
..1.2.3......................
-}
