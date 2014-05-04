module Main (main) where

import System.Environment
import Data.Map(Map)
import Data.Function(fix)
import qualified Data.Map as Map

main :: IO ()
main = do
   args <- getArgs
   case args of
      [] -> putStrLn "Enter a problem number to run."
      x : _ -> Map.findWithDefault (error $ "No problem: " ++ x) (read x) problems

problems :: Map Int (IO ())
problems = Map.fromList [(1, p1), (2, p2)]

-- | Find the sum of all the multiples of 3 or 5 below 1000.
p1 :: IO ()
p1 = putStrLn $ show $ sum [x | x <- [3 .. 999], x `mod` 3 == 0 || x `mod` 5 == 0]


p2 :: IO ()
p2 = putStrLn $ show $ sum $ takeWhile ((>) 4000000) $ filter (\ x -> x `mod` 2 == 0) $ take 40 fibs
   where
   fibs = fix ( (1 :) . scanl (+) 2 )