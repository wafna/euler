{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import System.Environment
import Data.Function(fix)
import Data.Maybe
import Data.Array
import qualified Data.List as List

import System.IO.Unsafe
import Debug.Trace

-- This allows our problems to return anything we can print to the screen.
data Showable = forall a . Show a => Showable a

main :: IO ()
main = do
   args <- getArgs
   putStrLn $ show args
   case args of
      [] -> putStrLn "Enter a problem number to run."
      x : _ -> printResult $ (listArray (1, length problems) problems) ! (read x)
   where
   printResult (Showable a) = putStrLn $ show a

problems :: [Showable]
problems = 
      [ s p1, s p2, s p3, s p4, s p5, s p6, s p7, s p8, s p9, s p10
      , s p11, s p12, s p13, s p14, s p15, s p16, u 17, s p18, u 19, s p20
      , s p21, u 22, s p23
      ]
   where
   s :: Show a => a -> Showable
   s = Showable
   u :: Int -> a
   u x = error $ "No problem: " ++ show x -- in case I skip a problem


-- Utilities

-- | This lets us shove nasty huge blocks of data off to files (and speed up the compile considerably).
readData :: (Read a) => FilePath -> a
readData = read . unsafePerformIO . readFile


factorial :: Integer -> Integer
factorial n = if n==0 then 1 else n * factorial(n-1)

properDivisors :: Integer -> [Integer]
properDivisors n = 1 : filter (\ v -> 0 == n `mod` v) [2 .. n `div` 2]

-- | Returns LT if the number is deficient, EQ if the number is perfect, and GT if the number is abundant.
perfection :: Integer -> Ordering
perfection n = (sum $ properDivisors n) `compare` n

-- | http://www.haskell.org/haskellwiki/Prime_numbers
primesToG :: Integer -> [Integer]
primesToG m = 2 : sieve [3,5..m]  where
   sieve (p:xs) 
      | p*p > m   = p : xs
      | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])
   sieve [] = error "Ceci n'est ce pas une pipe."

primesPE :: [Integer]
primesPE = 2 : primes'
   where
   primes' = sieve [3,5..] 9 primes'
   sieve :: [Integer] -> Integer -> [Integer] -> [Integer]
   sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^(2::Integer)) t
   sieve [] _ _ = error "Ceci n'est ce pas une pipe."

minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

--union :: Ord a => [a] -> [a] -> [a]
--union (x:xs) (y:ys) = case (compare x y) of 
--           LT -> x : union  xs  (y:ys)
--           EQ -> x : union  xs     ys 
--           GT -> y : union (x:xs)  ys
--union  xs     []    = xs
--union  []     ys    = ys

-- PROBLEMS

-- | Find the sum of all the multiples of 3 or 5 below 1000.
p1 :: Integer
p1 = sum [x :: Integer | x <- [3 .. 999], x `mod` 3 == 0 || x `mod` 5 == 0]

-- | By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
p2 :: Integer
p2 = sum $ takeWhile ((>) (4000000 :: Integer)) $ filter (\ x -> x `mod` 2 == 0) $ take 40 fibs
   where
   fibs = fix ( (1 :) . scanl (+) 2 )

-- | What is the largest prime factor of the number 600851475143?
p3 :: Integer
p3 = do
   fromJust $ testPrime Nothing $ primesToG root
   where
   target = 600851475143
   root = floor $ ((sqrt $ fromInteger target) :: Double)
   testPrime v ps = case ps of
      [] -> v
      (p : s) -> if target `mod` p == 0 then testPrime (Just p) s else testPrime v s

-- | Find the largest palindrome made from the product of two 3-digit numbers.
p4 :: Integer
p4 = head $ reverse $ List.sort $ filter palindrome [x * y | x <- [100..999], y <- [100..999]]
   where
   palindrome x = let s = show x in s == reverse s

-- | What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-- Here, I started with the product of [11..20] and then eliminated factors that were implied by divisibility by higher numbers.
p5 :: Integer
p5 = 20 * 19 * 9 * 17 * 4 * 7 * 13 * 11

-- | Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
p6 :: Integer
p6 = let ns = [1..100] in (sq $ sum ns) - (sum $ fmap sq ns)
   where
   sq x = x * x

-- | What is the 10 001st prime number?
p7 :: Integer
p7 = head $ drop 10000 $ primesPE

-- | Find the greatest product of five consecutive digits in the 1000-digit number.
p8 :: Int
p8 = let s = fmap (\ c -> read [c]) $ concat $ readData "p8.txt"
   in
   sift 0 s
   where
   sift v (a:t@(b:c:d:e:_)) = sift (max v (a * b * c * d * e)) t
   sift v _ = v

-- | There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
p9 :: Int
p9 = (\(a,b,c) -> a * b * c) $ head [(a,b,c) | a <- [1..1000], b <- [1..1000], c <- [1..1000], a * a + b * b == c * c, a + b + c == 1000]

-- | Find the sum of all the primes below two million.
p10 :: Integer
p10 = sum $ primesToG 2000000

-- | What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
p11 :: Int
p11 = 
   let 
      allIx = [(x,y) | x <- [1..(20::Int)], y <- [1..(20::Int)]]
      grid = array ((1,1),(20,20)) $ zip allIx
               [ 08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08
               , 49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00
               , 81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65
               , 52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91
               , 22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80
               , 24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50
               , 32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70
               , 67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21
               , 24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72
               , 21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95
               , 78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92
               , 16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57
               , 86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58
               , 19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40
               , 04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66
               , 88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69
               , 04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36
               , 20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16
               , 20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54
               , 01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48
               ] 
      evalVector v = product $ fmap (\ p -> grid ! p) v
   in
   maximum $ (flip map) allIx $ \ ix -> 
      let 
         allVectors = map (\ f -> f ix) [ve, vs, vse, vne]
         -- some vectors go off the edge
         validVectors = filter (all (\ (x,y) -> x > 0 && x < 21 && y > 0 && y < 21)) allVectors
         evaluated = map evalVector validVectors
      in
      case evaluated of
         [] -> 0
         _ -> maximum evaluated
   where 
   vector p f = map f $ zip [0..3] $ replicate 4 p 
   ve p = vector p $ \ (o, (x,y)) -> (x, y+o)
   vs p = vector p $ \ (o, (x,y)) -> (x+o, y)
   vse p = vector p $ \ (o, (x,y)) -> (x+o, y+o)
   vne p = vector p $ \ (o, (x,y)) -> (x-o, y+o)

-- | What is the value of the first triangle number to have over five hundred divisors?
p12 :: Integer
p12 = head $ dropWhile (\ n -> (500 :: Integer) > aliquots n) $ triangles 3 6
   where
   triangles n s = s : (triangles (n + 1) (n + s + 1))
   -- the number of aliquot divisors
   aliquots x =
      let m = floor $ ((sqrt $ fromInteger x) :: Double) in
      foldl (\ s f -> s + if 0 == x `mod` f then 2 else 0) 1 [2..m]

-- | Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
-- NB converting each number to a Double would be leaner and a Double has more than enough precision to get us the first ten digits.
p13 :: Integer
p13 = 
   let
      nums :: [Integer]
      nums = readData "p13.txt"
   in
   read $ take 10 $ show $ sum nums

-- | Which starting number, under one million, produces the longest Collatz sequence?
p14 :: Integer
p14 = snd $ List.maximumBy (\ x y -> fst x `compare` fst y) $ map (\n -> (collatz 1 n, n)) [2..999999]
   where
   collatz :: Integer -> Integer -> Integer
   collatz s n 
      | n == 1         = s
      | 0 == n `mod` 2 = collatz (s + 1) (n `div` 2)
      | otherwise      = collatz (s + 1) (3 * n + 1)

-- | How many such routes are there through a 20×20 grid?
-- This is just a Bernoulli problem where we're looking for the number of ways to flip 40 fair coins and get exactly half heads and half tails.
p15 :: Integer
p15 = (factorial 40) `div` (factorial 20 * factorial 20)

-- | What is the sum of the digits of the number 21000?
p16 :: Integer
p16 = sum $ map (\c -> read [c]) $ show $ (2 :: Integer)^(1000 :: Integer)

p18 :: Int
p18 = 
   let
      triangle = 
         [ [ 75 ]
         , [ 95, 64 ]
         , [ 17, 47, 82 ]
         , [ 18, 35, 87, 10 ]
         , [ 20, 04, 82, 47, 65 ]
         , [ 19, 01, 23, 75, 03, 34 ]
         , [ 88, 02, 77, 73, 07, 63, 67 ]
         , [ 99, 65, 04, 28, 06, 16, 70, 92 ]
         , [ 41, 41, 26, 56, 83, 40, 80, 70, 33 ]
         , [ 41, 48, 72, 33, 47, 32, 37, 16, 94, 29 ]
         , [ 53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14 ]
         , [ 70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57 ]
         , [ 91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48 ]
         , [ 63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31 ]
         , [ 04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23 ]
         ]
   in
   -- fold right is key, here.
   head $ foldr1 combine triangle
   where
   -- combine two rows by adding in the larger adjacent value from the second row into each value in the first row
   combine :: [Int] -> [Int] -> [Int]
   combine xs ys = map (\ (x, p) -> x + (uncurry max) p) $ zip xs (pairwise ys)
   -- turns a row into lists of the pairs reachable from each corresponding element of the row above it.
   pairwise :: [a] -> [(a,a)]
   pairwise xs = case xs of
      p : t@(q : _) -> (p,q) : pairwise t
      _ -> []

-- | Find the sum of the digits in the number 100!
p20 :: Int
p20 = sum $ map (\ c -> read [c]) $ show $ factorial 100

-- | Evaluate the sum of all the amicable numbers under 10000.
p21 :: Integer
p21 = 
   let t = 10000 in
   -- ensure a /= b or we'll pick up the perfect numbers.
   sum $ map (uncurry (+)) $ filter (uncurry amicable) [(a,b) | a <- [2..(t-1)], b <- [(a+1)..t]]
   where
   amicable p q = (sum $ properDivisors p) == q && (sum $ properDivisors q) == p

-- | Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
-- All integers greater than 28123 can be written as the sum of two abundant numbers.
p23 :: Int
p23 = 
   let ubound = 21823 in
   length $ filter ((==) GT) $ map perfection [12..ubound]
   
