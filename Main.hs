{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import System.Environment
import Data.Function(fix)
import Data.Maybe
import Data.Array
import qualified Data.List as List

-- import Debug.Trace

-- This allows our problems to return anything we can print to the screen.
data Showable = forall a . Show a => Showable a

main :: IO ()
main = do
   putStrLn ""
   args <- getArgs
   case args of
      [] -> putStrLn "Enter a problem number to run."
      x : _ -> case (listArray (1, length problems) problems) ! (read x) of
         Nothing -> putStrLn $ "No problem: " ++ x
         Just p -> printResult p
   where
   printResult (Showable a) = putStrLn $ show a

problems :: [Maybe Showable]
problems = 
      [ s p1, s p2, s p3, s p4, s p5, s p6, s p7, s p8, s p9, s p10
      , s p11, u
      ]
   where
   s :: Show a => a -> Maybe Showable
   s = Just . Showable
   u = Nothing -- in case I skip a problem


-- Utilities

-- | http://www.haskell.org/haskellwiki/Prime_numbers
primesToG :: Integer -> [Integer]
primesToG m = 2 : sieve [3,5..m]  where
   sieve (p:xs) 
      | p*p > m   = p : xs
      | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])
                 -- p : sieve (xs `minus` map (p*) [p,p+2..])
                 -- p : eulers (xs `minus` map (p*) (p:xs))
   sieve [] = error "This will never happen."

primesPE :: [Integer]
primesPE = 2 : primes'
   where
   primes' = sieve [3,5..] 9 primes'
   sieve :: [Integer] -> Integer -> [Integer] -> [Integer]
   sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^(2::Integer)) t
   sieve [] _ _ = error "This will never happen."

minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

union :: Ord a => [a] -> [a] -> [a]
union (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys 
           GT -> y : union (x:xs)  ys
union  xs     []    = xs
union  []     ys    = ys

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
-- Here, I started with the product of [11..20] and then starting eliminating factors that were implied by divisibility by higher numbers.
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
p8 = let s = fmap (\ c -> read [c]) $ concat 
            [ "73167176531330624919225119674426574742355349194934"
            , "96983520312774506326239578318016984801869478851843"
            , "85861560789112949495459501737958331952853208805511"
            , "12540698747158523863050715693290963295227443043557"
            , "66896648950445244523161731856403098711121722383113"
            , "62229893423380308135336276614282806444486645238749"
            , "30358907296290491560440772390713810515859307960866"
            , "70172427121883998797908792274921901699720888093776"
            , "65727333001053367881220235421809751254540594752243"
            , "52584907711670556013604839586446706324415722155397"
            , "53697817977846174064955149290862569321978468622482"
            , "83972241375657056057490261407972968652414535100474"
            , "82166370484403199890008895243450658541227588666881"
            , "16427171479924442928230863465674813919123162824586"
            , "17866458359124566529476545682848912883142607690042"
            , "24219022671055626321111109370544217506941658960408"
            , "07198403850962455444362981230987879927244284909188"
            , "84580156166097919133875499200524063689912560717606"
            , "05886116467109405077541002256983155200055935729725"
            , "71636269561882670428252483600823257530420752963450"]
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
p11 = let grid = array ((1,1),(20,20)) $ zip [(x,y) | x <- [1..(20::Int)], y <- [1..(20::Int)]]
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
            ] in
   grid ! (1,3)