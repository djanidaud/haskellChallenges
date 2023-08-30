{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (histogram,approxPi,amSplit) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


-- Exercise A1
histogram :: Int -> [Int] -> [Int]
histogram n xs | n <= 0 = error ("Error: n can't be " ++ show n)
               | otherwise = getFrequencies [] 0
                where getFrequencies fs iteration | sum fs == length xs = fs
                                                  | otherwise = getFrequencies (fs ++ [frequency]) (iteration + 1)
                                                           where frequency = length [x | x <- xs, min <= x , x <= max]
                                                                 min = iteration*n
                                                                 max = (iteration+1)*n - 1



factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

doubleFactorial :: Integer -> Integer
doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * doubleFactorial (n - 2)

-- Exercise A2
approxPi :: Int -> Double 
approxPi n | n <= 0 = error ("Error: n can't be " ++ show n)
           | otherwise = 2 * sum (getSequence [] 0)
           where getSequence nums currentIndex | currentIndex == n = nums 
                                               | otherwise = getSequence (nums ++ [getEntry (toInteger currentIndex)]) (currentIndex + 1)
                 getEntry k = fromIntegral(factorial k) / fromIntegral(doubleFactorial (2 * k + 1))
                 


-- Exercise A3
amSplit :: Ord a => [a] -> [[a]]
amSplit [] = []
amSplit [x] = [[x]];
amSplit rs = let amSplit1 (x:xs) ls | null xs = if isAntiMonotone (ls ++ [x]) then [ls ++ [x]] else [ls] ++ [[x]]
                                    | isAntiMonotone (ls ++ [x]) = amSplit1 xs (ls ++ [x])
                                    | otherwise = [ls] ++ amSplit1 xs [x]
                                    in amSplit1 rs []


isAntiMonotone :: Ord a => [a] -> Bool
isAntiMonotone xs = isNextBigger xs || isNextSmaller xs

isNextBigger :: Ord a => [a] -> Bool
isNextBigger [] = True;
isNextBigger [_] = True
isNextBigger (x:xs) | x == head(xs) = isNextBigger xs
                    | x < head(xs) = isNextSmaller xs
                    | otherwise = False

isNextSmaller :: Ord a => [a] -> Bool
isNextSmaller [] = True;
isNextSmaller [_] = True
isNextSmaller (x:xs) | x == head(xs) = isNextSmaller xs
                     | x > head(xs) = isNextBigger xs
                     | otherwise = False