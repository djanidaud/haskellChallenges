{-# LANGUAGE DeriveGeneric #-}
--SOLUTIONS TO COURSEWORK 1 for COMP2209, 2019
--DO NOT DISTRIBUTE
--Julian Rathke, Oct 2019

module Exercises (evalInst,findMaxReducers,isPossiblePower,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.Function(on)
import Data.List


-- Exercise A7
data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Maybe Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

evalInst :: Stack -> SMProg -> Stack
evalInst stack [] = stack
evalInst [] _  = error "Empty Stack"
evalInst (_:stack) (Pop:prog) = evalInst stack prog
evalInst (x:stack) (Dup:prog) = evalInst (x:x:stack) prog
evalInst [_] _ = error "Error: Operation requires 2 stack values"
evalInst (Nothing:_:stack) (_:prog) = evalInst (Nothing:stack) prog
evalInst (_:Nothing:stack) (_:prog) = evalInst (Nothing:stack) prog
evalInst (_:Just 0:stack) (Div:prog) = evalInst (Nothing:stack) prog
evalInst (Just x:Just y:stack) (Div:prog) = evalInst (Just (x `div` y):stack) prog
evalInst (Just x:Just y:stack) (Mul:prog) = evalInst (Just (x * y):stack) prog
evalInst (Just x:Just y:stack) (Add:prog) = evalInst (Just (x + y):stack) prog
evalInst (Just x:Just y:stack) (Sub:prog) = evalInst (Just (x - y):stack) prog

-- Exercise A8
findMaxReducers :: Stack -> [SMProg]
findMaxReducers [] = []
findMaxReducers [_] = [[]]
findMaxReducers l@(x:xs) = if containsNothing then findMaxReducers'' split else evalInstructions l $ findMaxReducers' l 
                      where  containsNothing = (elem Nothing xs)
                             split = splitByNothing l

findMaxReducers'' :: (Stack, Stack) -> [SMProg]
findMaxReducers'' (xs,ys) = [(concat x) ++ y | x<-(combinations (length xs - 1)), y<-(evalInstructions ys $ findMaxReducers' ys)  ]

combinations :: Int -> [[SMProg]]
combinations 0 = [[]];       
combinations l = [inst:y | inst <- [[Add], [Sub], [Mul], [Div],[Pop]], y<-combinations (l-1) ]

splitByNothing :: Stack -> (Stack, Stack)
splitByNothing xs = (reverse $ dropWhile (/=Nothing) $ reverse xs ,(++) [Nothing] $ reverse $ takeWhile (/=Nothing) $ reverse xs)   


findMaxReducers' :: Stack -> [SMProg]
findMaxReducers' []  = []
findMaxReducers' [_] = [[]]
findMaxReducers' (Nothing:Just x:xs) = if doWePopNextStep (Just x:xs) then generate xs bonusOperations else generate xs [(Just x,Pop)]
                                     where bonusOperations = (Just x,Pop) : [(Nothing,z)| z<-[Add,Sub,Mul,Div]]                                       
findMaxReducers' (Just x:Just y:xs) | y == 0 && doWePopNextStep (Just 0:xs) = generate xs divWith0
                                    | y == 0                                = generate xs unsafeDiv
                                    | x < 0 || y < 0                        = generate xs negative
                                    | otherwise                             = generate xs safeDiv
                                    where unsafeDiv  = findBiggest operations
                                          safeDiv    = findBiggest ((x `div` y, Div):operations)
                                          divWith0   = [(Nothing, Div)] ++ unsafeDiv
                                          negative   = map (\(x,ins) -> (Just x,ins)) ((x `div` y, Div):operations)
                                          operations = [((x + y), Add),(y,Pop), ((x * y), Mul), ((x - y), Sub)]

generate :: Stack -> [(Maybe Int, Instruction)] -> [SMProg]
generate xs arr = [inst:y| (result,inst) <- arr, y<-findMaxReducers' (result:xs)]                         

doWePopNextStep :: Stack -> Bool
doWePopNextStep [] = False;
doWePopNextStep [_] = False;
doWePopNextStep (Just x:Just z:_) = elem (Just z,Pop) $ findBiggest [(z + x, Add),(z,Pop), (z * x, Mul), (x - z, Sub)]

evalInstructions :: Stack -> [SMProg] -> [SMProg]
evalInstructions _ [[]] = [[]] 
evalInstructions stack iss = map (\(inst,_) -> inst) (filter (\(_,x) -> firstEl == x) sorted)
                  where sorted = sortBy (compare `on` snd) evaledStack
                        firstEl = snd $ last sorted
                        evaledStack = eval stack iss

eval:: Stack -> [SMProg] -> [(SMProg,Int)]
eval stack xs = map (\x -> (x,evalStack stack x)) xs;

evalStack :: Stack -> SMProg -> Int
evalStack stack inst = head $ unwrap $ evalInst stack inst

unwrap :: Stack -> [Int]
unwrap xs = map (\(Just x) -> x) xs 

findBiggest:: [(Int,Instruction)] -> [(Maybe Int,Instruction)]
findBiggest [] = []
findBiggest [(x,inst)] = [(Just x, inst)]
findBiggest xs = map (\(x,inst) -> (Just x, inst)) (filter (\(x,_) -> firstEl == x) sorted)
                  where sorted = sortBy (compare `on` fst) xs
                        firstEl = fst $ last sorted

-- Exercise A9
isPossiblePower :: Int -> Int -> Bool
isPossiblePower 0 _            = error "Error cant be zero"
isPossiblePower k l | k < 0    = False
                    |otherwise = shortestL <= l && l <= k - 1
                     where shortestL = min (getTrailLength 2) (getTrailLength 8 + 2)
                           getTrailLength unit = length (snd $ produceTrail 2 k unit) `div` 2
    
produceTrail :: Int -> Int -> Int -> (Int, [Instruction])
produceTrail _ 0 _ = (1,[])
produceTrail x 1 _ = (x,[])
produceTrail x n unit | n == steps = (unit,[])
                      | even $ n `mod` 2    = ((fst squairedPair) * (fst squairedPair),          snd squairedPair ++ [Dup,Mul])
                      | otherwise           = (unit  * (fst multipliedPair)           , [Dup] ++ snd multipliedPair ++ [Mul])
                      where squairedPair    = produceTrail x (n `div` 2) unit
                            multipliedPair  = produceTrail x (n - steps) unit
                            steps           = loga_b x unit
                                      
loga_b :: Int -> Int -> Int
loga_b a b | b == 1 = 0| b == a = 1 | otherwise = 1 + loga_b a (b `div` a)