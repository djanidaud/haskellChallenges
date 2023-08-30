{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (neighbours,findBonding,insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

import Data.List
import Data.Function (on)

type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

-- Exercise A4

neighbours ::  Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k m center xs | k < 0 = error "Error: K can't be a negative number!"
                         | k >= length xs = xs
                         | otherwise      = [x | (x,_) <- (take k sortedList)]
                         where sortedList = sortBy (compare `on` snd) list
                               list = zip xs (map (m center) xs)


-- Exercise A5
findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding f xs | length result == length xs = Just result
                 | otherwise = Nothing
                  where result = concat [[(x,y),(y,x)] | (x,y) <- (backtrack xs filteredUniqueEdges [[]]), elem (y,x) filteredTuples]
                        filteredTuples = filter (\(x,y) -> f x y) (pairs xs)
                        filteredUniqueEdges = filter (\(x,y) -> f x y) (edges [] (pairs xs))
 
backtrack _ [] _ = []; 
backtrack abc l@(x:xs) stack | isSolutionFound = l
                             | isErrorFound    = backtrack abc pop (tail stack)
                             | otherwise       = backtrack abc filtered ([xs] ++ stack)   

                              where isSolutionFound = length l == (length abc `div` 2) && (areAllVerticesUsed abc l)
                                    isErrorFound    = not (areAllVerticesUsed abc l)
                                    filtered        = filterPair x xs ++ [x]
                                    pop             = head stack
                  
                                       


areAllVerticesUsed :: Eq a => [a] -> [(a, a)] -> Bool
areAllVerticesUsed xs pairs = and [elem x (concat [[a,b] | (a, b) <- pairs]) | x <- xs]

filterPair :: Eq b => (b, b) -> [(b, b)] -> [(b, b)]
filterPair (x,y) pairs = filter (\(a,b) -> (a,b)==(x,y) || (a/=x && b/=x && b/=y && a/=y) || (b,a) == (x,y)) pairs    

pairs :: Eq b => [b] -> [(b, b)]
pairs ls = [(x,y) | x <-ls, y<-ls, x/=y]

edges :: Eq a => [(a, a)] -> [(a, a)] -> [(a, a)]
edges acc [] = acc
edges acc ((x,y):xs) = if elem (y,x) acc then edges acc xs else edges (acc ++ [(x,y)]) xs



-- Exercise A6

data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode val (tree, []) = addNode (tree,[]) val
insertFromCurrentNode val (Node l x v r, ts) = insertFromCurrentNode' val (Node l x v r, ts) x

insertFromCurrentNode' :: Ord t => t -> Zipper t -> t -> Zipper t
insertFromCurrentNode' val (tree,[]) _ = addNode (tree,[]) val
insertFromCurrentNode' val (Node l x v r, ts) previous | x == val                        = current
                                                       | checkBounding previous val x    = addNode (goLeft current) val
                                                       | checkBounding x val previous    = addNode (goRight current) val
                                                       | otherwise                       = insertFromCurrentNode' val (goUp current) x
                                                        where current = (Node l x v r, ts)
                                                      
checkBounding :: Ord a => a -> a -> a -> Bool
checkBounding a  b c = a <= b && b <= c

createNode :: a -> VTree a
createNode value = Node Leaf value 1 Leaf

addNode :: Ord a => Zipper a -> a -> Zipper a
addNode (Leaf,ts) value = (createNode value, ts)
addNode (Node l n v r, ts) value | value > n = addNode (goRight current) value
                                 | value < n = addNode (goLeft current) value
                                 | otherwise = current
                                 where current = (Node l n v r, ts)

goLeft :: Zipper a -> Zipper a
goLeft (Node l x v r, ts) = (visit l, L x v r:ts)

goRight :: Zipper a -> Zipper a
goRight (Node l x v r, ts) = (visit r, R x v l:ts)

goUp :: Zipper a -> Zipper a
goUp (t, L x v r:ts) = (Node t x (v+1) r, ts)
goUp (t, R x v l:ts) = (Node l x (v+1) t, ts)

visit:: VTree a -> VTree a
visit Leaf = Leaf
visit (Node l x v r) = (Node l x (v+1) r)

