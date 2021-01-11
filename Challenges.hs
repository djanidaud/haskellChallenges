{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Dzhani S Daud, dsd1u19@soton.ac.uk
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random


import qualified Data.Map as Map
import Data.Maybe ( catMaybes, fromJust, isJust, isNothing )
import Data.Array.IO ( newListArray, readArray, writeArray, IOArray )
import Data.Function(on)
import Data.Tuple ( swap )

instance NFData Orientation
instance NFData LamMacroExpr
instance NFData LamExpr

-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read,Generic)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read,Generic)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read,Generic)









-- END OF CODE YOU MUST NOT MODIFY

-- ADD YOUR OWN CODE HERE
-- To make the code easier to understand, I have introduced the following custom types:
-- for Part I:
type Crossword = [Cells]
type Cells = [Cell]
type Cell = Maybe Char
type SuccessorFunction = Posn -> Posn
type LookUpTable = Map.Map [Maybe Char] [[Maybe Char]]
type Arrangement = [( [Maybe Char] , Placement)]

-- for Part II & III:
type MacroTable = [(String, LamExpr)]

-------------------------------- Challenge 1 --------------------------------s
-- Recursively finds the locations of all the words in a given wordSearchPuzzle
solveWordSearch :: [String] -> WordSearchGrid -> [ (String, Maybe Placement) ]
solveWordSearch _ [] = error "Error: the input grid must be non-empty!"
solveWordSearch [] _ = []
solveWordSearch words grid | isGridUnsquare       = error "Error: the input grid needs to be square!"
                           | hasPalindromes words = error "Error: the input words cannot contain palinromes!"
                           | otherwise            = foldl (\a w -> (findPlacements (toMaybe w) crossword) ++ a) [] setOfWords
                             where 
                             isGridUnsquare :: Bool
                             isGridUnsquare = any (\s -> length grid /= length s) grid 

                             setOfWords :: [String]
                             setOfWords = nub words

                             crossword:: Crossword
                             crossword = map (toMaybe.toUpperString) grid 


orientations :: [Orientation]
orientations = [Forward, Back, Up, Down, UpForward, UpBack, DownForward, DownBack]


hasPalindromes :: [String] -> Bool
hasPalindromes = any (\w -> (toUpperString w) == (reverse $ toUpperString w))                                                   


toUpperString :: String -> String
toUpperString = map toUpper


toMaybes:: [String] -> [ [Maybe Char] ]
toMaybes = map toMaybe


toMaybe :: String -> [Maybe Char] 
toMaybe = map Just


-- Takes a word and a crossword and finds all of the word's placements inside the crossword
-- This is how the algorithm works:
--    1. We take the first letter of the word
--    2. We start iterating the crossword, looking for a cell, whose value is our firstLetter
--    3. Each time we find such a position, we start checking all the directions (Up, Down, Back.. etc) to look for our word
-- The reason why we are working with the "[Maybe Char]" and "Crossword" types (and not "String" and "WordSearchGrid")
-- is because this function is widely used in Challenge 2, where we work with those types
findPlacements :: [Maybe Char]  -> Crossword -> [(String, Maybe Placement)]
findPlacements [] _ = [("", Nothing)]
findPlacements word@(firstLetter:_) cw | null placements = [ (stringWord, Nothing) ] 
                                       | otherwise       = [ (stringWord , Just placement) | placement <- placements]
                                         where 
                                         cells :: Orientation -> Posn -> Cells
                                         cells = getCells cw $ length word

                                         stringWord :: String
                                         stringWord = catMaybes word

                                         upperLetter :: Maybe Char
                                         upperLetter = fmap toUpper firstLetter

                                         upperWord :: [Maybe Char]
                                         upperWord = map (fmap toUpper) word

                                         placements :: [(Posn, Orientation)]
                                         placements = [ (pos, orientation) | (pos, cell) <- iterateMatrix cw,
                                                                             upperLetter == cell,
                                                                             orientation <- orientations,
                                                                             upperWord  == cells orientation pos]


-- Takes a crossword, a travel length 'n', an orientation and a starting position.
-- Returns the cells that one would obtain by traveling from that starting position, in that direction, with 'n' steps in our crossword
-- If we accidentaly travel out of the crossword, we return the empty list
getCells ::  Crossword ->  Int -> Orientation -> Posn -> Cells
getCells cw n ori posn | arePosnsInGrid = cells 
                       | otherwise = []
                         where 
                         arePosnsInGrid :: Bool
                         arePosnsInGrid = isWithinGrid (last positions) cw

                         positions :: [Posn]
                         positions = take n $ iterate (go ori) posn

                         cells :: [Cell]
                         cells = map (getCell cw) positions


isWithinGrid :: Posn -> Crossword -> Bool
isWithinGrid (x,y) cw = let n = length cw - 1 in elem x [0..n] && elem y [0..n]


getCell :: Crossword -> Posn -> Cell
getCell cw (colId, rowId) = cw !! rowId !! colId


-- Takes an orientation and returns a function describing how a position would change by traveling in that direction
go:: Orientation -> SuccessorFunction
go Forward     =  \(colId, rowId) -> (colId + 1, rowId)
go Back        =  \(colId, rowId) -> (colId - 1, rowId)
go Up          =  \(colId, rowId) -> (colId, rowId - 1)
go Down        =  \(colId, rowId) -> (colId, rowId + 1)
go UpForward   =  go Up . go Forward
go UpBack      =  go Up . go Back
go DownForward =  go Down . go Forward
go DownBack    =  go Down . go Back



-------------------------------- Challenge 2 --------------------------------
-- Takes a list of words and a grid density and returns a WordSearchPuzzle
--
-- The algorithm is based on two assumptions:
--  1. The input words must have enought chars to guarantee uniqueness within the grid (as stated in the coursework spec)
--  2. Deriving from the 1st assumption, if there are no possible ways of uniquely filling a n*n crossword with random chars,
--     then it is also impossible to do it in a (n+1)*(n+1) crossword
--
-- How the algorithm works:
-- Based on the density and the longest word's length we calculate an initial grid size
-- Using that size, we try to create a crossword. If we fail => we increase the size by 1 and try again until we succeed
-- Once we have created a crossword (which is only filled with words), we try to fill its Nothing values with random chars
-- If we fail, we try again with a different crossword of the same size (we do not increase the size because of assumption 2)
createWordSearch :: [String] -> Double -> IO WordSearchGrid
createWordSearch [] _ = error "Error: the words input cannot be [] because such input produces the [] grid and the instructions specify that each grid must be non-empty!"
createWordSearch words density | density <= 0 || 1 < density = error "Error: the grid density must be bigger than 0 and less than 1!"
                               | null words = return []
                               | hasPalindromes words = error "Error: the input words cannot contain palinromes!"
                               | otherwise = toWordSearchGrid $ createWordSearch' initialSize
                                 where 
                                 initialSize :: Int
                                 initialSize = getMinCwSize sortedWords density

                                 sortedWords :: [String]
                                 sortedWords = sortBy (flip compare `on` length) words

                                 maybeWords :: [[Maybe Char]]
                                 maybeWords = toMaybes.(map toUpperString).nub $ sortedWords

                                 createWordSearch' :: Int -> IO Crossword
                                 createWordSearch' size = do cw <- createCrossword maybeWords size 
                                                             if null cw then createWordSearch' $ succ size
                                                             else do
                                                                  puzzle <- fillPuzzle maybeWords cw
                                                                  if null puzzle then createWordSearch' size else return puzzle


toWordSearchGrid :: IO Crossword -> IO WordSearchGrid
toWordSearchGrid ioCw = map catMaybes <$> ioCw


-- Takes a list of words and a crossword size
-- Creates an empty crossword and tries to fill it with the list of words
createCrossword :: [ [Maybe Char] ] -> Int -> IO Crossword
createCrossword words size = parseArrangement arrangement empty
                             where 
                             empty :: Crossword
                             empty = createEmptyCrossword size   

                             arrangements :: IO [Arrangement]
                             arrangements = generateWordArrangements words empty table

                             arrangement :: IO Arrangement
                             arrangement  = safeHead <$> arrangements

                             table :: LookUpTable
                             table = Map.fromList [(w, commonWords)| w <- words, let commonWords = [ cmW | cmW <- words, c <- w, elem c cmW]]


-- Takes a list of words, a crossword and a lookUpTable
-- Returns a random arrangement of a possible way of inserting the words into the crossword
-- The algorithm makes use of Haskell's lazyness to avoid unnecessary computations.
-- It uses a list comprehension to calculate all possible different arrangements, but we only return the first successful one.
-- The lookUpTable here is a Map that stores (word, [words that contain common chars as our word]) data
generateWordArrangements ::  [ [Maybe Char] ] -> Crossword -> LookUpTable -> IO [Arrangement]
generateWordArrangements [] _ _= return [[]]
generateWordArrangements (first:words) cw table = do shuffeled <- shuffle $ computeLegalPlacements first cw
                                                     firstNonEmptyIO [ arrangement plc newCw | plc <- shuffeled,
                                                                                               let newCw = insertWord cw (first, plc),
                                                                                               isUnique newCw common]
                                                     where 
                                                     common :: [[Maybe Char]]
                                                     common = fromJust $ Map.lookup first table     

                                                     arrangement :: Placement -> Crossword -> IO [Arrangement]       
                                                     arrangement plc newCw = do ys <- generateWordArrangements words newCw table
                                                                                return [ (first,plc) : y | y <- ys]


-- Takes a list of words and a crossword which is already filled with these words.
-- Tries to fill the rest of the crosssword with random chars
-- If the crossword is already fully filled, we just return it, else => we calculate a random possible arrangement
fillPuzzle:: [[Maybe Char]] -> Crossword  -> IO Crossword
fillPuzzle words cw = if null initialBlanks then return cw else parseArrangement arrangement cw
                      where 
                      initialBlanks :: [Posn]
                      initialBlanks = [ pos | (pos,cell) <- iterateMatrix cw, isNothing cell]

                      setOfChars :: [Maybe Char]
                      setOfChars = nub $ concat words

                      arrangements :: IO [Arrangement]
                      arrangements = generateCharArrangements initialBlanks setOfChars cw table

                      arrangement :: IO Arrangement
                      arrangement = safeHead <$> arrangements

                      table :: LookUpTable
                      table = Map.fromList [([c], commonWords) | c <- setOfChars, let commonWords = [cmW | cmW <- words, elem c cmW]] 


-- Takes a list of blanks (positions in a crossword which hold the Nothing value), a set of chars, a crossword and a lookUpTable
-- Returns a random arrangement of a possible way of inserting chars (from the setOfChars) into the blank positions 
-- The algorithm makes use of Haskell's lazyness to avoid unnecessary computations.
-- It uses a list comprehension to calculate all possible different arrangements, but we only return the first successful one.
-- The lookUpTable here is a Map that stores (char, [words that contain that char]) data
generateCharArrangements :: [Posn] -> [Maybe Char] -> Crossword -> LookUpTable -> IO [Arrangement]
generateCharArrangements [] _ _ _ = return [[]]
generateCharArrangements (b:blanks) setOfChars cw table = do shuffled <- shuffle setOfChars
                                                             firstNonEmptyIO [ arrangement c newCw | c <- shuffled,
                                                                                                     let newCw = insertWord cw ([c],(b, Up)),
                                                                                                     isUnique newCw $ commonWords c]
                                                             where
                                                             commonWords :: Maybe Char -> [[Maybe Char]]
                                                             commonWords c = fromJust $ Map.lookup [c] table

                                                             arrangement :: Maybe Char -> Crossword -> IO [Arrangement]
                                                             arrangement c cw = do ys <- generateCharArrangements blanks setOfChars cw table
                                                                                   return [ ([c],(b,Up)) : y | y <- ys]


-- Picks the first non-empty value from a list of IO lists
firstNonEmptyIO:: [IO [a]] -> IO [a]
firstNonEmptyIO [] = return []
firstNonEmptyIO (xm:xms) = do x <- xm; if null x then firstNonEmptyIO xms else return x


safeHead :: [[a]] -> [a]
safeHead [] = []
safeHead xs = head xs


-- Takes an arrangement (a list of (word, placement) tuples) and a crossword 
-- and fills the crossword with the words and their respective placements
parseArrangement :: IO Arrangement -> Crossword -> IO Crossword
parseArrangement arrangement crossword = do a <- arrangement ; if null a then return [] else return $ foldl insertWord crossword a


-- Tests if the words in a crossword appear only once
isUnique :: Crossword -> [ [Maybe Char] ] -> Bool
isUnique cw words = and [ length (findPlacements w cw) == 1 | w <- words]


-- Creates a list of (posn,value) tuples of all the cells in a crossword
iterateMatrix :: Crossword -> [(Posn, Cell)]
iterateMatrix matrix = let n = length matrix - 1 in [ (pos, getCell matrix pos) | rowId <- [0..n], colId <- [0..n], let pos = (colId,rowId)]


-- Takes a word and a crossword and returns a list of all the placements that the word could be inserted into
computeLegalPlacements:: [Maybe Char] -> Crossword -> [Placement]
computeLegalPlacements [] _  = []
computeLegalPlacements word cw = [ (pos, orientation) | (pos,_) <- iterateMatrix cw,
                                                        orientation <- orientations,
                                                        canWordBeInserted word $ cells orientation pos]
                                  where 
                                  cells :: Orientation -> Posn -> Cells
                                  cells = getCells cw (length word)


-- Takes a candidate word and some cells and checks if the word can safely be inserted into these cells
-- Example input: 
-- [Just 'h', Just 'i'] [Nothing, Nothing] returns True,
-- [Just 'h', Just 'i'] [Nothing, Just 'i'] also returns True,
-- [Just 'h', Just 'i'] [Nothing, Just 'g'] would return False
canWordBeInserted:: [Maybe Char] -> Cells -> Bool
canWordBeInserted [] xs = xs == []
canWordBeInserted _ []  = False
canWordBeInserted (x:xs) (c:cs) = (isNothing c || c == x) && canWordBeInserted xs cs


createEmptyCrossword :: Int -> Crossword
createEmptyCrossword size = replicate size $ replicate size Nothing


-- Takes a crossword, a word and a placement and inserts the word into the crossword based on the placement
-- To achieve that, it iterates through the crossword and updates each cell appropriately
insertWord:: Crossword -> ([Maybe Char], Placement) -> Crossword
insertWord cw (word, (posn, orientation)) = [ [ updateCell (colId,rowId) | colId <- [0..n] ] | rowId <- [0..n]]
                                            where 
                                            n :: Int
                                            n = length cw - 1
                                            
                                            table :: Map.Map Posn (Maybe Char)
                                            table = Map.fromList $ zip positions word

                                            positions :: [Posn]
                                            positions = take (length word) $ iterate (go orientation) posn

                                            updateCell :: Posn -> Maybe Char
                                            updateCell posn = let l = Map.lookup posn table in if isJust l then fromJust l else getCell cw posn


-- Takes a list of words and a grid density and based on that calculates the minimum size of a grid
getMinCwSize ::[String] -> Double -> Int
getMinCwSize sortedWords density = max longestWordLenght (minCwSide + 1)
                                   where 
                                   minCwSide, longestWordLenght :: Int
                                   minCwSide = ceiling.sqrt $ fromIntegral (length.concat $ sortedWords) / density
                                   longestWordLenght = length.head $ sortedWords


shuffle :: [a] -> IO [a]
shuffle arr = do ar <- newIOArray arr
                 forM [1..n] (\i -> do ran <- randomRIO (i,n)
                                       val1 <- readArray ar i
                                       val2 <- readArray ar ran
                                       writeArray ar ran val1
                                       return val2)
              where 
              newIOArray ::  [a] -> IO (IOArray Int a)
              newIOArray = newListArray (1,n)
              n = length arr


-------------------------------- Challenge 3 --------------------------------
-- Tries to unparse a LamMacroExpr into string
-- The prettyPrint's functionality comes from the "printLamExpr" and "unexpandLamExpr" helper functions defined below
prettyPrint :: LamMacroExpr -> String
prettyPrint me@(LamDef table e) | hasInvalidMacroName me = error "Error: the input macro names must be represented using a string of uppercase characters!"
                                | hasNegativeVars me = error "Error: the input LamVars can only be defined using natural numbers!"
                                | otherwise = pritntedMacros ++ printedExpr
                                  where 
                                  -- Identifies sub-expressions which are syntactically equivalent to an already existing macro. 
                                  -- we call this procedure 'unexpand'
                                  unexpanedTable:: MacroTable
                                  unexpanedTable = foldl (\acc (m,e)  -> acc ++ [ (m, unexpandLamExpr $ LamDef acc e) ]) [] table

                                  pritntedMacros, printedExpr:: String
                                  pritntedMacros = concatMap (\(m,e) -> concat ["def ", m, " = ", printLamExpr e, " in "]) unexpanedTable
                                  printedExpr = printLamExpr.unexpandLamExpr $ LamDef unexpanedTable e

-- Tests if all LamMacro names are defined using only [A-Z] symbols
hasInvalidMacroName :: LamMacroExpr -> Bool
hasInvalidMacroName me@(LamDef table _) = isTableInvalid || isExprInvalid
                                          where 
                                          isNameInvalid :: String -> Bool
                                          isNameInvalid = any (\c -> ord c `notElem` [65..90])

                                          isTableInvalid, isExprInvalid :: Bool
                                          isTableInvalid = any (isNameInvalid.fst) table
                                          isExprInvalid = anyLeaf (\e -> case e of 
                                                                          LamMacro g -> isNameInvalid g
                                                                          _ -> False) me


hasNegativeVars :: LamMacroExpr -> Bool
hasNegativeVars (LamDef table e) = any (<0) (getAllLamVarIds e ++ concatMap (getAllLamVarIds.snd) table)


getAllLamVarIds :: LamExpr -> [Int]
getAllLamVarIds (LamVar id) = [id]
getAllLamVarIds (LamAbs id e) = [id] ++ getAllLamVarIds e
getAllLamVarIds (LamMacro _) = []
getAllLamVarIds (LamApp e1 e2) = getAllLamVarIds e1 ++ getAllLamVarIds e2


-- Takes a predicate and returns True if there exists a LamMacro/LamVar in a LamMacroExpr that satisfies it
anyLeaf :: (LamExpr -> Bool) -> LamMacroExpr -> Bool
anyLeaf f (LamDef table expr) = any (\(_,e) -> anyLeaf' f e) table || anyLeaf' f expr
                                where 
                                anyLeaf' :: (LamExpr -> Bool) -> LamExpr -> Bool
                                anyLeaf' f (LamApp e1 e2) = anyLeaf' f e1 || anyLeaf' f e2
                                anyLeaf' f (LamAbs _ e) = anyLeaf' f e
                                anyLeaf' f e = f e


-- Prints a LamExpr.
-- Omits unnecessary bracketing by following these 3 rules:
-- If we have a LamApp e1 e2, we will:
--              - place brackets around e1 if e1 == LamAbs
--              - place brackets around e2 if e2 == LamApp
--              - if e1 == LamApp _ e1'@(LamAbs), we will place brackets around e1'
printLamExpr:: LamExpr -> String
printLamExpr (LamMacro g) = g
printLamExpr (LamVar n) = "x" ++ show n
printLamExpr (LamAbs n e) = concat ["\\", printLamExpr (LamVar n), " -> ", printLamExpr e]
printLamExpr (LamApp e1 e2) =  case e1 of
                                    LamApp e1' e1'' -> unparse e1' (isAbs e1') ++ " " ++ unparse e1'' (isApp e1'' || isAbs e1'') ++ " " ++ secondTerm 
                                    _ ->  unparse e1 (isAbs e1) ++ " " ++ secondTerm
                               where
                               secondTerm :: String
                               secondTerm = unparse e2 (isApp e2)

                               unparse :: LamExpr -> Bool -> String
                               unparse exp bracketsCond | bracketsCond = "(" ++ printLamExpr exp ++ ")"
                                                        | otherwise    = printLamExpr exp


isAbs :: LamExpr -> Bool
isAbs (LamAbs _ _) = True
isAbs _ = False


isApp :: LamExpr -> Bool
isApp (LamApp _ _ ) = True
isApp _ = False


isLamMacro:: LamExpr -> Bool
isLamMacro (LamMacro _) = True
isLamMacro _ = False 


unexpandLamExpr:: LamMacroExpr -> LamExpr
unexpandLamExpr (LamDef table expr) | null reductions = expr
                                    | otherwise = snd.last $ reductions 
                                      where
                                      reductions :: [(LamExpr, LamExpr)]
                                      reductions = takeWhile (uncurry (/=)) $ zip iters (tail iters) 

                                      iters :: [LamExpr]
                                      iters = iterate unexpand expr

                                      -- We will use this table to identify sub-expressions which are syntactically equivalent to a macro.
                                      swappedTable :: [(LamExpr, String)]
                                      swappedTable = map swap table

                                      macros :: [String]
                                      macros = map fst table   
                                        
                                      -- executes a single step of unexpansion 
                                      -- (Replacing sub-expressions which are syntactically equivalent to an already existing macro)
                                      -- Before replacing a LamMacro x with another LamMacro y, we will test if y is defined before x in the MacroTable
                                      -- In the case of LamApp e1 e2, where we can unnexpand both e1 and e2, we will chose to unexpand the sub-expr
                                      -- whose macro is defined first in the MacroTable
                                      unexpand:: LamExpr -> LamExpr
                                      unexpand e | isJust maybeMacro && (not.isLamMacro) e = macro
                                                 | otherwise = case e of
                                                                LamVar _ -> e
                                                                LamAbs n e -> LamAbs n $ unexpand e
                                                                LamMacro _ | isJust maybeMacro && macro `isToTheLeft` e -> macro
                                                                           | otherwise -> e
                                                                             
                                                                LamApp e1 e2 | unexpandE1 -> LamApp red1 e2 
                                                                             | otherwise -> LamApp e1 red2
                                                                               where 
                                                                               red1, red2, replacedE1Macro, replacedE2Macro :: LamExpr
                                                                               red1 = unexpand e1
                                                                               red2 = unexpand e2    
                                                                               replacedE1Macro = head $ getAllLamMacros red1 \\ getAllLamMacros e1
                                                                               replacedE2Macro = head $ getAllLamMacros red2 \\ getAllLamMacros e2

                                                                               unexpandE1 :: Bool
                                                                               unexpandE1 = red1 /= e1  && (red2 == e2 || replacedE1Macro `isToTheLeft` replacedE2Macro )                 
                                                    where
                                                    maybeMacro :: Maybe String
                                                    maybeMacro = lookup e swappedTable

                                                    macro :: LamExpr
                                                    macro = LamMacro $ fromJust maybeMacro

                                      -- returns True if the first macro is defined before the second macro. ie it is 'toTheLeft' in the order of the table
                                      isToTheLeft :: LamExpr -> LamExpr -> Bool
                                      (isToTheLeft) (LamMacro a) (LamMacro b) = fromJust $ liftA2 (<) (elemIndex a macros) (elemIndex b macros)   
                                                     
                                                                                              
getAllLamMacros :: LamExpr -> [LamExpr]
getAllLamMacros (LamVar _ ) = []
getAllLamMacros (LamMacro g ) = [LamMacro g]
getAllLamMacros (LamAbs _ e ) = getAllLamMacros e
getAllLamMacros (LamApp e1 e2 ) = getAllLamMacros e1 ++ getAllLamMacros e2

-- Challenge 4 --
{-
We will transform the following grammar 
MacroExpr ::= "def" MacroName "=" Expr "in" MacroExpr | Expr
Expr ::= Var | MacroName | Expr Expr | “\” Var “->” Expr | “(“ Expr “)” 
MacroName ::= UChar | UChar MacroName
UChar ::= "A" | "B" | ... | "Z"
Var ::= “x” Digits
Digits ::= Digit | Digit Digits
Digit ::= “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”

into 

MacroExpr ::= "def" MacroName "=" Expr "in" MacroExpr | Expr
Expr ::= Application | "\" Var "->" Expr
Application ::= Application Atom | Atom
Atom ::= "(" Expr ")" | Var | MacroName | "\" Var "->" Expr
MacroName ::= UChar | UChar MacroName
UChar ::= "A" | "B" | ... | "Z"
Var ::= “x” Digits
Digits ::= Digit | Digit Digits
Digit ::= “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”

We will use the newly defined grammar to construct our parser.
-}

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro x  | isExprInvalid || hasRepeateMacroDef || hasUnclosedMacroDef result = Nothing
                 | otherwise = Just result
                  where
                  isExprInvalid, hasRepeateMacroDef :: Bool
                  isExprInvalid = null parsedExp || (not.null.snd.head) parsedExp 
                  hasRepeateMacroDef = length (nub macros) /= length macros
                  
                  parsedExp :: [(LamMacroExpr, String)]
                  parsedExp = parse macroExpr x

                  result :: LamMacroExpr
                  result@(LamDef table _) = fst $ head parsedExp

                  macros:: [String]
                  macros = map fst table


hasUnclosedMacroDef :: LamMacroExpr -> Bool
hasUnclosedMacroDef (LamDef table _) = any (not.isTermClosed.snd) table

varExpr :: Parser LamExpr
varExpr = char 'x' >> LamVar <$> nat
          
abstractionExpr :: Parser LamExpr
abstractionExpr = do char '\\'
                     var <- token varExpr
                     let (LamVar varId) = var

                     string "->"

                     exp <- token expr

                     return (LamAbs varId exp)


bracketedExpr :: Parser LamExpr
bracketedExpr = do char '('
                   expr <- token expr
                   char ')'
                   return expr


applicationExpr :: Parser LamExpr
applicationExpr = do atoms <- some $ token atomExpr
                     let ex = foldl (\at acc -> (LamApp at acc) ) (head atoms) (tail atoms)
                     return ex


atomExpr :: Parser LamExpr
atomExpr = bracketedExpr <|> varExpr <|> macroNameExpr <|> abstractionExpr


macroNameExpr :: Parser LamExpr
macroNameExpr = LamMacro <$> some upper
                 

expr :: Parser LamExpr
expr = applicationExpr <|> abstractionExpr


macroExpr :: Parser LamMacroExpr
macroExpr = do  arr <- many (do  token $ string "def"
                                 
                                 macro <- macroNameExpr
                                 let (LamMacro mName) = macro
                                 
                                 token $ char '='
                                 
                                 exp <- expr
                                 
                                 token $ string "in"
                                 
                                 return (mName, exp))
                exp <- expr
                return $ LamDef arr exp


-- Checks if a var is bounded or not
isFree :: Int -> LamExpr -> Bool 
isFree _ (LamMacro _) = False
isFree x (LamVar y) = x == y
isFree x (LamApp e1 e2) = isFree x e1 || isFree x e2  
isFree x (LamAbs y e) = if x == y then False else isFree x e 
            

isTermClosed :: LamExpr -> Bool
isTermClosed expr = isTermClosed' expr        
                    where       
                    isTermClosed' :: LamExpr -> Bool
                    isTermClosed' (LamVar y) = not $ isFree y expr
                    isTermClosed' (LamAbs y e) = (not $ isFree y expr) && isTermClosed' e
                    isTermClosed' (LamApp e1 e2) = isTermClosed' e1 && isTermClosed' e2
                    isTermClosed' (LamMacro _) = True



-- Challenge 5
-- Transforms a LamMacroExpr into a CPS-LamMacroExpr while avoiding free-variable capture
-- The algorithm is consisted of two parts: one which pattern-matches an untransformed expr and returns its cps-version
--                                          the other part determines the names of the newly-created cps variables. 
-- Each time we introduce a new variable, we name it so that it is "1" bigger than the previously introduced variable
-- For example, if the previously introduced variable is x2 => the next one will be x3
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform me@(LamDef table exp) | hasInvalidMacroName me = error "Error: the input macro names must be represented using a string of uppercase characters!"
                                   | hasNegativeVars me = error "Error: the input LamVars can only be defined using natural numbers!"
                                   | otherwise = LamDef cpsTable cpsExpr
                                     where 
                                     cpsExpr :: LamExpr
                                     cpsExpr = expTransform exp cpsTableLastId

                                     cpsTableLastId :: Int
                                     cpsTableLastId = safeMaximum lastId $ map (getBiggestVarId 0.snd) cpsTable
                                     
                                     cpsTable :: MacroTable
                                     cpsTable = map (\(m,ex) -> (m, expTransform ex lastId)) table

                                     lastId, expLastId, tableLastId :: Int
                                     lastId = max expLastId tableLastId
                                     expLastId = getBiggestVarId 0 exp
                                     tableLastId = safeMaximum 0 $ map (getBiggestVarId 0. snd) table
                                  

safeMaximum :: Ord a => a -> [a] -> a
safeMaximum def xs = maximum (def:xs)


expTransform :: LamExpr -> Int -> LamExpr
expTransform expr lastId = case expr of
                             LamMacro _ -> expr
                             LamVar x   -> LamAbs k (LamApp (LamVar k) (LamVar x))
                             LamAbs x exp -> LamAbs k (LamApp (LamVar k) (LamAbs x (expTransform exp k)))
                             LamApp exp1 exp2 -> LamAbs k (LamApp firstTerm (LamAbs f (LamApp secondTerm (LamAbs e (LamApp (LamApp (LamVar f) (LamVar e) ) (LamVar k))))))
                                                 where
                                                 firstTerm, secondTerm :: LamExpr
                                                 firstTerm = expTransform exp1 e
                                                 secondTerm = expTransform exp2 $ getBiggestVarId e firstTerm

                                                 f, e :: Int
                                                 f = k + 1
                                                 e = f + 1   
                            where 
                            k :: Int
                            k = lastId + 1
                            
                               
getBiggestVarId :: Int -> LamExpr -> Int
getBiggestVarId n e = safeMaximum n $ getAllLamVarIds e


-- Challenge 6
compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
compareInnerOuter me steps | steps < 0 = error "Error: the number of steps cannot be negative"
                           | hasInvalidMacroName me = error "Error: the input macro names must be represented using a string of uppercase characters!"
                           | hasNegativeVars me = error "Error: the input LamVars can only be defined using natural numbers!"
                           | hasUnclosedMacroDef me = error "Error: the input LamMacro definitions must be closed!"
                           | otherwise = (inner, outer, cpsInner, cpsOuter)
                             where 
                             inner, outer, cpsInner, cpsOuter :: Maybe Int
                             inner = getSteps innerRedn1 me
                             outer =  getSteps outerRedn1 me
                             cpsInner = if isJust inner then getSteps innerRedn1 cps else Nothing
                             cpsOuter = if isJust outer then getSteps outerRedn1 cps else Nothing

                             cps :: LamMacroExpr
                             cps = LamDef t1 $ LamApp c1 (LamAbs 1 (LamVar 1))
                             LamDef t1 c1 = cpsTransform me
                                             
                             isNotFullyReduced :: Maybe LamMacroExpr -> Bool
                             isNotFullyReduced Nothing = True
                             isNotFullyReduced (Just (LamDef t1 e1)) = t1 /= [] || hasRedex e1

                             getSteps :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> Maybe Int
                             getSteps sse e | steps' > steps || isNotFullyReduced lastEval = Nothing 
                                            | otherwise = Just steps'
                                              where 
                                              steps' :: Int
                                              steps' = length evalTrace

                                              evalTrace :: [Maybe LamMacroExpr]
                                              evalTrace = take steps $ trace sse e
                                                
                                              lastEval :: Maybe LamMacroExpr
                                              lastEval = if null evalTrace then Just me else last evalTrace


trace :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> [Maybe LamMacroExpr]
trace ssev m = map snd $ takeWhile (uncurry (/=)) $ reductions ssev m

                         
reductions :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> [(Maybe LamMacroExpr, Maybe LamMacroExpr)]
reductions ssev m = zip evals (tail evals)
                    where 
                    evals :: [Maybe LamMacroExpr]
                    evals = iterate (maybe1Step) (Just m)

                    maybe1Step :: Maybe LamMacroExpr -> Maybe LamMacroExpr
                    maybe1Step Nothing = Nothing       
                    maybe1Step (Just e) = ssev e
                     

-- Performs a single step of innermost evaluation
-- For each redex:
-- Evaluates the argument before applying it to a function (LamAbs)
-- If the function (LamAbs) contains redexes => go inside it first  
-- The local function "inner" returns a (expr, Maybe string) tuple representing (expr, the name of a LamMacro that we have expanded if any)
-- Each time we expand a macro, we expand it everywhere using the macroExpansion method      
innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 me@(LamDef t expr) | hasInvalidMacroName me || hasNegativeVars me || hasUnclosedMacroDef me = Nothing
                              | (not.hasRedexOrMac) expr = Just macroExpand1
                              | isJust expandedM = Just $ macroExpansion me (fromJust expandedM)
                              | otherwise = Just $ LamDef t evaluatedE
                                where
                                (evaluatedE, expandedM) = inner expr :: (LamExpr, Maybe String) 

                                macroExpand1 :: LamMacroExpr
                                macroExpand1 = if null t then me else macroExpansion me (fst.head $ t)
                                
                                inner :: LamExpr -> (LamExpr, Maybe String)   
                                inner (LamMacro g) = (fromJust $ lookup g t, Just g)                                                              
                                inner (LamApp e@(LamAbs x e1) e2) | hasRedex e1 = (LamApp (LamAbs x e1') e2, m') 
                                                                  | hasRedex e2 = (LamApp e e2', m'') 
                                                                  | otherwise   = (subst e1 x e2, Nothing)
                                                                    where 
                                                                    (e1', m')  = inner e1
                                                                    (e2', m'') = inner e2                              
                                inner (LamApp e@(LamMacro g) e2) | hasRedexOrMac e2 = (LamApp e e2', m') 
                                                                 | otherwise = (LamApp (fromJust $ lookup g t) e2,  Just g)
                                                                   where 
                                                                   (e2', m') = inner e2                                                  
                                inner m = singleEval inner m

-- Performs a single step of outermost evaluation
-- The local function "outer" returns a (expr, maybe string) tuple representing (expr, the name of a LamMacro that we have expanded)
-- Each time we expand a macro, we expand it everywhere using the macroExpansion method   
outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 me@(LamDef t expr) | hasInvalidMacroName me || hasNegativeVars me || hasUnclosedMacroDef me = Nothing
                              | (not.hasRedexOrMac) expr = macroExpand1
                              | isJust expandedM = Just $ macroExpansion me (fromJust expandedM)
                              | otherwise = Just $ LamDef t evaluatedE
                                where 
                                (evaluatedE, expandedM) = outer expr :: (LamExpr, Maybe String)

                                macroExpand1 :: Maybe LamMacroExpr
                                macroExpand1 = if null t then Just me else Just $ macroExpansion me (fst.head $ t)
                                          
                                outer :: LamExpr -> (LamExpr, Maybe String)  
                                outer (LamMacro g) = (fromJust $ lookup g t, Just g)            
                                outer (LamApp (LamAbs x e1) e2) = (subst e1 x e2, Nothing)
                                outer m = singleEval outer m


-- Performs a single step of evaluation with some strategy (innermost/outermost)
-- The code for this method applies to both strategies and the method's purpose is to make the code more easy to understand
singleEval :: (LamExpr -> (LamExpr, Maybe String)) -> LamExpr -> (LamExpr, Maybe String)
singleEval f (LamAbs x e) = let (e', t') = f e in (LamAbs x e', t')  
singleEval f (LamApp e1 e2) | hasRedexOrMac e1 = (LamApp e1' e2, t')
                            | otherwise        = (LamApp e1 e2', t'')
                              where 
                              (e1', t')  = f e1 :: (LamExpr, Maybe String)
                              (e2', t'') = f e2 :: (LamExpr, Maybe String)                                                                                          
singleEval _ m@(LamVar _) = (m, Nothing)


-- Takes a LamMacroExpr and a LamMacro's name and expands this macro everywhere inside the LamMacroExpr
macroExpansion :: LamMacroExpr -> String -> LamMacroExpr
macroExpansion (LamDef table expr) g = LamDef table' (explore expr)
                                       where 
                                       table' :: [(String, LamExpr)]
                                       table' = filter ((/=g).fst) $ map (\(g,e) -> (g, explore e)) table

                                       explore :: LamExpr -> LamExpr
                                       explore (LamApp e1 e2) =  LamApp (explore e1) (explore e2)
                                       explore (LamAbs x e) = LamAbs x (explore e)
                                       explore m@(LamMacro h) = if h == g then fromJust $ lookup h table else m
                                       explore e = e

 
hasRedex:: LamExpr -> Bool
hasRedex (LamApp (LamAbs _ _) _) = True
hasRedex (LamApp e1 e2) = hasRedex e1 || hasRedex e2 
hasRedex (LamAbs _ e) = hasRedex e
hasRedex (LamMacro _) = False
hasRedex _ = False


hasRedexOrMac:: LamExpr -> Bool
hasRedexOrMac (LamApp (LamAbs _ _) _) = True
hasRedexOrMac (LamApp e1 e2) = hasRedexOrMac e1 || hasRedexOrMac e2 
hasRedexOrMac (LamAbs _ e) = hasRedexOrMac e
hasRedexOrMac (LamMacro _) = True
hasRedexOrMac _ = False


subst:: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamMacro g) _ _ = (LamMacro g)
subst (LamVar x) y e = if x == y then e else LamVar x 
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)
subst (LamAbs x e1) y e | x /= y && not(isFree x e) = LamAbs x (subst e1 y e)
                        | x /= y && isFree x e = let x' = (rename x e1) in subst (LamAbs x' (subst e1 x (LamVar x'))) y e 
                        | otherwise = LamAbs x e1


rename :: Int -> LamExpr -> Int
rename x e = if isFree (x + 1) e then rename (x + 1) e else x + 1