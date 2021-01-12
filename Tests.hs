-- comp2209 Functional Programming Challenges Tests
-- (c) University of Southampton 2020
-- Author: Dzhani S Daud, dsd1u19@soton.ac.uk 

import qualified Control.Exception as E
import Data.List
import Data.Maybe
import Test.HUnit
import Challenges
import Data.Either

main :: IO ()
main = testChallengeI >> testChallengeII >> testChallengeIII >>  testChallengeIV >> testChallengeV >> testChallengeVI 


assertNull:: (Eq a, Show a) => String -> [a] -> Assertion
assertNull m a = assertEqual m [] a


assertEqualLists:: (Eq a, Show a) => String -> [a] -> [a] -> Assertion
assertEqualLists m a b = assertNull m (a \\ b)


createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln


printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws
                      return ()


-- Tests the following cases:
--    base case functionality 
--    lowercase words test
--    duplicated words test
--    empty words test, ie [] test
--    empty grid test
--    words that contain the empty string test
--    unsquare grid test
--    palindromes test
testChallengeI :: IO ()
testChallengeI = do  putStrLn "-------- Testing Challenge I --------" 
                     
                     let test1 = TestCase $ assertEqualLists "" solution1 (solveWordSearch words1 grid1)
                     let test2 = TestCase $ assertEqualLists "" solution2 (solveWordSearch words2 grid2)
                     let test3 = TestCase $ assertEqualLists "" solution3 (solveWordSearch words3 grid3)
                     let test4 = TestCase $ assertEqualLists "" solution4 (solveWordSearch words4 grid4)
                     let test5 = TestCase $ assertEqualLists "The algorithm should be able to handle lowercase chars!" solution5 (solveWordSearch words5 grid5)

                     let test6 = TestCase $ assertEqual "The algorithm should not look for the same word more than one time!" 
                            [("WORD", Nothing)] (solveWordSearch ["WORD", "WORD"] ["XX","XX"])

                     let test7 = TestCase $ assertNull "The algorithm should be able to handle the [] words as input!" (solveWordSearch [] ["XXX","XXX","XXX"])
                     let test8 = TestCase $ assertEqual "The algorithm should be able to handle the [] grid" ([("SOMEWORD", Nothing)]) (solveWordSearch ["SOMEWORD"] [])
                     let test9 = TestCase $ assertError "The algorithm should throw an error for the \"\" word!" (solveWordSearch ["ME", "" , "JOB"] ["GJV", "DWA", "TSW"])
                     let test10 = TestCase $ assertError "The algorithm should throw an error for unsquare grids!" (solveWordSearch ["SOMEWORD"] ["KDK","DWLN","KWLN"]) 
                     
                     let test11 = TestCase $ assertError "The algorithm should throw an error for palindrome words!" 
                            (solveWordSearch ["TEEMO","ANNA","MARIA"] [ "HAGNIRTSH","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR"])
               
                     runTestTT $ TestList [
                            TestLabel "test1" test1,
                            TestLabel "test2" test2,
                            TestLabel "test3" test3,
                            TestLabel "test4" test4,
                            TestLabel "lowercaseTest" test5,
                            TestLabel "duplucateInputWords" test6,
                            TestLabel "emptyWordsTest" test7,
                            TestLabel "emptyGridTest" test8,
                            TestLabel "emptyStringTest" test9,
                            TestLabel "unsquareGridTest" test10,
                            TestLabel "palinromesTest" test11 ]

                     putStrLn "-------- End of test --------"
                     where 
                     assertError :: String -> [(String, Maybe Placement)] -> Assertion
                     assertError m f = do wasErrorThrown <- isLeft <$> (E.try $ E.evaluate f :: IO (Either E.SomeException ( [(String, Maybe Placement)])))
                                          assertBool m wasErrorThrown
                                                            
                        
-- Tests the following cases:
--    test with a density of 1, 0.75, 0.5
--    duplicated words test
--    lowercase words test
--    empty words test, ie [] test
--    zero density test
--    negative density test
--    desnity > 1 test
--    palindromes test      
--    words that contain the empty string test       
testChallengeII :: IO ()
testChallengeII = do  putStrLn "-------- Testing Challenge II --------"
                      
                      let test1 = TestCase $ assertSolution "" words1 1 
                      let test2 = TestCase $ assertSolution "" words2 0.75
                      let test3 = TestCase $ assertSolution "" words1 0.5
                      let test4 = TestCase $ (do result <- createAndSolve ["SOMEWORD", "SOMEWORD"] 1
                                                 assertBool "The algorithm should not place an input word into the new grid more than one time!" (length result == 1))

                      let test5 = TestCase $ assertSolution  "The algorithm should be able to handle lowercase letters as input" ["hello","WORLD"] 1

                      let test6 = TestCase $ assertIOError "The algorithm should throw an error for the [] words, because such input produces the [] grid and the output grid density is undefined, so it is not strictly less then the input density" 
                            (createWordSearch [] 0.5)

                      let test7 = TestCase $ assertIOError "The algorithm should throw an error for a density of 0!" (createWordSearch ["SOMEWORD"] 0)
                      let test8 = TestCase $ assertIOError "The algorithm should be able to handle negative grid density!" (createWordSearch ["SOMEWORD"] (-0.4))
                      let test9 = TestCase $ assertIOError "The algorithm should be able to handle grid densities larger than 1!" (createWordSearch ["SOMEWORD"] 2)
                      let test10 = TestCase $ assertIOError "Failed, The algorithm should throw an error for palindrome words!" ( createWordSearch ["TEEMO","ANNA","MARIA"] 0.7)
                      let test11 = TestCase $ assertIOError "The algorithm should throw an error for the \"\" word" ( createWordSearch ["ME", "" , "JOB"] 0.3)
                  
                      runTestTT $ TestList [
                            TestLabel "test1" test1,
                            TestLabel "test2" test2,
                            TestLabel "test3" test3,
                            TestLabel "duplucateInputWords" test4,
                            TestLabel "lowercaseTest" test5,
                            TestLabel "emptyWordsTest" test6,
                            TestLabel "zeroDensityTest" test7,
                            TestLabel "negativeDensity" test8,
                            TestLabel "largeDensityTest" test9,
                            TestLabel "palindromeTest" test10,
                            TestLabel "emptyStringTest" test11 ]
   
                      putStrLn "-------- End of test --------" 
                      where
                      -- tests if the output grid's density is strictly less then the given input density
                      -- checks is the words appear exacly once in the output grid
                      assertSolution:: String -> [String] -> Double -> Assertion
                      assertSolution m words density = do grid <- createWordSearch words density
                                                          printGrid grid
                                                          let result = solveWordSearch words grid
                                                          assertBool m (checkSolution result words density $ length grid)

                      assertIOError ::  String -> IO WordSearchGrid -> Assertion
                      assertIOError m f = do wasErrorThrown <- isLeft <$> (E.try $ E.evaluate f :: IO (Either E.SomeException (IO WordSearchGrid)))
                                             assertBool m wasErrorThrown

                      -- returns True if the output grid's density is strictly less then the given input density AND the words appear exacly once in the output grid
                      checkSolution :: [(String, Maybe Placement)] -> [String] -> Double -> Int ->  Bool
                      checkSolution test words d gridSize = sameLength && allWordsArePresent && outputD < d
                                                            where 
                                                            sameLength = length test == length words
                                                            allWordsArePresent = length [ w | w <- words, (w1,plc) <- test, w == w1, isJust plc] == length words
                                                            outputD =  fromIntegral (length.concat $ words)  /  fromIntegral (gridSize * gridSize)
                      

-- Tests the following cases:
--    correct bracketing
--    replacement of sub-exprs which are equal to an already defined macro
--    macros with equal definitions test
--    composed macros
--    negative lamvars
--    invalid macro name
testChallengeIII :: IO ()
testChallengeIII = do  putStrLn "-------- Testing Challenge III --------"
                       
                       let test1 = TestCase $ assertEqual "" lamText1 (prettyPrint lamExp1)
                       let test2 = TestCase $ assertEqual "" lamText2 (prettyPrint lamExp2) 
                       let test3 = TestCase $ assertEqual "" lamText3 (prettyPrint lamExp3) 
                       let test4 = TestCase $ assertEqual "" lamText4 (prettyPrint lamExp4) 
                       let test5 = TestCase $ assertEqual "" lamText5 (prettyPrint lamExp5) 
                       let test6 = TestCase $ assertEqual "" lamText6 (prettyPrint lamExp6) 
                       let test7 = TestCase $ assertEqual "" lamText7 (prettyPrint lamExp7) 
                       let test8 = TestCase $ assertEqual "" lamText8 (prettyPrint lamExp8) 
                       let test9 = TestCase $ assertEqual "" lamText9 (prettyPrint lamExp9) 
                       let test10 = TestCase $ assertEqual "" lamText10 (prettyPrint lamExp10) 
                       let test11 = TestCase $ assertEqual "" lamText11 (prettyPrint lamExp11) 
                       let test12 = TestCase $ assertEqual "" lamText12 (prettyPrint lamExp12) 
                       let test13 = TestCase $ assertEqual "" lamText13 (prettyPrint lamExp13) 

                       let test14 = TestCase $ assertEqual "The algorithm must recognise input sub-expr that are equal to ones defined in a macro" lamText14 (prettyPrint lamExp14)

                       let test15 = TestCase $ assertEqual "The algorithm should be able to handle equal macros, by choosing the one defined first" lamText15 (prettyPrint lamExp15)  

                       let test16 = TestCase $ assertEqual "The algorithm should be able to handle composed macros by choosing the more general one" lamText16 (prettyPrint lamExp16)  

                       let test17 = TestCase $ assertError "The algorithm should throw an error for negative LamVars" (prettyPrint negativeLamVar) 

                       let test18 = TestCase $ assertError "The algorithm should throw an error if the input macros are not consisted of uppercase characters only" (prettyPrint invalidMacroName)  
                              
                       runTestTT $ TestList [
                            TestLabel "test1" test1,
                            TestLabel "test2" test2,
                            TestLabel "test3" test3,
                            TestLabel "test4" test4,
                            TestLabel "test5" test5,
                            TestLabel "test6" test6,
                            TestLabel "test7" test7,
                            TestLabel "test8" test8,
                            TestLabel "test9" test9,
                            TestLabel "test10" test10,
                            TestLabel "test11" test11,
                            TestLabel "test12" test12,
                            TestLabel "test13" test13,
                            TestLabel "replaceExpWithMacro" test14,
                            TestLabel "equalMacros" test15,
                            TestLabel "composedMacros" test16,
                            TestLabel "negativeLamVars" test17,
                            TestLabel "invalidMacroName" test18 ]

                       putStrLn "-------- End of test --------"
                       where 
                       assertError :: String -> String -> Assertion
                       assertError m f = do wasErrorThrown <- isLeft <$> (E.try $ E.evaluate f :: IO (Either E.SomeException String))
                                            assertBool m wasErrorThrown
                       
              
-- Tests the following cases:
--    base case functionality
--    gramatically incorrect input
--    repeated macro def
--    unclosed macro definitions
testChallengeIV :: IO ()
testChallengeIV = do  putStrLn "-------- Testing Challenge IV --------" 
                     
                      let test1 = TestCase $ assertEqual "" (Just lamExp1) (parseLamMacro lamText1)
                      let test2 = TestCase $ assertEqual "" (Just lamExp2) (parseLamMacro lamText2)
                      let test3 = TestCase $ assertEqual "" (Just lamExp3) (parseLamMacro lamText3)
                      let test4 = TestCase $ assertEqual "" (Just lamExp4) (parseLamMacro lamText4)
                      let test5 = TestCase $ assertEqual "" (Just lamExp5) (parseLamMacro lamText5)                  
                      let test6 = TestCase $ assertEqual "" (Just lamExp6) (parseLamMacro lamText6) 
                      let test7 = TestCase $ assertEqual "" (Just lamExp7) (parseLamMacro lamText7) 
                      let test8 = TestCase $ assertEqual "" (Just lamExp8) (parseLamMacro lamText8)
                      let test9 = TestCase $ assertEqual "" (Just lamExp9) (parseLamMacro lamText9) 
                      let test10 = TestCase $ assertEqual "" (Just lamExp10) (parseLamMacro lamText10)
                      let test11 = TestCase $ assertEqual "" (Just lamExp11) (parseLamMacro lamText11)
                      let test12 = TestCase $ assertEqual "" (Just lamExp12) (parseLamMacro lamText12)
                      let test13 = TestCase $ assertEqual "" (Just lamExp13) (parseLamMacro lamText13)

                      let test14 = TestCase $ assertEqual "The input is not gramatically correct and should not parse successfully" Nothing  (parseLamMacro " dnwdnwidowd")

                      let test15 = TestCase $ assertEqual "The input is not gramatically correct and should not parse successfully" Nothing (parseLamMacro "defF=\\x1->x1(defG=\\x1->x1inx1)in\\x2->x2")

                      let test16 = TestCase $ assertEqual "Each macro must be uniquely defined" Nothing  (parseLamMacro "def F = \\x1->x1 in def F = \\x2 -> x2 x1 in x1")

                      let test17 = TestCase $assertEqual "The algorithm should return Nothing when a macro contains free vars" Nothing (parseLamMacro "def F = x1 in F")

                      runTestTT $ TestList [
                            TestLabel "test1" test1,
                            TestLabel "test2" test2,
                            TestLabel "test3" test3,
                            TestLabel "test4" test4,
                            TestLabel "test5" test5,
                            TestLabel "test6" test6,
                            TestLabel "test7" test7,
                            TestLabel "test8" test8,
                            TestLabel "test9" test9,
                            TestLabel "test10" test10,
                            TestLabel "test11" test11,
                            TestLabel "test12" test12,
                            TestLabel "test13" test13,
                            TestLabel "notInGrammarTest" test14,
                            TestLabel "notInGrammarTest" test15,
                            TestLabel "repeatedMacroDef" test16,
                            TestLabel "macroBodyClosure" test17]

                      putStrLn "-------- End of test --------"

-- Tests the following cases:
--    base case functionality
--    negative lamvars
--    invalid macro name
testChallengeV :: IO ()
testChallengeV = do  putStrLn "-------- Testing Challenge V --------"
                     
                     let test1 = TestCase $ assertEqual "" lamCps1 (cpsTransform lamExp1)
                     let test2 = TestCase $ assertEqual "" lamCps2 (cpsTransform lamExp2)
                     let test3 = TestCase $ assertEqual "" lamCps3 (cpsTransform lamExp3)
                     let test4 = TestCase $ assertEqual "" lamCps4 (cpsTransform lamExp4)
                     let test5 = TestCase $ assertEqual "" lamCps5 (cpsTransform lamExp5)
                     let test6 = TestCase $ assertEqual "" lamCps6 (cpsTransform lamExp6)
                     let test7 = TestCase $ assertEqual "" lamCps7 (cpsTransform lamExp7)
                     let test8 = TestCase $ assertEqual "" lamCps8 (cpsTransform lamExp8)
                     let test9 = TestCase $ assertEqual "" lamCps9 (cpsTransform lamExp9)
                     let test10 = TestCase $ assertEqual "" lamCps10 (cpsTransform lamExp10)
                     let test11 = TestCase $ assertEqual "" lamCps11 (cpsTransform lamExp11)
                     let test12 = TestCase $ assertEqual "" lamCps12 (cpsTransform lamExp12)
                     let test13 = TestCase $ assertEqual "" lamCps13 (cpsTransform lamExp13)
                     let test14 = TestCase $ assertEqual "" lamCps14 (cpsTransform lamExp14)
                     let test15 = TestCase $ assertEqual "" lamCps15 (cpsTransform lamExp15)
                     let test16 = TestCase $ assertEqual "" lamCps16 (cpsTransform lamExp16)
                     let test17 = TestCase $ assertEqual "" lamCps17 (cpsTransform lamExp17)
                     let test18 = TestCase $ assertEqual "" lamCps18 (cpsTransform lamExp18)
                     let test19 = TestCase $ assertEqual "" lamCps19 (cpsTransform lamExp19)
                     let test20 = TestCase $ assertEqual "" lamCps20 (cpsTransform lamExp20)

                     let test21 = TestCase $ assertError "The algorithm should throw an error for negative LamVars" (cpsTransform  negativeLamVar)  
                     let test22 = TestCase $ assertError "The algorithm should throw an error if the input macros are not consisted of uppercase characters only" (cpsTransform invalidMacroName)  
                     
                     runTestTT $ TestList [
                            TestLabel "test1" test1,
                            TestLabel "test2" test2,
                            TestLabel "test3" test3,
                            TestLabel "test4" test4,
                            TestLabel "test5" test5,
                            TestLabel "test6" test6,
                            TestLabel "test7" test7,
                            TestLabel "test8" test8,
                            TestLabel "test9" test9,
                            TestLabel "test10" test10,
                            TestLabel "test11" test11,
                            TestLabel "test12" test12,
                            TestLabel "test13" test13,
                            TestLabel "test14" test14,
                            TestLabel "test15" test15,
                            TestLabel "test16" test16,
                            TestLabel "test17" test17,
                            TestLabel "test18" test18,
                            TestLabel "test19" test19,
                            TestLabel "test20" test20,
                            TestLabel "negativeLamVars" test21,
                            TestLabel "invalidMacroName" test22]
                     
                     putStrLn "-------- End of test --------"
                     where 
                     assertError :: String -> LamMacroExpr -> IO ()
                     assertError m f = do wasErrorThrown <- isLeft <$> (E.try $ E.evaluate f :: IO (Either E.SomeException LamMacroExpr))
                                          assertBool m wasErrorThrown
                     

-- Tests the following cases:
--    base case functionality
--    not enought steps
--    negative number of steps
--    unclosed macro definitions
--    negative lamvars
--    invalid macro name  
--    tests on the innerRedn1 and outerRedn1 for negative lamvars, invalid macro names, unclosed macro definitions and already reduced terms
--    endless loop test
testChallengeVI :: IO ()
testChallengeVI = do  putStrLn "-------- Testing Challenge VI --------"

                      let test1 = TestCase $ assertEqual "" lamEval1 (compareInnerOuter lamExp1 100)
                      let test2 = TestCase $ assertEqual "" lamEval2 (compareInnerOuter lamExp2 100)
                      let test3 = TestCase $ assertEqual "" lamEval3 (compareInnerOuter lamExp3 100)
                      let test4 = TestCase $ assertEqual "" lamEval4 (compareInnerOuter lamExp4 100) 
                      let test5 = TestCase $ assertEqual "" lamEval5  (compareInnerOuter lamExp5 100)
                      let test6 = TestCase $ assertEqual "" lamEval6 (compareInnerOuter lamExp6 100)
                      let test7 = TestCase $ assertEqual "" lamEval7 (compareInnerOuter lamExp7 100)
                      let test8 = TestCase $ assertEqual "" lamEval8 (compareInnerOuter lamExp8 100)
                      let test9 = TestCase $ assertEqual "" lamEval9 (compareInnerOuter lamExp9 100)
                      let test10 = TestCase $ assertEqual "" lamEval10 (compareInnerOuter lamExp10 100) 
                      let test11 = TestCase $ assertEqual "" lamEval11 (compareInnerOuter lamExp11 100)
                      let test12 = TestCase $ assertEqual "" lamEval12 (compareInnerOuter lamExp12 100)
                      let test13 = TestCase $ assertEqual "" lamEval13 (compareInnerOuter lamExp13 100)
                      let test14 = TestCase $ assertEqual "" lamEval14 (compareInnerOuter lamExp14 100)
                      let test15 = TestCase $ assertEqual "" lamEval15 (compareInnerOuter lamExp15 1)

                      let test16 = TestCase $ assertError "The number of steps cannot be negative" (compareInnerOuter lamExp16 (-10))
                      let test17 = TestCase $ assertError "The input LamMacro definitions must be closed" (compareInnerOuter lamExp16 10)
                      let test18 = TestCase $ assertError "The input LamVars can only be defined using natural numbers" (compareInnerOuter negativeLamVar 10)
                      let test19 = TestCase $ assertError "The input macro names must be represented using a string of uppercase characters!" (compareInnerOuter invalidMacroName 10)

                      let test20 = TestCase $ assertEqual "The input LamVars can only be defined using natural numbers" Nothing (innerRedn1 negativeLamVar)
                      let test21 = TestCase $ assertEqual "The input macro names must be represented using a string of uppercase characters!" Nothing (innerRedn1 invalidMacroName)
                      let test22 = TestCase $ assertEqual "The input LamMacro definitions must be closed" Nothing (innerRedn1 lamExp16)

                      let test23 = TestCase $ assertEqual "The input LamVars can only be defined using natural numbers" Nothing (outerRedn1 negativeLamVar)
                      let test24 = TestCase $ assertEqual "The input macro names must be represented using a string of uppercase characters!" Nothing (outerRedn1 invalidMacroName)
                      let test25 = TestCase $ assertEqual "The input LamMacro definitions must be closed" Nothing (outerRedn1 lamExp16)

                      let test26 = TestCase $ assertEqual "When the term is already reduced and is not possible to take a step of evaluation => we should return Nothing" Nothing (innerRedn1 (LamDef [] var1))
                      let test27 = TestCase $ assertEqual "When the term is already reduced and is not possible to take a step of evaluation => we should return Nothing" Nothing (outerRedn1 (LamDef [] var1))

                      let test28 = TestCase $ assertEqual "" lamEval16 (compareInnerOuter lamExp21 100)
                      let test29 = TestCase $ assertEqual "" lamEval17 (compareInnerOuter lamExp22 100) 
                      let test30 = TestCase $ assertEqual "" lamEval18 (compareInnerOuter lamExp23 100)
                      let test31 = TestCase $ assertEqual "The algorithm needs to be able to handle endless loops of evaluation" lamEval19 (compareInnerOuter lamExp24 100)
                      let test32 = TestCase $ assertEqual "" lamEval20 (compareInnerOuter lamExp25 100)
                      let test33 = TestCase $ assertEqual "" lamEval21 (compareInnerOuter lamExp26 100)
                      let test34 = TestCase $ assertEqual "The algorithm needs to be able to handle endless loops of evaluation" lamEval22 (compareInnerOuter lamExp27 1000)

                      runTestTT $ TestList [
                            TestLabel "test1" test1,
                            TestLabel "test2" test2,
                            TestLabel "test3" test3,
                            TestLabel "test4" test4,
                            TestLabel "test5" test5,
                            TestLabel "test6" test6,
                            TestLabel "test7" test7,
                            TestLabel "test8" test8,
                            TestLabel "test9" test9,
                            TestLabel "test10" test10,
                            TestLabel "test11" test11,
                            TestLabel "test12" test12,
                            TestLabel "test13" test13,
                            TestLabel "test14" test14,
                            TestLabel "notEnoughSteps" test15,
                            TestLabel "negativeSteps" test16,
                            TestLabel "unclosedMacroDef" test17,
                            TestLabel "negativeLamVar" test18,
                            TestLabel "invalidMacroName" test19,

                            TestLabel "negativeLamVar" test20,
                            TestLabel "invalidMacroName" test21,
                            TestLabel "unclosedMacroDef" test22,

                            TestLabel "negativeLamVar" test23,
                            TestLabel "invalidMacroName" test24,
                            TestLabel "unclosedMacroDef" test25,

                            TestLabel "alreadyReducedTerm" test26,
                            TestLabel "alreadyReducedTerm" test27,

                            TestLabel "ex6-1 (example from spec)" test28,
                            TestLabel "ex6-2 (example from spec)" test29,
                            TestLabel "ex6-3 (example from spec)" test30,
                            TestLabel "loopTest, ex6-4 (example from spec)" test31,
                            TestLabel "ex6-5 (example from spec)" test32,
                            TestLabel "ex6-6 (example from spec)" test33,
                            TestLabel "ex6-7 (example from spec)" test34]

                      putStrLn "-------- End of test --------"
                      where 
                      assertError :: String -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> IO ()
                      assertError m f = do wasErrorThrown <- isLeft <$> (E.try $ E.evaluate f :: IO (Either E.SomeException (Maybe Int, Maybe Int,Maybe Int,Maybe Int)))
                                           assertBool m wasErrorThrown
                       


negativeLamVar,invalidMacroName :: LamMacroExpr
negativeLamVar = LamDef [] (LamApp (LamAbs 1 (LamVar (-1))) exId)
invalidMacroName = LamDef [("X-=.", var1)] var1


exId, var1, wExp  :: LamExpr
exId =  LamAbs 1 (LamVar 1)
var1 = LamVar 1
wExp = LamAbs 1 (LamApp var1 var1)


lamExp1, lamExp2, lamExp3, lamExp4, lamExp5, lamExp6, lamExp7, lamExp8, lamExp9, lamExp10, lamExp11, lamExp12, lamExp13, lamExp14, lamExp15, lamExp16, lamExp17, lamExp18, lamExp19, lamExp20, lamExp21, lamExp22 , lamExp23, lamExp24, lamExp25, lamExp26, lamExp27  :: LamMacroExpr
lamExp1 = LamDef [] (LamApp exId exId)
lamExp2 = LamDef [] (LamAbs 1 (LamApp var1 exId))
lamExp3 = LamDef [] (LamAbs 1 (LamApp (LamApp var1 (LamVar 3)) exId))
lamExp4 = LamDef [] (LamAbs 1  (LamApp (LamApp var1 (LamAbs 3 (LamAbs 2 (LamVar 2)))) exId))
lamExp5 = LamDef [] (LamApp (LamApp (LamAbs 1 (LamVar 2)) var1) var1)
lamExp6 = LamDef [] (LamApp (LamApp (LamAbs 1 (LamVar 2)) var1) (LamApp (LamAbs 1 (LamVar 2)) var1))
lamExp7 = LamDef [] (LamApp (LamApp var1 var1) (LamApp var1 var1))
lamExp8 = LamDef [] (LamAbs 1 (LamApp (LamApp var1 var1) (LamApp var1 var1)))
lamExp9 = LamDef [] (LamAbs 1 (LamApp exId exId))
lamExp10 = LamDef [] (LamAbs 1 (LamApp exId var1))
lamExp11 = LamDef [] (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp var1 var1) (LamApp var1 var1)))))
lamExp12 = LamDef [] (LamApp (LamApp (LamApp var1 var1) (LamApp var1 var1)) (LamApp (LamApp var1 var1) (LamApp var1 var1)))
lamExp13 = LamDef [ ("F", exId ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
lamExp14 = LamDef [ ("F", exId ) ] (LamAbs 2 (LamApp exId (LamVar 2))) 
lamExp15 = LamDef [ ("F", exId), ("G", exId ) ] (LamAbs 2 (LamApp exId (LamVar 2))) 
lamExp16 = LamDef [ ("F", LamAbs 1 (LamMacro "G") ), ("G", var1 ) ] (LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 var1)))
lamExp17 = LamDef [] (LamApp (LamVar 1) (LamVar 2))
lamExp18 = LamDef [("F", exId)] (LamVar 2) 
lamExp19 = LamDef [("F", exId)] (LamMacro "F") 
lamExp20 = LamDef [("F", exId)] (LamApp (LamMacro "F") (LamMacro "F"))
lamExp21 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))
lamExp22 = LamDef [ ("F",exId) ] (LamMacro "F")
lamExp23 = LamDef [] (LamApp exId (LamAbs 2 (LamVar 2)))
lamExp24 = LamDef [] (LamApp wExp wExp)
lamExp25 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))
lamExp26 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))
lamExp27 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 

lamEval1, lamEval2, lamEval3, lamEval4, lamEval5, lamEval6, lamEval7, lamEval8, lamEval9, lamEval10, lamEval11, lamEval12, lamEval13, lamEval14, lamEval15, lamEval16, lamEval17, lamEval18, lamEval19, lamEval20, lamEval21, lamEval22 :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
lamEval1 = (Just 1, Just 1, Just 8, Just 8)
lamEval2 = (Just 0, Just 0, Just 6, Just 6)
lamEval3 = (Just 0, Just 0, Just 9, Just 9)
lamEval4 = (Just 0, Just 0, Just 9, Just 9)
lamEval5 = (Just 1, Just 1, Just 11, Just 11)
lamEval6 = (Just 2, Just 2, Just 17, Just 17)
lamEval7 = (Just 0, Just 0, Just 11, Just 11)
lamEval8 = (Just 0, Just 0, Just 12, Just 12)
lamEval9 = (Just 1, Just 1, Just 8, Just 8)
lamEval10 = (Just 1, Just 1, Just 8, Just 8)
lamEval11 = (Just 0, Just 0, Just 12, Just 12)
lamEval12 = (Just 0, Just 0, Just 23, Just 23)
lamEval13 = (Just 1, Just 1, Just 7, Just 7)
lamEval14 = (Just 2, Just 2, Just 9, Just 9)
lamEval15 = (Nothing, Nothing, Nothing, Nothing)
lamEval16 = (Just 0, Just 0, Just 6, Just 6)
lamEval17 = (Just 1, Just 1,Just 3,Just 3)
lamEval18 = (Just 1,Just 1,Just 8,Just 8)
lamEval19 = (Nothing,Nothing,Nothing,Nothing)
lamEval20 = (Just 4,Just 4,Just 22,Just 22)
lamEval21 = (Just 4,Just 3,Just 21,Just 21)
lamEval22 = (Nothing,Just 4,Nothing,Nothing)

lamText1, lamText2, lamText3, lamText4, lamText5, lamText6, lamText7, lamText8, lamText9, lamText10, lamText11, lamText12, lamText13, lamText14, lamText15, lamText16  :: String
lamText1 = "(\\x1 -> x1) \\x1 -> x1" 
lamText2 = "\\x1 -> x1 \\x1 -> x1"
lamText3 = "\\x1 -> x1 x3 \\x1 -> x1"
lamText4 = "\\x1 -> x1 (\\x3 -> \\x2 -> x2) \\x1 -> x1"
lamText5 = "(\\x1 -> x2) x1 x1"
lamText6 = "(\\x1 -> x2) x1 ((\\x1 -> x2) x1)"
lamText7 = "x1 x1 (x1 x1)"
lamText8 = "\\x1 -> x1 x1 (x1 x1)"
lamText9 = "\\x1 -> (\\x1 -> x1) \\x1 -> x1"
lamText10 = "\\x1 -> (\\x1 -> x1) x1"
lamText11 = "\\x1 -> \\x2 -> \\x3 -> x1 x1 (x1 x1)"
lamText12 = "x1 x1 (x1 x1) (x1 x1 (x1 x1))"
lamText13 = "def F = \\x1 -> x1 in \\x2 -> x2 F"
lamText14 = "def F = \\x1 -> x1 in \\x2 -> F x2"
lamText15 = "def F = \\x1 -> x1 in def G = F in \\x2 -> F x2"
lamText16 = "def F = \\x1 -> G in def G = x1 in \\x2 -> x2 F"

lamCps1, lamCps2, lamCps3, lamCps4, lamCps5, lamCps6, lamCps7, lamCps8, lamCps9, lamCps10, lamCps11, lamCps12, lamCps13, lamCps14, lamCps15, lamCps16, lamCps17, lamCps18, lamCps19, lamCps20 :: LamMacroExpr
lamCps1 = LamDef [] (LamAbs 2 (LamApp (LamAbs 5 (LamApp (LamVar 5) (LamAbs 1 (LamAbs 6 (LamApp (LamVar 6) (LamVar 1)))))) (LamAbs 3 (LamApp (LamAbs 7 (LamApp (LamVar 7) (LamAbs 1 (LamAbs 8 (LamApp (LamVar 8) (LamVar 1)))))) (LamAbs 4 (LamApp (LamApp (LamVar 3) (LamVar 4)) (LamVar 2)))))))
lamCps2 = LamDef [] (LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamVar 1))) (LamAbs 4 (LamApp (LamAbs 7 (LamApp (LamVar 7) (LamAbs 1 (LamAbs 8 (LamApp (LamVar 8) (LamVar 1)))))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3))))))))))
lamCps3 = LamDef [] (LamAbs 4 (LamApp (LamVar 4) (LamAbs 1 (LamAbs 5 (LamApp (LamAbs 8 (LamApp (LamAbs 11 (LamApp (LamVar 11) (LamVar 1))) (LamAbs 9 (LamApp (LamAbs 12 (LamApp (LamVar 12) (LamVar 3))) (LamAbs 10 (LamApp (LamApp (LamVar 9) (LamVar 10)) (LamVar 8))))))) (LamAbs 6 (LamApp (LamAbs 13 (LamApp (LamVar 13) (LamAbs 1 (LamAbs 14 (LamApp (LamVar 14) (LamVar 1)))))) (LamAbs 7 (LamApp (LamApp (LamVar 6) (LamVar 7)) (LamVar 5))))))))))
lamCps4 = LamDef [] (LamAbs 4 (LamApp (LamVar 4) (LamAbs 1 (LamAbs 5 (LamApp (LamAbs 8 (LamApp (LamAbs 11 (LamApp (LamVar 11) (LamVar 1))) (LamAbs 9 (LamApp (LamAbs 12 (LamApp (LamVar 12) (LamAbs 3 (LamAbs 13 (LamApp (LamVar 13) (LamAbs 2 (LamAbs 14 (LamApp (LamVar 14) (LamVar 2))))))))) (LamAbs 10 (LamApp (LamApp (LamVar 9) (LamVar 10)) (LamVar 8))))))) (LamAbs 6 (LamApp (LamAbs 15 (LamApp (LamVar 15) (LamAbs 1 (LamAbs 16 (LamApp (LamVar 16) (LamVar 1)))))) (LamAbs 7 (LamApp (LamApp (LamVar 6) (LamVar 7)) (LamVar 5)))))))))) 
lamCps5 = LamDef [] (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamAbs 9 (LamApp (LamVar 9) (LamAbs 1 (LamAbs 10 (LamApp (LamVar 10) (LamVar 2)))))) (LamAbs 7 (LamApp (LamAbs 11 (LamApp (LamVar 11) (LamVar 1))) (LamAbs 8 (LamApp (LamApp (LamVar 7) (LamVar 8)) (LamVar 6))))))) (LamAbs 4 (LamApp (LamAbs 12 (LamApp (LamVar 12) (LamVar 1))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3)))))))
lamCps6 = LamDef [] (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamAbs 9 (LamApp (LamVar 9) (LamAbs 1 (LamAbs 10 (LamApp (LamVar 10) (LamVar 2)))))) (LamAbs 7 (LamApp (LamAbs 11 (LamApp (LamVar 11) (LamVar 1))) (LamAbs 8 (LamApp (LamApp (LamVar 7) (LamVar 8)) (LamVar 6))))))) (LamAbs 4 (LamApp (LamAbs 12 (LamApp (LamAbs 15 (LamApp (LamVar 15) (LamAbs 1 (LamAbs 16 (LamApp (LamVar 16) (LamVar 2)))))) (LamAbs 13 (LamApp (LamAbs 17 (LamApp (LamVar 17) (LamVar 1))) (LamAbs 14 (LamApp (LamApp (LamVar 13) (LamVar 14)) (LamVar 12))))))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3)))))))
lamCps7 = LamDef [] (LamAbs 2 (LamApp (LamAbs 5 (LamApp (LamAbs 8 (LamApp (LamVar 8) (LamVar 1))) (LamAbs 6 (LamApp (LamAbs 9 (LamApp (LamVar 9) (LamVar 1))) (LamAbs 7 (LamApp (LamApp (LamVar 6) (LamVar 7)) (LamVar 5))))))) (LamAbs 3 (LamApp (LamAbs 10 (LamApp (LamAbs 13 (LamApp (LamVar 13) (LamVar 1))) (LamAbs 11 (LamApp (LamAbs 14 (LamApp (LamVar 14) (LamVar 1))) (LamAbs 12 (LamApp (LamApp (LamVar 11) (LamVar 12)) (LamVar 10))))))) (LamAbs 4 (LamApp (LamApp (LamVar 3) (LamVar 4)) (LamVar 2)))))))
lamCps8 = LamDef [] (LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamAbs 9 (LamApp (LamVar 9) (LamVar 1))) (LamAbs 7 (LamApp (LamAbs 10 (LamApp (LamVar 10) (LamVar 1))) (LamAbs 8 (LamApp (LamApp (LamVar 7) (LamVar 8)) (LamVar 6))))))) (LamAbs 4 (LamApp (LamAbs 11 (LamApp (LamAbs 14 (LamApp (LamVar 14) (LamVar 1))) (LamAbs 12 (LamApp (LamAbs 15 (LamApp (LamVar 15) (LamVar 1))) (LamAbs 13 (LamApp (LamApp (LamVar 12) (LamVar 13)) (LamVar 11))))))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3))))))))))
lamCps9 = LamDef [] (LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamAbs 1 (LamAbs 7 (LamApp (LamVar 7) (LamVar 1)))))) (LamAbs 4 (LamApp (LamAbs 8 (LamApp (LamVar 8) (LamAbs 1 (LamAbs 9 (LamApp (LamVar 9) (LamVar 1)))))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3))))))))))
lamCps10 = LamDef [] (LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamAbs 1 (LamAbs 7 (LamApp (LamVar 7) (LamVar 1)))))) (LamAbs 4 (LamApp (LamAbs 8 (LamApp (LamVar 8) (LamVar 1))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3))))))))))
lamCps11 = LamDef [] (LamAbs 4 (LamApp (LamVar 4) (LamAbs 1 (LamAbs 5 (LamApp (LamVar 5) (LamAbs 2 (LamAbs 6 (LamApp (LamVar 6) (LamAbs 3 (LamAbs 7 (LamApp (LamAbs 10 (LamApp (LamAbs 13 (LamApp (LamVar 13) (LamVar 1))) (LamAbs 11 (LamApp (LamAbs 14 (LamApp (LamVar 14) (LamVar 1))) (LamAbs 12 (LamApp (LamApp (LamVar 11) (LamVar 12)) (LamVar 10))))))) (LamAbs 8 (LamApp (LamAbs 15 (LamApp (LamAbs 18 (LamApp (LamVar 18) (LamVar 1))) (LamAbs 16 (LamApp (LamAbs 19 (LamApp (LamVar 19) (LamVar 1))) (LamAbs 17 (LamApp (LamApp (LamVar 16) (LamVar 17)) (LamVar 15))))))) (LamAbs 9 (LamApp (LamApp (LamVar 8) (LamVar 9)) (LamVar 7))))))))))))))))
lamCps12 = LamDef [] (LamAbs 2 (LamApp (LamAbs 5 (LamApp (LamAbs 8 (LamApp (LamAbs 11 (LamApp (LamVar 11) (LamVar 1))) (LamAbs 9 (LamApp (LamAbs 12 (LamApp (LamVar 12) (LamVar 1))) (LamAbs 10 (LamApp (LamApp (LamVar 9) (LamVar 10)) (LamVar 8))))))) (LamAbs 6 (LamApp (LamAbs 13 (LamApp (LamAbs 16 (LamApp (LamVar 16) (LamVar 1))) (LamAbs 14 (LamApp (LamAbs 17 (LamApp (LamVar 17) (LamVar 1))) (LamAbs 15 (LamApp (LamApp (LamVar 14) (LamVar 15)) (LamVar 13))))))) (LamAbs 7 (LamApp (LamApp (LamVar 6) (LamVar 7)) (LamVar 5))))))) (LamAbs 3 (LamApp (LamAbs 18 (LamApp (LamAbs 21 (LamApp (LamAbs 24 (LamApp (LamVar 24) (LamVar 1))) (LamAbs 22 (LamApp (LamAbs 25 (LamApp (LamVar 25) (LamVar 1))) (LamAbs 23 (LamApp (LamApp (LamVar 22) (LamVar 23)) (LamVar 21))))))) (LamAbs 19 (LamApp (LamAbs 26 (LamApp (LamAbs 29 (LamApp (LamVar 29) (LamVar 1))) (LamAbs 27 (LamApp (LamAbs 30 (LamApp (LamVar 30) (LamVar 1))) (LamAbs 28 (LamApp (LamApp (LamVar 27) (LamVar 28)) (LamVar 26))))))) (LamAbs 20 (LamApp (LamApp (LamVar 19) (LamVar 20)) (LamVar 18))))))) (LamAbs 4 (LamApp (LamApp (LamVar 3) (LamVar 4)) (LamVar 2)))))))
lamCps13 = LamDef [("F",LamAbs 3 (LamApp (LamVar 3) (LamAbs 1 (LamAbs 4 (LamApp (LamVar 4) (LamVar 1))))))] (LamAbs 5 (LamApp (LamVar 5) (LamAbs 2 (LamAbs 6 (LamApp (LamAbs 9 (LamApp (LamVar 9) (LamVar 2))) (LamAbs 7 (LamApp (LamMacro "F") (LamAbs 8 (LamApp (LamApp (LamVar 7) (LamVar 8)) (LamVar 6))))))))))
lamCps14 = LamDef [("F",LamAbs 3 (LamApp (LamVar 3) (LamAbs 1 (LamAbs 4 (LamApp (LamVar 4) (LamVar 1))))))] (LamAbs 5 (LamApp (LamVar 5) (LamAbs 2 (LamAbs 6 (LamApp (LamAbs 9 (LamApp (LamVar 9) (LamAbs 1 (LamAbs 10 (LamApp (LamVar 10) (LamVar 1)))))) (LamAbs 7 (LamApp (LamAbs 11 (LamApp (LamVar 11) (LamVar 2))) (LamAbs 8 (LamApp (LamApp (LamVar 7) (LamVar 8)) (LamVar 6))))))))))
lamCps15 = LamDef [("F",LamAbs 3 (LamApp (LamVar 3) (LamAbs 1 (LamAbs 4 (LamApp (LamVar 4) (LamVar 1)))))),("G",LamAbs 3 (LamApp (LamVar 3) (LamAbs 1 (LamAbs 4 (LamApp (LamVar 4) (LamVar 1))))))] (LamAbs 5 (LamApp (LamVar 5) (LamAbs 2 (LamAbs 6 (LamApp (LamAbs 9 (LamApp (LamVar 9) (LamAbs 1 (LamAbs 10 (LamApp (LamVar 10) (LamVar 1)))))) (LamAbs 7 (LamApp (LamAbs 11 (LamApp (LamVar 11) (LamVar 2))) (LamAbs 8 (LamApp (LamApp (LamVar 7) (LamVar 8)) (LamVar 6))))))))))
lamCps16 = LamDef [("F",LamAbs 3 (LamApp (LamVar 3) (LamAbs 1 (LamMacro "G")))),("G",LamAbs 3 (LamApp (LamVar 3) (LamVar 1)))] (LamAbs 4 (LamApp (LamVar 4) (LamAbs 2 (LamAbs 5 (LamApp (LamAbs 8 (LamApp (LamVar 8) (LamVar 2))) (LamAbs 6 (LamApp (LamAbs 9 (LamApp (LamVar 9) (LamAbs 1 (LamAbs 10 (LamApp (LamVar 10) (LamVar 1)))))) (LamAbs 7 (LamApp (LamApp (LamVar 6) (LamVar 7)) (LamVar 5))))))))))
lamCps17 = LamDef [] (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamVar 1))) (LamAbs 4 (LamApp (LamAbs 7 (LamApp (LamVar 7) (LamVar 2))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3)))))))
lamCps18 = LamDef [("F",LamAbs 3 (LamApp (LamVar 3) (LamAbs 1 (LamAbs 4 (LamApp (LamVar 4) (LamVar 1))))))] (LamAbs 5 (LamApp (LamVar 5) (LamVar 2)))
lamCps19 = LamDef [("F",LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 3 (LamApp (LamVar 3) (LamVar 1))))))] (LamMacro "F")
lamCps20 = LamDef [("F",LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 3 (LamApp (LamVar 3) (LamVar 1))))))] (LamAbs 4 (LamApp (LamMacro "F") (LamAbs 5 (LamApp (LamMacro "F") (LamAbs 6 (LamApp (LamApp (LamVar 5) (LamVar 6)) (LamVar 4)))))))

words1, words2, words3, words4, words5:: [String]
words1 = ["HASKELL","STRING","STACK","MAIN","METHOD"]
words2 = ["BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]
words3 = ["MAILO", "BABA", "HELLO", "DMWDKDKWNDKW"]
words4 = ["JAVASCRIPT", "PYTHON", "C#", "C++", "SWIFT"]
words5 = ["123", "x2y", "415", "637"]

solution1, solution2, solution3, solution4, solution5 :: [(String, Maybe Placement)]
solution1 = [("HASKELL",Just ((0,0),DownForward)),("STRING",Just ((7,0),Back)),("STACK",Just ((2,2),Forward)),("MAIN",Just ((2,7),Up)),("METHOD",Just ((4,3),Down))]
solution2 = [("BANANA",Just ((5,6),UpBack)),("ORANGE",Just ((1,0),DownForward)),("MELON",Just ((7,8),Up)),("RASPBERRY",Just ((8,0),DownBack)),("APPLE",Just ((2,8),UpForward)),("PLUM",Just ((5,1),DownBack)),("GRAPE",Just ((8,6),Up))]
solution3 = [("MAILO",Just ((4,2),DownBack)),("BABA",Just ((9,3),Back)),("HELLO",Just ((13,7),Down)), ("DMWDKDKWNDKW", Nothing)]
solution4 = [("JAVASCRIPT",Just ((47,8),DownBack)),("PYTHON",Just ((37,44),DownBack)),("C#",Just ((39,3),Down)),("C++",Just ((7,25),Back)),("SWIFT",Just ((47,13),DownBack))]
solution5 = [("123",Just ((1,2),Up)),("x2y",Just ((2,1),Back)),("415",Just ((0,2),Forward)),("637",Just ((0,0),Forward))]

grid1, grid2, grid3, grid4, grid5 :: WordSearchGrid
grid1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
grid2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
grid3 = ["YNPHEPGWBCICFQA", "HYMREXABKRVMPTT", "PVSGMBVUBSYLDUF", "JRRAJQABABQORII", "PJIYHFASMULKNFN", "ELOJTZZZJPQRRYE", "OLIKUWIOZNZFUJA", "LCTKFIITBLOQWHV", "ESHYZFCSCUYSBEN", "EZODESGNVSUNBLI", "EAZPIAARXIIJJLH", "LELSMJMHBFLXWOY","YPLKNNYEHDQDUTY","AWINGJOBEHZLJYP","HIWNOKFFQVGOLDH"] 
grid5 = ["637","Y2X","415"]
grid4 = ["YOHSRNCFNYCYCITRINRRFI+TT++A#+OJ+SROPSJWWFROIHHCSNV",
         "CAYJSSRTJAHWHPHJFYTNTTIHJIIPN+TRIFH+WWY#ORSOPA+YNSC",
         "PROJT#NI+SJPA#TT#SSJ#ICFWVRWIHICNFIIYJSJNHWWV##JCAP",
         "WHFHYO++WTAFPNJNTSCIIVYFT#VAPRIPI+##JWVCOWJSTJISACV",
         "+VONOSFSCRRTSCY+OVRHPIFR#PVFFAJPRHYHOSO#IJWYRJFOYTI",
         "HTAVS+WWIJPSN+OT#JOCF#RFYPOVWPNV+IRIJ+WSWV+VRFAA+YC",
         "TNP+ICRPJIYOPCFW+PNSRFR#JH+SAJHVYISNSO+ASTOJVVWWANN",
         "OFA#V+ROFWSRWTF+WCTSCOHWVHNVWO#OAYHPTFHSH#WI+YYSJOS",
         "YRFRRAYWV+P+IT#SVJINVCHWRVCHI+TTPNRH#WPIHAJJ#NOJP+J",
         "WJHCN+NPCANWIRRR+THFIWI#HSF+TNHFCTFFJHPSARIITTAHR+#",
         "CONPANOHHPWHNYTYYVJJIF+YV+JOWTPTA+VCAF+NNYNCNVOSVP#",
         "NFFFCAPVJ+#+T#SSSSVNHNHYAR+I+TNAATPCNNTWF#TJATRVOVP",
         "+CHIJJNJWOYYWJAIPOTCPHANRWHHJJVPNFTNOHC+OFASHASSRCS",
         "WVFFRCFIJJPAAJPJTWYWCCPAVFRWWIPCHCCTWSOYIJCW+ARSFHH",
         "SOPTNTVWS#WJAWYFYF#NVFOCYFHNWR+OWCCPRSSVARRNP+WAHRC",
         "AFWFOO#+OI+WTRFNVOSSCHVIIVCH#RVHPP+HJ+OHITYPIIJOANP",
         "#PTRYH#WPWORCCCSOY+VCRRIFPIJTPFJO#PAJRYPFRFAFTJP+A+",
         "VNOYPROONCASTCFAC+STHHAYFOPIVTYWHSFIO+TV#TWTNVAVYI+",
         "HNIOFAY+FSIANIJYOCSN#S+RCRTAOPTSN#VOTAP#ASIOITSNA#Y",
         "FC+#WCSVITAVSWNVA+FVONATHTOYNPJJRW#NCH+POTAFPOCWVOV",
         "NAJWWSRJNYCRP#SNSCCCNCCICHRYFPWAOFPTSWWTVCP#NFPCYSA",
         "W#TFPNYRT+HCJWTFRWH+IFIYSTYFN+VP+VFSTSWCFWYRA#RNNJ+",
         "JFONTRIWP#VHJNVTJ+SRNJTASJIYPWNYPJPSFTNYIAH#NVTRJAR",
         "SIAOOJNHTYFCYWONFTSCP+S+#I+PPY+ASOAFY+#ARWHNONPFI#+",
         "JVA+SANVI+SYVT#YYA+OI#FJF#HSW#SSJJPWSRASOAIAIONVVFY",
         "N##WF++CJICTFVNFVJYN+NOFNRN#YTTWPYF+HRROAVHW#WFVHN+",
         "WTHJWCC+HIISHTNWST++IOYOOTORNRTPJAPTPOTYOWOW+PFACAV",
         "TSSI+IAAJHTTHP+CCFHP#FT+CJPOONFPIVCJOYIPOTPRYPA+WVA",
         "+JVPPJPVA#PO#OONRJ#FAWHPPIAIIJPHYY+AW+RSOVJ+AFJFH+O",
         "YO+PWYVAR+JJJPVPYHTSYJST+JTFRTCAVPFHRRANHYCSVYAYTRP",
         "SWNPANCRAOFNN#NSRVPFPO#AHNNSPJSSAIJIHN#APS+IIYJW+PP",
         "CAS#JR+CSSWRRPIJVSTAPYNACS+TOINRFCH#RN#AVOWHRTT#YOP",
         "+YSTI#WVVCSCRFFSINWWHCCVRY+H+CV#HROFNAAVWCFNAOJ+RRY",
         "NPSNPOTOJ+NOJHOO+V#FOFFWFR#OCHNFSVP#JVHSSARJ+P+ICNI",
         "YPSNRWO+TIAIHRW+FIJJFASJSYYROOI#PCRFP#NNWOANYVRNV+A",
         "WSRRFHORIYH#+J#YTOTOYNPFFPATW+I+NICPOHOCJCNPWYNJIVF",
         "##+VPSIRFWATWWSRO#TNNYASWVHI##VPOA+WAR+J+FASVCVVRF+",
         "PP+A##HARHNHWV+T+YHJNCAO#NYAI+FCIHVFSICWF#THI+OYJNA",
         "NJJWT++WCACIT++SAOYPOIJVAIFWOACAT+IAYHJCIJSOSAFCPFC",
         "YVHVIOJAJVCPWA+NJRNAJAAWFFYPNRVTPO#PW#OCAYNFYISVJYY",
         "ATARNY#F#TISHFJJH+IT#OOAYWPICIOJNONOW+ISJTVACIWNJYF",
         "N+FYTAITSO#HFVAIITTHY#OHRSCH+RRFARISAVOHF+WHTTJCV+A",
         "FCNC+OPPJSRF+OVJ+A#NAYW#+FYOISORYPRRRTCYYFRTFARVCH+",
         "SH++TOA#VVVA#HRNNORP+IPJRNRJHCWNVJSYVIRNYWCAPWYWNPY",
         "RCWACWYFJRJ#FFJ+SJYRVJHAJJJY+CO#OIFWAPVHHPHPVOCFOHI",
         "OTWROOTTYSTS+COIINIHFAIFPS+HJA+A#NOWYJTR#OPAAV+JAIJ",
         "VNRHSRRTWYAJNHSFNJ+VIJJSYVJFIIFSTTVTWNJTPFVRRANYHIJ",
         "OTHAYFIACJVJ+CYVO#N#TSFIYHR#+NVFCSHIIHPF+CT##PNVNFA",
         "WHASF+RAJHYJTOPORYOPRIRWFATRJCCTOONHAWHIYJ+RROCNRCT",
         "TJV+#RRY+SWP#WI+JIOCWPHTOO+FHFH+NCSCYW#+VNWFCRNJVJP",
         "HHJAHT+TP+AVAT++F+IIJFJVFSSIRYRIY+PAV#WAWHIOJYSYSCF"]





