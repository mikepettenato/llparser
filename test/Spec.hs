module Main (main) where

import Test.HUnit
import System.Exit
import Parser
import Data.Char
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)

-- Functor tests
testFunctor = TestCase (assertEqual "parse (fmap toUpper item) \"foo\"" ([('F', "oo")]) (parse (fmap toUpper item) "foo"))
testFunctorEmptyString = TestCase (assertEqual "parse (fmap toUpper item) \"\"" ([]) (parse (fmap toUpper item) ""))

-- Applicative tests
testApplicative = TestCase (assertEqual "parse (pure toLower <*> item) \"test\"" ([('t', "EST")]) (parse (pure toLower <*> item) "TEST"))

-- Monad tests
testMonad = TestCase (assertEqual "parse (item >>= (\\s -> pure toUpper s)) \"test\"" ([('T', "est")]) (parse (item >>= (\s -> return $ toUpper s)) "test"))

-- Parser tests
testItem = TestCase (assertEqual "parse item \"test\"" ([('t', "est")]) (parse item "test"))
testEmptyItem = TestCase (assertEqual "parse item \"\"" ([]) (parse item ""))
testSat = TestCase (assertEqual "parse (sat isUpper) \"Test\"" ([('T', "est")]) (parse (sat isUpper) "Test"))
testSatFail = TestCase (assertEqual "parse (sat isUpper) \"test\"" ([]) (parse (sat isUpper) "test"))
testLower = TestCase (assertEqual "parse lower \"test\"" ([('t', "est")]) (parse lower "test"))
testLowerFail = TestCase (assertEqual "parse lower \"Test\"" ([]) (parse lower "Test"))
testUpper = TestCase (assertEqual "parse upper \"Test\"" ([('T', "est")]) (parse upper "Test"))
testUpperFail = TestCase (assertEqual "parse upper \"test\"" ([]) (parse upper "test"))
testSpace = TestCase (assertEqual "parse space \"   \"" ([((), "")]) (parse space "   "))
testPosint = TestCase (assertEqual "parse posint \"123\"" ([(123, "")]) (parse posint "123"))
testNegint = TestCase (assertEqual "parse negint \"-123\"" ([(-123, "")]) (parse negint "-123"))
testVarint = TestCase (assertEqual "parse (intvar (H.fromList([(\"var\", 1)]))) \"var\"" ([(1, "")]) (parse (intvar (H.fromList([("var", 1)]))) "var"))
testIntPos = TestCase (assertEqual "parse int \"123\"" ([(123, "")]) (parse (int H.empty) "123"))
testIntNeg = TestCase (assertEqual "parse int \"-123\"" ([(-123, "")]) (parse (int H.empty) "-123"))
testSpecificChar = TestCase (assertEqual "parse (char 'c') \"char\"" ([('c', "har")]) (parse (char 'c') "char"))
testSpecificCharFail = TestCase (assertEqual "parse (char 'c') \"hcar\"" ([]) (parse (char 'c') "hcar"))
testSpecificString = TestCase (assertEqual "parse (string \"str\") \"str\"" ([("str", "")]) (parse (string "str") "str"))
testSpecificStringPrefix = TestCase (assertEqual "parse (string \"str\") \"strtest\"" ([("str", "test")]) (parse (string "str") "strtest"))
testSpecificStringFail = TestCase (assertEqual "parse (string \"tstr\") \"tstr\"" ([]) (parse (string "str") "tstr"))
testIntToken = TestCase (assertEqual "parse (token int) \"  123  \"" [(123,"")] (parse (token (int H.empty)) "   123   "))
testPlusToken = TestCase (assertEqual "parse (symbol \"+\") \"  +  \"" [("+","")] (parse (symbol "+") "   +   "))
testLetOp = TestCase (assertEqual "parse (letop (H.empty)) \"  +  \"" [(2,"")] (parse (letop (H.empty)) "   let a = 1 ; a + 1"))

-- Interpreter tests
testPlus = TestCase(assertEqual "1 + 1" ([(2::Int, "")]) (apply (expr H.empty) "1 + 1"))
testComplexMath = TestCase(assertEqual " 1 - 2 * 3 + 4" ([(-1::Int, "")]) (apply (expr H.empty) " 1 - 2 * 3 + 4"))
testDivision = TestCase(assertEqual " 8/4" ([(2, "")]) (apply (expr H.empty) "8/4"))
testAdditionWithDivision = TestCase(assertEqual "2+8/4" ([(4, "")]) (apply (expr H.empty) "2+8/4"))
testLetExpr = TestCase(assertEqual "let a = 10;a+1" ([(11, "")]) (apply (expr H.empty) "let a = 10;a+1"))
testLetExprMultiVar = TestCase(assertEqual "let a = 10;a+1+a" ([(21, "")]) (apply (expr H.empty) "let a = 10;a+1+a"))

main :: IO ()
main = do
  counts <- runTestTT (test
    [
      -- Test the functor implementation
      testFunctor,
      testFunctorEmptyString,

      -- Test the applicative implementation
      testApplicative,

      -- test Monad implementation
      testMonad,

      -- Test the parser implementation
      testItem,
      testEmptyItem,
      testSat,
      testSatFail,
      testLower,
      testLowerFail,
      testUpper,
      testUpperFail,
      testSpace,
      testPosint,
      testIntPos,
      testIntNeg,
      testVarint,
      testSpecificChar,
      testSpecificCharFail,
      testSpecificStringPrefix,
      testSpecificStringFail,
      testIntToken,
      testPlusToken,
      testLetOp,

      -- Test expressions
      testPlus,
      testComplexMath,
      testDivision,
      testAdditionWithDivision,
      testLetExpr,
      testLetExprMultiVar
    ])
  if (errors counts + failures counts == 0) then
    exitSuccess
  else
    exitFailure
