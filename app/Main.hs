module Main where

import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Parser
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)

type Env = H.HashMap String Int

main :: IO ()
main = do
  putStr "Calc> "
  hFlush stdout
  line <- getLine
  if line == "bye" then
    do putStrLn "Bye!"
       return ()
  else
    case apply (expr H.empty) line of
      [(a, "")] -> do print a
                      main
      otherwise -> do print "Expression syntax error.  Please try again."
                      main
