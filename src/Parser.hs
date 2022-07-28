module Parser where

-- This module implements a monadic LL parser.
-- The implementation is inspired by the following document
-- https://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
-- The goal is to build parsers that can be used
-- as combinators for more complicated parsers.
-- As desribed in the above mentioned documentation, we can
-- avoid the lexical phase by defining suitable combinators, which
-- handle things like spaces between tokens.

-- This Parse implements the following grammar
-- as defined in the above mentioned document.
--
--  expr ::= expr addop term | term | letop
--  term ::= term mulop factor | factor
--  factor ::= number | ( expr )
--  letop ::= letsym var equalsym (integer
--  addop ::= + | -
--  mulop ::= * | /
--  appsep := ;
--  letsym ::= 'let'
--  equalsym ::= =
--  var ::= [a-zA-Z]+
-- number ::= <integer> | var -> <integer>

import Control.Monad
import Control.Applicative
import Debug.Trace (trace)
import Data.Char
import Control.Monad.State
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)

type Env = H.HashMap String Int

traceMonad :: (Show a, Monad m) => String -> a -> m a
traceMonad prefix x = trace (prefix ++ ": " ++ show x) (return x)


-- Parser takes a function taking a string
-- and returning a list of pairs (a, String).
-- For all intents and purposes the list will
-- have a length of one, indicating a valid parse
-- and zero indicating an invalid parse
newtype Parser a = Parser (String -> [(a, String)])

plus :: Parser Char
plus = Parser (\cs -> case cs of
                          "" -> []
                          (x:xs) -> if x == '+'  then [(x, xs)] else [])

-- basic parser that takes the first char
-- from a string and returns it as the first element of
-- a pair and the rest of the string as a list as the
-- second element of the pair
item :: Parser Char
item = Parser (\cs -> case cs of
                        "" -> []
                        (x:xs) -> [(x, xs)])

-- parser that takes a predicate (isUpper, isNumber, etc...)
-- and parsers the char is the predicate is true otherwise
-- it returns a Parser with an empty list
sat :: (Char -> Bool) -> Parser Char
sat p = item >>= (\x ->
  case p x of
    True -> return x
    otherwise -> Parser(\_->[]))

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

-- Parser to read zero or more spaces.
space :: Parser ()
space = many (sat isSpace) >> return ()

-- building blocks for integers
-- Parser deals with positive integers
-- some reads 1 or more digits
-- read function makes 'd' in int.
posint :: Parser Int
posint =
  do d <- some (sat isDigit)
     return (read d)

-- building blocks for integers
-- Parser deals with negative integers
negint :: Parser Int
negint = char '-' >> posint >>= (\d -> return (-d))

-- Parser combines the posint and negint parsers
-- to parse either negative or positive integers
int :: Env -> Parser Int
int env = posint <|> negint <|> intvar env

-- building blocks for integers
-- Parser deals with environment variable 
-- substitution for variables used that have
-- integer values in the environment.
intvar :: H.HashMap String Int -> Parser Int
intvar env =
  var >>= (\r ->
      let val = H.lookup r env
      in case val of
        Just a -> (return a)
        otherwise ->  Parser(\s -> []))

-- Parser for a specific char, c
char :: Char -> Parser Char
char c = sat (c ==)

-- Parser for specific string.
-- Built by leveraging the char parser
string :: String -> Parser String
string "" = return ""
string (c:cs) = char c >> string cs >> return (c:cs)

-- Parser that decorates other parsers to handle
-- beginning and ending spaces
token :: Parser a -> Parser a
token p = space >> p >>= (\a -> space >> return a)

-- Parser for symbols, i.e. +, -, *, etc...
symbol :: String -> Parser String
symbol s = token (string s)

var :: Parser String
var =
  do d <- some (sat isAlpha)
     return (read ("\"" ++ d ++ "\"")::String)

equalsym :: Parser String
equalsym = symbol "="

-- deconstructs the parser
parse :: (Parser a) -> String -> [(a, String)]
parse (Parser a) s = a s

-- Functor implemenation for Parser
-- impements fmap
instance Functor Parser where
  --fmap :: (a->b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser (\s ->
    case p s of
      [] -> []
      [(x, xs)] -> [(f x, xs)])

-- Applicative implementation for Parser
-- implementing pure, and <*> (application)
instance Applicative Parser where
  pure a = Parser (\cs -> [(a, cs)])
  (Parser pf) <*> (Parser pa) =
    Parser (\cs -> case pf cs of
                     [] -> []
                     [(x, xs)] -> parse (fmap x (Parser pa)) xs)

-- Monad implementation for Parser.
-- implements return and >>= (bind)
instance Monad Parser where
  return = pure
  p >>= f = Parser (\cs -> case parse p cs of
                            [] -> []
                            [(x, xs)] -> parse (f   x) xs)

-- Alternative is a subclass of Applicative that
-- has to define the 'empty' '<|>'
-- '<|>' can be used to run two parsers in parallel.
-- We us the result of the first on if it succeeds else
-- we use the second one.
instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser (\s -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser (\s -> case parse p s of
                          []        -> parse q s
                          [(v,out)] -> [(v,out)])

-- Parse recursively Parser p, separated by applications of
-- Parser op.  The values are assumed to assocated left
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a}
    where
      rest a = (do f <- op
                   b <- p
                   rest (f a b)) <|> return a

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = p `chainl1` op <|> return a

-- Apply a Parser p, ignoring leading spaces
apply :: Parser a -> String -> [(a, String)]
apply p = parse (do { space ; p })

-- Below, our haskell based grammar can be implemented
-- using parsers and combinators as building blocks
--
-- Note: The BNF implemented is defined at the top of
-- Parser.hs

-- Uses chainl1 to implement left recursive rules
expr :: Env -> Parser Int
--expr = term `chainl1` addop
expr env = (term env) `chainl1` addop <|> letop env
-- expr = letop

letop env = symbol "let" >>
  var >>= (\r1 ->
  (symbol "=") >>= (\r2 ->
  int env >>= (\r3 ->
  (symbol ";") >>= (\r4 ->
    let newEnv = H.insert r1 r3 env
    in expr newEnv))))

addop :: Parser (Int -> Int -> Int)
addop = do {symbol "+"; return (+)} <|> do {symbol "-"; return (-)}

mulop :: Parser (Int -> Int -> Int)
mulop = do {symbol "*"; return (*)} <|> do {symbol "/"; return (div)}

-- Uses chainl1 to implement left recursive rules
term env = factor env `chainl1` mulop

factor env = int env <|> do {symbol "("; n <- (expr env); symbol ")"; return n}
