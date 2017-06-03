module Lib
    ( repl
    , malRead
    , malShow
    ) where

import System.IO (stdout, hFlush)
import Control.Monad (forever)

import Text.Parsec.String (Parser)
import Text.Parsec (anyChar, parse, many, many1, manyTill, digit, oneOf, noneOf, choice, char, string)
import Data.List (intercalate)

data MalExp = MalNumber Int
  | MalString String
  | MalSymbol String
  | MalList [MalExp]
  deriving Show

repl :: IO ()
repl = do
  forever loop
  where
    loop :: IO ()
    loop = do
      putStr "user> "
      hFlush stdout
      userInput <- getLine
      putStrLn $ rep userInput

    rep :: String -> String
    rep input = either show malShow (malRead input)

malShow :: MalExp -> String
malShow (MalList subexps) = "(" ++ (intercalate " " $ map malShow subexps) ++ ")"
malShow (MalNumber i)     = show i
malShow (MalSymbol sym)   = sym
malShow (MalString s)     = s

malRead = parse parseMalExp ""
  where
    specialChars = "[]{}()'`~^@"
    spaceChars = " ,"

    parseInt :: Parser Int
    parseInt = read <$> (many1 digit)

    ignoreSpace :: Parser [Char]
    ignoreSpace = many $ oneOf spaceChars

    parseMalNumber :: Parser MalExp
    parseMalNumber = MalNumber <$> parseInt

    parseMalList = do
      char '('
      ignoreSpace
      exprs <- many parseMalExp
      ignoreSpace
      char ')'
      return $ MalList exprs

    parseMalSymbol :: Parser MalExp
    parseMalSymbol = MalSymbol <$> many1 (noneOf $ specialChars ++ spaceChars)

    parseMalExp :: Parser MalExp
    parseMalExp = do
      ignoreSpace
      exp <- choice [parseMalList, parseMalString, parseMalNumber, parseMalSymbol]
      ignoreSpace
      return exp

    parseMalString :: Parser MalExp
    parseMalString = do
      char '"'
      strs <- many character
      char '"'
      return $ MalString $ concat strs
      where
        {- fmap return turns Parser Char in Monad m => Parser m Char which.
         - since escapeQuote is Parser [Char] the m becomes [] -}
        character = choice [(fmap return notEscapedQuote), escapedQuote]
        escapedQuote = string "\\\""
        notEscapedQuote :: Parser Char
        notEscapedQuote = noneOf "\\\""

