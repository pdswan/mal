module Lib
    ( loop
    , malReadEvalPrint
    , malReadPrint
    , malRead
    , malEval
    , malPrint
    , MalEnv(MalEnv)
    , MalExp(..)
    , MalReplError(EvalError)
    ) where

import System.IO (stdout, hFlush)
import Control.Monad (forever, mapM, join)

import Text.Parsec.String (Parser)
import Text.Parsec (anyChar, parse, many, many1, manyTill, digit, oneOf, noneOf, choice, char, string)
import qualified Text.Parsec.Error (ParseError)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

type MalFn = [MalExp] -> Either MalReplError MalExp
data MalEnv = MalEnv (Map String MalFn)

data MalExp = MalNumber Int
  | MalString String
  | MalSymbol String
  | MalList [MalExp]
  deriving Show

data MalReplError = EnvError String
  | EvalError String
  | ParseError Text.Parsec.Error.ParseError
  deriving Show

loop :: (String -> String) -> IO ()
loop f = do
  forever go
  where
    go :: IO ()
    go = do
      putStr "user> "
      hFlush stdout
      userInput <- getLine
      putStrLn $ f userInput

malEnvLookup :: String -> MalEnv -> Either MalReplError MalFn
malEnvLookup key (MalEnv map) = case (Map.lookup key map) of
  Nothing -> Left $ EnvError ("Symbol not found " ++ key)
  (Just f) -> Right f

malReadEvalPrint :: MalEnv -> String -> String
malReadEvalPrint malEnv input = do
  either show malPrint (read input >>= eval)
  where
    eval :: MalExp -> Either MalReplError MalExp
    eval = malEval malEnv

    read :: String -> Either MalReplError MalExp
    read input = case (malRead input) of
      (Left e)   -> Left $ ParseError e
      (Right ok) -> return ok

malEval :: MalEnv -> MalExp -> Either MalReplError MalExp
malEval malEnv (MalList ((MalSymbol sym):exps)) = do
  f <- malEnvLookup sym malEnv
  join $ f <$> (mapM (malEval malEnv) exps)
malEval _ exp         = return exp

malReadPrint :: String -> String
malReadPrint input = either show malPrint (malRead input)

malPrint :: MalExp -> String
malPrint (MalList subexps) = "(" ++ (intercalate " " $ map malPrint subexps) ++ ")"
malPrint (MalNumber i)     = show i
malPrint (MalSymbol sym)   = sym
malPrint (MalString s)     = "\"" ++ s ++ "\""

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

