{-# LANGUAGE LambdaCase #-}

module Parser (parseLambda, parseLine) where

import Code
import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isLower, isUpper)
import Lambda

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Nothing -> Nothing
    Just (a, rs) -> Just (f a, rs)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  Parser pf <*> Parser pa = Parser $ \s -> case pf s of
    Nothing -> Nothing
    Just (f, rs) -> case pa rs of
      Nothing -> Nothing
      Just (a, rs') -> Just (f a, rs')

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \s -> case p s of
    Nothing -> Nothing
    Just (a, rs) -> parse (f a) rs

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \s -> case p1 s of
    Nothing -> p2 s
    r -> r

instance MonadPlus Parser

-- comb primitives

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  (c : rs) | f c -> Just (c, rs)
  _ -> Nothing

charP :: Char -> Parser Char
charP c = satisfy (== c)

variableP :: Parser String
variableP = some (satisfy isLower)

macroP :: Parser String
macroP = some (satisfy (\c -> isUpper c || isDigit c))

-- comb grammar

atomP :: Parser Lambda
atomP =
  parens lambdaP
    <|> (Var <$> variableP)
    <|> (Macro <$> macroP)
  where
    parens p = charP '(' *> p <* charP ')'

-- lambda
appP :: Parser Lambda
appP = do
  first <- atomP
  rest <- many (charP ' ' *> atomP)
  return (foldl App first rest)

-- lambda <|>
absP :: Parser Lambda
absP = do
  _ <- charP '\\'
  v <- variableP
  _ <- charP '.'
  body <- lambdaP
  return (Abs v body)

lambdaP :: Parser Lambda
lambdaP = absP <|> appP

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda s = case parse lambdaP s of
  Just (l, _) -> l
  Nothing -> error ("Error parser: " ++ s)

-- 3.3.
-- <|> (Eval <$> lambdaP)
bindingP :: Parser Line
bindingP = do
  name <- macroP
  _ <- charP '='
  body <- lambdaP
  return (Binding name body)

lineP :: Parser Line
lineP = bindingP <|> (Eval <$> lambdaP)

parseLine :: String -> Either String Line
parseLine s = case parse lineP s of
  Just (l, "") -> Right l
  Just (_, rs) -> Left ("trail error: " ++ rs)
  Nothing -> Left "parse error"
