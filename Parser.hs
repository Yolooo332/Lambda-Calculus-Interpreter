module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Code

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda = undefined

-- 3.3.
parseLine :: String -> Either String Line
parseLine = undefined
