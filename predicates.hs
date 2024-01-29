module Predicates where

import Data.Char (isDigit)

isOperator :: String -> Bool
isOperator str = str `elem` ["+", "-", "*", "/"]

isNumber :: String -> Bool
isNumber str = all isDigit str