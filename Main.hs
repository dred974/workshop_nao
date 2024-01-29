module Main where

import Operations
import System.Environment (getArgs)

performOperations :: Int -> String -> Int -> Int
performOperations x "+" y = Operations.addition x y;
performOperations x "-" y = Operations.substraction x y;
performOperations x "*" y = Operations.multiply x y;
performOperations x "/" y = case Operations.divide x y of
    Just res -> res
    Nothing -> error "Cannot divide by zero"


main :: IO ()
main = do
    input <- getLine

    let [xStr, op, yStr] = words input

    let x = read xStr :: Int
        y = read yStr :: Int

    putStrLn $ "Result: " ++ show (performOperations x op y)
