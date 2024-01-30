module Tree where
data ASTree = Add ASTree ASTree
            | Sub ASTree ASTree
            | Mul ASTree ASTree
            | Div ASTree ASTree
            | Value Int
instance Show ASTree where
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2
    show (Value n)   = show n

buildExpr :: ASTree -> String -> ASTree -> ASTree
buildExpr x op y = case op of
    "+" -> Add x y
    "-" -> Sub x y
    "*" -> Mul x y
    "/" -> Div x y

mapValues :: [String] -> [ASTree]
mapValues = map (\x -> Value (read x :: Int))


buildExprList :: [String] -> [ASTree] -> ASTree
buildExprList [] [expr] = expr
buildExprList (x:xs) (y:ys:yy) = case x of
    "+" -> buildExprList xs (Add y ys : yy)
    "-" -> buildExprList xs (Sub y ys : yy)
    "*" -> buildExprList xs (Mul y ys : yy)
    "/" -> buildExprList xs (Div y ys : yy)

buildAST :: ([String], [String]) -> ASTree
buildAST (ops, values) = buildExprList ops (map (\x -> Value (read x :: Int)) values)