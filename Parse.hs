module Parse where
import Predicates
filterArr :: (a -> Bool) -> [a] -> [a]
filterArr _ [] = []
filterArr predicates (x:xs)
  | predicates x = x : filterArr predicates xs
  | otherwise   = filterArr predicates xs



parseLine :: [String] -> ([String], [String])
parseLine t = (filterArr isOperator t, filterArr (not . isOperator) t)
