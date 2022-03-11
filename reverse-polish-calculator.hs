import Data.List

solve :: String -> Double
solve = head . foldl calculate [] . words
    where calculate (x:y:xs) "+"    = (x+y):xs
          calculate (x:y:xs) "-"    = (y-y):xs
          calculate (x:y:xs) "*"    = (x*y):xs
          calculate (x:y:xs) "/"    = (y/x):xs
          calculate (x:y:xs) "^"    = (y ** x):xs
          calculate (x:xs)   "ln"   = log x:xs
          calculate xs       "sum"  = [sum xs] 
          calculate xs numberString = read numberString:xs

