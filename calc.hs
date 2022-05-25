import Data.List

triangular :: Int -> Int
triangular x = x * (x + 1) `div` 2

triSeries :: Int -> [Int]
triSeries x = map triangular [1..x]

sunTriNumbers n =  take (fromInteger n) [sum [1..x] | x <- [1..]]

-- getUnique [1, 1, 1, 2, 1, 1]

unique_alt :: [Int] -> [Int]
unique_alt [] = []
unique_alt (x:xs)
    | x `elem` unique_alt xs = unique_alt xs
    | otherwise                = x : unique_alt xs

getUnique xs = head (unique_alt xs)