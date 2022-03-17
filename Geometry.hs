

module Geometry
(
    sphereVolume,
    sphereArea,
    cubeVolume,
    cubeArea,
    cuboidArea,
) where

import Data.Char
import Data.List


sphereVolume :: Float -> Float
sphereVolume radius = (4.0 * 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

countSheep :: [Bool] -> Int
countSheep = foldl (\acc x -> if x then acc + 1 else acc) 0

fakeBin :: String -> String
fakeBin = map (\x -> if x < '5' then '0' else '1')


capitals :: String -> [Int]
capitals [] = []
capitals xs = checkIfNum $ lettersWitNum xs

lettersWitNum :: (Num b, Enum b) => [a] -> [(a, b)]
lettersWitNum str = zip str [1..]

checkIfNum :: [(Char, Int)] -> [Int]
checkIfNum [] = []
checkIfNum (x:xs)
  | isUpper (fst x) = snd x : checkIfNum xs
  | otherwise       = checkIfNum xs

capitals' :: [Char] -> [Int]
capitals' = findIndices isUpper


perfectSq :: (Ord a, Num a, Show a) => a -> Bool
perfectSq x
    | x < 0           = False
    | intToString x  = True
    | otherwise       = False


intToString :: Show a => a -> Bool
intToString x
    | show x == ['5'] = False
    | show x == ['6'] = False
    | last (show x) == '1' = True
    | last (show x) == '4' = True
    | last (show x) == '5' = True
    | last (show x) == '6' = True
    | last (show x) == '9' = True
    | last (show x) == '0' = True
    | otherwise            = False