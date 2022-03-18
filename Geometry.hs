

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
import System.IO
import Prelude
import Control.Monad (when)


sphereVolume :: Float -> Float
sphereVolume radius = 4.0 * 3.0 * pi * radius ^ 3

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * radius ^2

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

capitals' :: String -> [Int]
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

data Person = Person { firstName :: String, lastName :: String, age :: Int, color :: Color}
    deriving (Eq, Read)

instance Show Person where
    show p =
        firstName p ++ " " ++ lastName p ++ " of age " ++ show (age p) ++ " with favorite color " ++ show (color p)

data Color = Black | Green | Blue | Red
    deriving (Show, Eq, Read)

flush = hFlush stdout

createPerson =
    putStrLn "What is you first name?" >> getLine >>= \firstName ->
        putStrLn "What is your last name?" >> getLine >>= \lastName ->
            putStrLn "What is your age?" >> getLine >>= \age ->
                putStrLn "What is your favorite color?" >> getLine >>= \color ->
                    pure Person
                        {
                              firstName = firstName
                            , lastName = lastName
                            , age = read age :: Int
                            , color = read color :: Color
                        }

main :: IO ()
main = createPerson >>= print


oddOrEven :: (Integral a, Foldable t) => t a -> String
oddOrEven xs
  | odd summed = "odd"
  | otherwise  = "even"
  where summed = sum xs

croquet [] = []
croquet (x:xs)
    | fst x < 50 && snd x > 7 = ["open"] : croquet xs
    | otherwise = ["Senior"] : croquet xs

{-
openOrSenior :: [(Int, Int)] -> [String]
openOrSenior [] = []
openOrSenior (x:xs)
    | fst x < 50 && snd x > 7 = "Open" : openOrSenior xs
    | otherwise = "Senior" : openOrSenior xs
-}

data Membership = Open | Senior deriving (Eq, Show)
openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior [] = []
openOrSenior [x]
    | fst x >= 55 && snd x > 7 = [Senior]
    | otherwise =  [Open]
openOrSenior (x:xs)
    | fst x >= 55 && snd x > 7 = Senior : openOrSenior xs
    | otherwise = Open : openOrSenior xs

repeater :: String -> Int -> String
repeater string n
    | n <= 0    = []
    | otherwise = string ++ repeater string (n - 1)

repeater' string n = concat $ replicate n string

solve :: String -> String
solve str
  | astr str == bstr str   = map toUpper str
  | otherwise = map toLower str

astr :: String -> Int
astr str = length $ filter isUpper str
bstr :: String -> Int
bstr str = length $ filter isLower str

friend :: [String] -> [String]
friend = filter (\x -> length x == 4)

--sequenceSum :: Int -> String
--sequenceSum :: Int -> [String]
--sequenceSum 

createList :: Int -> [Int]
createList n = reverse $ take (n + 1) [n, n - 1..]

listOfString :: Int -> [String]
listOfString n = map show (createList n)

stringWithPlus :: [String] -> String
stringWithPlus [] = []
stringWithPlus (x:xs) = x ++ "+" ++ stringWithPlus xs

listwithouth :: [a] -> [a]
listwithouth = init

listWithEq xs = xs ++ " = "

seqt = listWithEq . listwithouth . stringWithPlus . listOfString

newSeqt n
    | n < 0 = show n ++ " < 0"
    | otherwise = seqt n ++ show (sum (createList n))

data Base = A | T | G | C
    deriving (Eq, Show)

type DNA = [Base]

dnaStrand :: DNA -> DNA
dnaStrand = dnaStrand1

dnaStrand1 :: [Base] -> [Base]
dnaStrand1 [] = []
dnaStrand1 (x:xs)
    | x == A = T : dnaStrand1 xs
    | x == T = A : dnaStrand1 xs
    | x == G = C : dnaStrand1 xs
    | x == C = G : dnaStrand1 xs
    | otherwise = xs

longest :: Ord a => [a] -> [a] -> [a]
longest s1 s2 = sort $ filterdoubles $ s1 ++ s2

filterdoubles :: Eq a => [a] -> [a]
filterdoubles = nub

mxdiflg :: [String] -> [String] -> Maybe Int
mxdiflg s1 s2
    | null s1 && null s2 = Nothing
    | otherwise = Just $ abs $ length (s1 !! 1) - length (s2 !! 1)


mxdiflg' :: [String] -> [String] -> Maybe Int
mxdiflg' [] _ = Nothing
mxdiflg' _ [] = Nothing
mxdiflg' s1 s2 =  Just (maximum [abs(length a - length b) | a <- s1, b <- s2])

-- | Returns true if the number of
-- Xs is equal to the number of Os
-- (case-insensitive)
--xo :: String -> Bool
xo :: [Char] -> Bool
xo str
    | not hasNoX && not hasNoO         = True
    | length amountX == length amountO = True
    | otherwise                        = False
    where hasNoX  = 'x' `elem` map toLower str
          hasNoO  = 'o' `elem` map toLower str
          amountX = filter (\x -> x == 'x') $ (map toLower str)
          amountO = filter (\x -> x == 'o') $ (map toLower str)

