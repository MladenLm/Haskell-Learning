
xs :: [Integer]
xs = [1,2,3,4,5]

t = [x + 2 | x <- xs]

ys :: [Integer]
ys = [1..]

factorial :: Integer  -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)


doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = 2*x + 2*y


doubleSmallNumber :: (Ord p, Num p) => p -> p
doubleSmallNumber x = (if x < 100 then x * 2 else x) + 1

hailStone :: Integer -> Integer
hailStone n
    | even n    = n `div` 2
    | otherwise = 3 * n + 1



hailStone' :: Integral a => a -> a
hailStone' n = if even n then n `div` 2 else 3 * n + 1

factorial' :: Integer -> Integer
factorial' n
    | n == 0 = 1
    | otherwise  = n * factorial (n - 1)

hailStone'' :: Integral a => a -> a
hailStone'' n
    | even n = n `div` 2
    | otherwise = 3 * n + 1

factorial''' :: (Eq p, Num p) => p -> p
factorial''' n
    | n == 0 = 1
    | otherwise = n * factorial''' (n - 1)


someTuple :: (Integer, String)
someTuple = (4, "hi")

sumPair :: Num a => (a, a) -> a
sumPair (a, b) = a + b

comTy = ((1,2), (3,4))

funComTy ((a, b), (c, d)) = (a+b, c+d)

hailStoneNumbers :: (Eq a, Num a, Integral a) => a -> [a]
hailStoneNumbers 1 = [1]
hailStoneNumbers n = n : hailStoneNumbers (hailStone'' n)


sumEveryTwo :: (Num a) => [a] -> [a]
sumEveryTwo [] = []
sumEveryTwo [x] = [x]
sumEveryTwo (x:y:xs) = (x + y) : sumEveryTwo xs

sumIntList :: [Int] -> Int
sumIntList [] = 0
sumIntList [x] = x
sumIntList (x:xs) = x + sumIntList xs

take' :: Int -> [Int] -> [Int]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

hailStoneNumbersLength :: Integer -> Int
hailStoneNumbersLength = length . hailStoneNumbers

asc :: (Ord t, Num t) => t -> t -> [t]
asc n m
    | m < n = []
    | n == m = [m]
    | otherwise  = n : asc (n + 1) m

evens :: Integral a => [a] -> [a]
evens [] = []
evens (x:xs)
    | even x = x : evens xs
    | otherwise = evens xs

addTuples :: Num a => [(a, a)] -> [a]
addTuples xs = [x + y | (x, y) <- xs]

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = e == x || e `elem` xs

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs)
    | x `elem'` xs = nub' xs
    | otherwise = x : nub' xs



isAsc' :: Ord a => [a] -> Bool
isAsc' [] = True
isAsc' [x] = True
isAsc' (x:y:xs) = x <= y && isAsc' (y:xs)

type Id = Int

lookAtThis :: Id -> Id
lookAtThis x = x * x

data List a = Empty | Cons a (List a)
    deriving Show

tellBmi :: (Ord a, Fractional a) => a -> a -> [Char]
tellBmi height weight
    | bmi < 18 = "ok"
    | bmi < 25 = "skinny"
    | otherwise = "wow"
    where bmi = height / weight ^ 2

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName

initials' :: String -> String -> String
initials' firstName' lastName' = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstName'
          (l:_) = lastName'



taken :: (Eq t, Num t) => t -> [a] -> [a]
taken 0 _ = []
taken n [] = []
taken n (x:xs) = x : taken (n - 1) xs


taken' :: (Eq t, Num t) => t -> [a] -> [a]
taken' 0 xs = []
taken' n [] = []
taken' n (x:xs) = x : taken' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]



myBmi :: (Ord a, Fractional a) => a -> a -> String
myBmi weight height
    | bmi < 18 = "skinny"
    | bmi < 24 = "ok"
    | otherwise = "fat"
    where bmi = weight / height ^ 2

card :: Integral a => [a] -> a
card xs = sum $ doubleDigits $ lookCard $ reverse xs

validCard :: Integral a => [a] -> Bool
validCard xs
    | card xs `mod` 10 == 0 = True
    | otherwise = False

lookCard :: Integral a => [a] -> [a]
lookCard (x:y:xs) = x : y * 2 : lookCard xs
lookCard [x] = [x]
lookCard _ = []


doubleDigits :: Integral a => [a] -> [a]
doubleDigits [] = []
doubleDigits (x:xs)
    | x >= 10 = (x `mod` 10 + x `div` 10) : doubleDigits xs
    | otherwise = x : doubleDigits xs

add1 :: Integer -> Integer
add1 x = x + 1

r :: [Integer] -> [Integer]
r = map (\x -> x + 1)

e :: [(Integer, Integer)] -> [Integer]
e = map (uncurry (+))

w = filter (> 2)

sp :: Eq a => [(a, a)] -> [(a, a)]
sp = filter (uncurry (/=))

cylinder :: Floating a => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

cylinder' :: Floating a => a -> a -> a
cylinder' r h = sideArea + 2 * topArea
    where sideArea = 2 *pi *r * h
          topArea = pi * r ^ 2

m' = (let (a,b,c) = (1,2,3) in a+b+c) * 100

descriebList :: [a] -> String
descriebList xs = "The list is" ++ case xs of  []  -> " empty"
                                               [x] -> " singelton"
                                               xs -> " a longer list"

descriebList' xs = "The list is " ++ what xs
    where what []  = "empty"
          what [x] = "singelton"
          what xs = "longer list"


maximum' :: Ord a => [a] -> a
maximum' []  = error "No max of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' :: Ord a => [a] -> a
maximum'' [] = error "Empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Ord t, Num t) => t -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n - 1) x


take'' :: (Ord t, Num t) => t -> [a] -> [a]
take'' _ [] = []
take'' n _
    | n <= 0 = []
take'' n (x:xs) =  x : take'' (n - 1) xs

elem'' :: Eq t => t -> [t] -> Bool
n `elem''` [] = False
n `elem''` (x:xs)
    | n == x = True
    | otherwise = n `elem''` xs


quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let smallerSorted = quickSort' [a | a <- xs, a <= x]
        biggerSorted  = quickSort' [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted


com = negate . (* 3)


com2 :: [Integer]
com2 = map (negate . abs) [1,2,3]

com3 :: [Integer] -> [Integer]
com3 = map (negate . abs)

class Show' a where
    show' :: a -> String

instance Show' a => Show' (List a) where
    show' Empty       = "Empty"
    show' (Cons a xs) = "Cons" ++ show' a ++ show' xs

instance Show' Int where
    show' x = show x

-- creating datatype for rationals (like 1/5) 
data Q = Q Int Int

instance Show Q where
    show (Q nominator denominator) = concat [show nominator, "/", show denominator]

simpQ :: Q -> Q
simpQ (Q nominator denominator) = Q (nominator `div` c) (denominator `div` c)
    where c = gcd nominator denominator

instance Eq Q where
    r1 == r2 = nominator1 == nominator2 && denominator1 == denominator2
        where (Q nominator1 denominator1) = simpQ r1
              (Q nominator2 denominator2) = simpQ r2

addQ :: Q -> Q -> Q
addQ (Q n1 d1) (Q n2 d2) = simpQ $ Q (n1' + n2') m
    where m   = lcm d1 d2
          n1' = n1 * (m `div` d1)
          n2' = n2 * (m `div` d2)

multQ :: Q -> Q -> Q
multQ (Q n1 d1) (Q n2 d2) = simpQ $ Q (n1 * n2) (d1 * d2)

instance Num Q where
    (+)                              = addQ
    negate (Q nominator denominator) = Q (-nominator) denominator
    (*)                              = multQ
    abs (Q nominator denominator)    = Q (abs nominator) (abs denominator)
    signum (Q nominator denominator) = Q (signum nominator * signum denominator) 1



-- Cross between data and type
-- Makes a constructor wrapper around another type and we use it for perforance reasons
-- Allows us to define different typeclass definitions on our new types

newtype RevString = RevString String

instance Show RevString where
    show (RevString s) = reverse s

st :: String
st = "hello world"
st' :: RevString
st' = RevString st

data Color = Red
           | Black
           | Green
           | Blue
           | Pink
    deriving (Show, Eq)

isLukeSaber :: Color -> Bool
isLukeSaber Green = True
isLukeSaber _ = False

isLukeSaber' :: Color -> Bool
isLukeSaber' color
    | color == Green = True
    | otherwise = False

data FailableDouble = Failure | OK Double
    deriving Show

failToZero :: FailableDouble -> Double
failToZero Failure = 0.0
failToZero (OK n) = n

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv n d = OK (n / d)

data Person = Person String Int Color
    deriving Show

getAge :: Person -> Int
getAge (Person _ a _) = a

data AlgDataType = Constr1 Int Int
                 | Constr2 Int Int Int
                 | Constr3 Int
                 | Constr4

data PersonOrColor = CombinedPerson Person | CombinedColor Color
    deriving Show

colorToString :: Color -> String
colorToString = show

personNameOrColorName :: PersonOrColor -> String
personNameOrColorName (CombinedPerson (Person a _ _)) = a
personNameOrColorName (CombinedColor a)               = colorToString a

whatIsNameField :: Person -> String
whatIsNameField p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

whatIsNameField' :: Person -> [Char]
whatIsNameField' p = "This is " ++ show p

quickSort'' :: Ord a => [a] -> [a]
quickSort'' [] = []
quickSort'' (x:xs) =
    let smallerSorted = [a | a <- xs, a <= x]
        biggerSorted  = [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

data IntList = Conss Int IntList | Nil
    deriving Show

intListProd :: IntList -> Int
intListProd Nil = 1
intListProd (Conss x Nil) = x
intListProd (Conss x xs) = x * intListProd xs

data BinaryTree = Leaf Int | Node'' Int BinaryTree BinaryTree
    deriving (Show, Eq)

data BinaryTreeDataAtLeaves = Leaf' Int | Node' BinaryTreeDataAtLeaves BinaryTreeDataAtLeaves

dummyTree :: BinaryTree
dummyTree = Node'' 5 (Node'' 4 (Leaf 3) (Leaf 2)) (Node'' 6 (Leaf 7) (Leaf 8))

data BinarTree = EmptyL | Node Int BinarTree BinarTree

emptyLeafExample :: BinarTree
emptyLeafExample = Node 2 (Node 3 EmptyL EmptyL) EmptyL

checkEx :: BinarTree -> Int -> Bool
checkEx EmptyL _ = False
checkEx (Node x leftTree rightTree) element = x == element
                                                || checkEx leftTree element
                                                || checkEx rightTree element

data RoseTree = RoseTree Int [RoseTree] | EmptyRose
    deriving Show

dummyRose :: RoseTree
dummyRose = RoseTree 5 [RoseTree 4 [],
                        RoseTree 3 [],
                        RoseTree 2 [RoseTree 6 [], RoseTree 7 []],
                        RoseTree 1 [],
                        RoseTree 0 []]


data MessageType = Info
             | Warning
             | Error Int
             deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String | Unknows String
    deriving (Show, Eq)

-- parseLog :: String -> [LogMessage]


fold :: (a -> b -> b) -> b -> [a] -> b
fold cons empty []     = empty
fold cons empty (x:xs) = x `cons` fold cons empty xs

data Set a = EmptySet | Sing a | Union (Set a) (Set a)
    deriving (Show, Eq)


foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b
foldSet e s u EmptySet    = e
foldSet e s u (Sing x)    = s x
foldSet e s u (Union x y) = (foldSet e s u x) `u` (foldSet e s u y)


isIn :: Eq a => a -> Set a -> Bool
isIn x = foldSet False (==x) (||)

testSet :: Set Integer
testSet = Union (Sing 1) (Union (Sing 2) EmptySet)

data PeaNum = Succ PeaNum | Zero
    deriving Show

data Calculation = Add Int Int 
                 | Sub Int Int 
                 | Mul Int Int 
                 | Div Int Int
                 deriving Show
                
calc :: Calculation -> Int
calc (Add x y) = x + y
calc (Sub x y) = x - y
calc (Mul x y) = x * y
calc (Div x y) = x `div` y
