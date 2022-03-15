{-# LANGUAGE GeneralizedNewtypeDeriving #-}

myXor :: [Bool] -> Bool
myXor xs = not helper
    where helper = even . length $ filter (== True) xs

--sieveSundarm :: Int -> [Int]

class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    toList x = [x]

-- Folding with trees
-- Folding a list of elements to build a balanced binary tree
data Tree a
  = Leaf
  | Node Int (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Int
height Leaf = 0
height (Node h _ _ _) = h

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node h left _ right) = abs (height left - height right) <= 1 && isBalanced left && isBalanced right

leftLean :: Tree a -> Bool
leftLean Leaf = False
leftLean (Node h left _ right) = height left > height right || leftLean left || leftLean right

-- tilt :: Tree a -> Int
-- tilt Leaf = 0
-- tilt (Node h left _ right) = 
--   | height left - height right == 0

insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree input Leaf = Node 1 Leaf input Leaf
insertIntoTree input curNode@(Node h left value right)
  | isBalanced curNode = Node (h + 1) (insertIntoTree input left) value right
  | leftLean curNode = Node h left value (insertIntoTree input right)
  | otherwise = Node h (insertIntoTree input left) value right

insertIntoTree' :: a -> Tree a -> Tree a
insertIntoTree' input Leaf =
    let left = Leaf
        right = Leaf
        h1 = max (height left) (height right) + 1
     in Node h1 left input right
insertIntoTree' input curNode@(Node h left value right)
    | height left > height right =
        let left1 = left
            right1 = insertIntoTree' input right
            h1 = max (height left1) (height right1) + 1
        in Node h1 left1 value right1
    | otherwise =
        let left1 = insertIntoTree' input left
            right1 = right
            h1 = max (height left1) (height right1) + 1
        in Node h1 left1 value right1


foldTree :: [a] -> Tree a
foldTree = foldr insertIntoTree' Leaf

result :: Bool
result = isBalanced (foldTree [1, 2, 3, 4, 5, 6, 7, 8, 9])


danielXor :: [Bool] -> Bool
danielXor = foldr (/=) False

data ListWithLen a = ListWithLen [a] Int
    deriving Show

instance Semigroup (ListWithLen a) where
  (ListWithLen list1 len1) <> (ListWithLen list2 len2) = ListWithLen (list1 <> list2) (len1 + len2)

fromHaskellList :: [a] -> ListWithLen a
fromHaskellList list = ListWithLen list (length list)

data Color = Red
           | Green
           | Blue
           | Yellow
           | Black
           | Clear
           | Orange
    deriving Show

instance Semigroup Color where
    Red <> Red     = Red
    Red <> Yellow  = Orange
    Yellow <> Red  = Orange
    Yellow <> Blue = Green
    Blue <> Yellow = Green
    x <> Clear     = x
    _ <> Black     = Black
    -- This works if we have all cases cover now, but if we add a new constructor it will fail 
    a <> b         = b <> a

-- EXAMPLE HOW IS IMPLEMENTED
--instance Semigroup a => Semigroup (Maybe a) where
--    Just x <> Just y = Just (x <> y)
--    _ <> _           = Nothing

newtype Name = Name String
    deriving Show

newtype Phone = Phone Int
    deriving (Show, Num, Ord, Eq)

newtype Sum a = Sum a
    deriving (Show, Eq, Num, Ord)

getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Semigroup (Sum a) where
    (<>) = (+)