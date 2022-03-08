
myXor :: [Bool] -> Bool
myXor xs = not helper
    where helper = even . length $ filter (== True) xs

--sieveSundarm :: Int -> [Int]

class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    toList x = [x]

