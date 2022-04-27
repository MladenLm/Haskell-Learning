import Control.Monad

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: (Ord b, Num b) => b -> (b, b) -> Maybe (b, b)
landLeft n (left, right) 
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing 

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) 
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing 

trying :: Maybe (Birds, Birds)
trying = landRight 1 (0, 0) >>= landLeft 2

withReturn :: Maybe (Birds, Birds)
withReturn = return (0, 0) >>= landLeft 3 >>= landRight 2 >>= landLeft 1

foo :: Maybe [Char]
foo = do 
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)