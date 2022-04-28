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

t :: [Integer]
t = [3,4,5] >>= \x -> [x, -x]

h :: [(Integer, Char)]
h = [1,2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

listOfTuples :: [(Integer, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    [(n, ch)]


{-

    MonadPlus is a typeclass for Monads that can also act as monoids

    class Monad m => MonadPlus m where
        mzero :: m a
        mplus :: m a -> m a -> m a

    instance MonadPlus [] where
        mzero = []
        mplus = (++)

    guard :: (MonadPlus m) => Bool -> m ()
    guard True  = return ()
    guard False = mzero

-}

z :: [String]
z = guard (5 > 2) >> return "cool" :: [String]

z' :: [String]
z' = guard (5 < 2) >> return "cool" :: [String]

