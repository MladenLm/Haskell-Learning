import Data.Char
import Geometry

encode :: Int -> String -> String
encode shift msg =
    let ords    = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

decoded :: Int -> String -> String
decoded shift = encode (negate shift)

phoneBook :: [([Char], [Char])]
phoneBook =
    [
        ("betty", "555-2938"),
        ("bonnie", "555-2939"),
        ("penny", "555-2940")
    ]


findKey :: Eq a => a -> [(a, c)] -> c
findKey key = snd . head. filter (\(k,v) -> key == k)

cube' :: Float
cube' = cubeArea 5.0