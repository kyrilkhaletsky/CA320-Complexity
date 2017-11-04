import Data.Char (toUpper,isUpper)

wc :: String -> String
wc = filter (\c -> not (isUpper c))

we :: [Int] -> [Int]
we = filter (\n -> n `mod` 2 /= 0)

wv :: String -> String
wv = filter (\x -> x `notElem` "aeiouAEIOU")
