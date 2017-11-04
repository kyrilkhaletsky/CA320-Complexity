import Data.Char (toUpper,isUpper)

capitalise :: String -> String
capitalise = map toUpper

squareall :: [Int] -> [Int]
squareall = map (^2)

prepend :: [a] -> [[a]] -> [[a]]
prepend xs = map (xs++)
