dectobin :: Int -> String

dectobin 0 = "0"
dectobin 1 = "1"
dectobin d 
   | d `mod` 2 == 0 = (dectobin (d `div` 2)) ++ "0"
   | otherwise      = (dectobin (d `div` 2)) ++ "1"
