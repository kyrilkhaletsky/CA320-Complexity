myGCD :: Int -> Int -> Int

myGCD n 0 = n
myGCD m n = myGCD n (m `mod` n)

