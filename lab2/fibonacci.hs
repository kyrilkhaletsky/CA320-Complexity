fibonacci2 :: Int -> Int

fibonacci2 1 = 0
fibonacci2 2 = 1
fibonacci2 n = fibonacci2 (n-1) + fibonacci2 (n-2)

