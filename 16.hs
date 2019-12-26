digit x = abs x `mod` 10

run xs = map f [1 .. length xs]
  where
    f n = digit $ sum $ map sum $ pattern (drop (n - 1) xs)
      where
        pattern [] = []
        pattern xs =
          take n xs :
          (map negate $ take n (drop (2 * n) xs)) : pattern (drop (4 * n) xs)

run2 xs = f xs (sum xs)
  where
    f [] _ = []
    f (x:xs) sum = digit sum : f xs (sum - x)

main = do
  f <- readFile "./16.txt"
  let f' = map (read . return) $ filter (/= '\n') f
  putStrLn $ concatMap show $ take 8 $ (!! 100) $ iterate run f'
  let offset = 5977341
  putStrLn $
    concatMap show $
    take 8 $ (!! 100) $ iterate run2 (drop offset $ concat $ replicate 10000 f')
