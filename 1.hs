f x = x `div` 3 - 2

f2 x =
  if f x <= 0
    then 0
    else f x + f2 (f x)

main :: IO ()
main = do
  contents <- readFile "./1.txt"
  let readInt x = read x :: Int
  let lines' = map (f . readInt) $ lines contents
  print $ sum lines'
  let lines2 = map (f2 . readInt) $ lines contents
  print $ sum lines2
  print $ f2 1969
