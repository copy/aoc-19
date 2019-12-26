import Control.Exception.Base (assert)
import Data.List (foldl')

modInv a m =
  let (i, _, g) = gcdExt a m
   in assert (g == 1) i
  where
    gcdExt a 0 = (1, 0, a)
    gcdExt a b =
      let (q, r) = a `quotRem` b
          (s, t, g) = gcdExt b r
       in (t, s - q * t, g)

apply total ["deal", "with", "increment", n] card = (card * read n) `mod` total
apply total ["cut", n] card = (card - read n) `mod` total
apply total ["deal", "into", "new", "stack"] card = total - card - 1

rev total ["deal", "with", "increment", n] = (modInv (read n) total, 0)
rev _ ["cut", n] = (1, read n)
rev _ ["deal", "into", "new", "stack"] = (-1, -1)

merge total (mul1, add1) (mul2, add2) =
  ((mul2 * mul1) `mod` total, (mul2 * add1 + add2) `mod` total)

calc total (m, a) x = (m * x + a) `mod` total

fast 1 _ n = n
fast i add n =
  if i `mod` 2 == 0
    then fast (i `div` 2) add (add n n)
    else add n $ fast (i `div` 2) add (add n n)

main = do
  f <- readFile "./22.txt"
  print $ foldl' (flip ($)) 2019 $ map (apply 10007 . words) $ lines f
  let max = 119315717514047
  let count = 101741582076661
  let g = foldl1 (merge max) $ reverse $ map (rev max . words) $ lines f
  print $ calc max (fast count (merge max) g) 2020
