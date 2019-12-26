import Data.List (minimumBy)
import Data.Ord (comparing)

chunks n [] = []
chunks n is =
  let (image, rest) = splitAt n is
   in image : chunks n rest

showImage [] = []
showImage is = map (map color) $ chunks 25 is
  where
    color '0' = ' '
    color '1' = 'X'

count x ys =
  sum $
  map
    (\y ->
       if x == y
         then 1
         else 0)
    ys

f2 image [] = image
f2 image (image':images) = f2 (zipWith help image image') images
  where
    help '2' x = x
    help y _ = y

main = do
  f <- readFile "./8.txt"
  let f' = filter (/= '\n') f
  let is = chunks (25 * 6) f'
  let min = minimumBy (comparing (count '0')) is
  print min
  print $ count '1' min * count '2' min
  putStrLn $ unlines $ showImage $ f2 (head is) (tail is)
