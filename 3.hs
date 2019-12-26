import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (splitOneOf)
import qualified Data.Map as Map

insert_path 0 0 n at map = map
insert_path x y n at map =
  insert_path (x - signum x) (y - signum y) (n - 1) at $
  Map.insert (fst at + x, snd at + y) n map

walk [] map x y n = map
walk ((direction:count):rest) map x y n =
  let count' = read count :: Int
      dx =
        count' *
        case direction of
          'R' -> 1
          'L' -> -1
          _ -> 0
      dy =
        count' *
        case direction of
          'D' -> 1
          'U' -> -1
          _ -> 0
      x' = x + dx
      y' = y + dy
   in walk rest (insert_path dx dy (n + count') (x, y) map) x' y' (n + count')

manhattan (x, y) = abs x + abs y

main = do
  f <- readFile "./3.txt"
  let [line1, line2] = map (splitOneOf ",") $ lines f
  let m1 = walk line1 Map.empty 0 0 0
  let m2 = walk line2 Map.empty 0 0 0
  print m1
  print m2
  print (Map.intersectionWith (,) m1 m2)
  let p =
        minimumBy (compare `on` manhattan) $
        map fst $ Map.toList $ Map.intersection m1 m2
  print p
  print (manhattan p)
  let p2 =
        minimumBy (compare `on` manhattan) $
        map snd $ Map.toList $ Map.intersectionWith (,) m1 m2
  print p2
  print (manhattan p2)
