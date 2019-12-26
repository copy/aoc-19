import Data.List (minimumBy)
import Data.List.Split (splitOneOf)
import qualified Data.Map as Map
import Data.Ord (comparing)

total mappy k = 1 + maybe 0 (sum . map (total mappy)) (Map.lookup k mappy)

tracy mappy current path =
  maybe
    []
    (concatMap
       (\n ->
          if n == "SAN"
            then [path]
            else if n `elem` path
                   then []
                   else tracy mappy n (n : path)))
    (Map.lookup current mappy)

main = do
  f <- readFile "./6.txt"
  let entries = map ((\[to, from] -> (from, [to])) . splitOneOf ")") $ lines f
  let mappy = Map.fromListWith (++) entries
  print $ sum $ map ((+ (-1)) . total mappy . fst) $ Map.toList mappy
  let entries2 =
        concatMap
          ((\[from, to] -> [(from, [to]), (to, [from])]) . splitOneOf ")") $
        lines f
  let mappy2 = Map.fromListWith (++) entries2
  print $
    (+ (-1)) $ length $ minimumBy (comparing length) $ tracy mappy2 "YOU" []
