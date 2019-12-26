import Data.List.Split (splitOneOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)

parseCount (count:x:rest) = (read count :: Int, x) : parseCount rest
parseCount [] = []

need table [] have = 0
need table ((count, "ORE"):rest) have = count + need table rest have
need table ((needCount, ingredient):rest) have =
  let haveCount = fromMaybe 0 $ Map.lookup ingredient have
      needCount' = max 0 (needCount - haveCount)
      have' = Map.insert ingredient (haveCount - (needCount - needCount')) have
   in if needCount' == 0
        then need table rest have'
        else let (count, ings) = fromJust $ Map.lookup ingredient table
                 toProduce = (needCount' + count - 1) `div` count
              in need
                   table
                   ((needCount', ingredient) :
                    rest ++ map (\(c, i) -> (c * toProduce, i)) ings)
                   (Map.insertWith (+) ingredient (count * toProduce) have')

main = do
  f <- readFile "./14.txt"
  let f' [in_, [(count, out)]] = (out, (count, in_))
  let mapping =
        map
          (f' .
           map
             (parseCount .
              filter (/= "") . splitOneOf " " . filter (`notElem` ",=")) .
           splitOneOf ">") $
        lines f
  let mapping2 = Map.fromList mapping
  print $ need mapping2 [(1, "FUEL")] Map.empty
  print $ need mapping2 [(2, "FUEL")] Map.empty
  print $ need mapping2 [(4215000, "FUEL")] Map.empty
  print $ need mapping2 [(4215655 - 1, "FUEL")] Map.empty
  print $ need mapping2 [(4215655, "FUEL")] Map.empty
  print $
    last $
    takeWhile ((< 1000000000000) . snd) $
    map (\fuel -> (fuel, need mapping2 [(fuel, "FUEL")] Map.empty)) [4215000 ..]
