import Data.Function (on)
import Data.List (maximumBy, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)

asteroid mappy (x, y) =
  sum $
  map
    (\(dx, dy) ->
       fromEnum $
       any
         (\m -> Map.lookup (x + m * dx, y + m * dy) mappy == Just '#')
         [1 .. size])
    points
  where
    size = 25

asteroids mappy = map (\p -> (p, asteroid mappy p)) $ Map.keys mappy

points =
  sortBy
    (compare `on` sorty)
    [(dx, dy) | dx <- [-size .. size], dy <- [-size .. size], gcd dx dy == 1]
  where
    sorty (x, y) = -atan2 (fromIntegral x) (fromIntegral y)
    size = 25

asteroids2 center mappy =
  mapMaybe (\p -> listToMaybe $ mapMaybe (go p) [1 .. 25])
  where
    go (dx, dy) m =
      let x' = fst center + dx * m
          y' = snd center + dy * m
       in case Map.lookup (x', y') mappy of
            Just '#' -> Just (x', y')
            _ -> Nothing

main = do
  f <- readFile "./10.txt"
  let mappy =
        Map.fromList $
        filter ((/= '.') . snd) $
        concatMap (\(y, line) -> map (\(x, f) -> ((x, y), f)) line) $
        zip [0 ..] $ map (zip [0 ..]) (lines f)
  print $ maximumBy (comparing snd) $ asteroids mappy
  let (p, _) = maximumBy (comparing snd) $ asteroids mappy
  print $ take 200 $ asteroids2 p mappy (cycle points)
  let (x, y) = asteroids2 p mappy (cycle points) !! 199
  print (x * 100 + y)
