{-# LANGUAGE TupleSections #-}

import Data.Char (isAsciiLower, isAsciiUpper, toLower)
import Data.List (delete, foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import qualified Data.Set as Set

surroundings (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

findPath mappy to nextPositions =
  let ((steps, path), nextPositions') = Set.deleteFindMin nextPositions
      pos = head path
      (nextPositions'', mappy') =
        foldl
          (\(p, m) pos' ->
             if Map.findWithDefault '#' pos' mappy /= '#'
               then (Set.insert (steps + 1, pos' : path) p, Map.delete pos' m)
               else (p, m))
          (nextPositions', mappy)
          (surroundings pos)
   in if Set.null nextPositions
        then Nothing
        else if pos == to
               then Just path
               else findPath mappy' to nextPositions''

run positions keys paths cache =
  let freePaths =
        concatMap
          (\position ->
             map (position, ) $
             filter (\(_, _, blockedBy) -> blockedBy `Set.isSubsetOf` keys) $
             fromJust $ Map.lookup position paths)
          positions
      (result, cache') =
        foldl'
          (\(result, cache) (position, (target, len, _)) ->
             let (try, cache') =
                   run
                     (Set.insert target $ Set.delete position positions)
                     (Set.insert target keys)
                     (Map.map
                        (filter (\(t', _, _) -> t' /= target))
                        (Map.delete position paths))
                     cache
              in ((try + len) : result, cache'))
          ([], cache)
          freePaths
      best = minimum result
      cacheKey = (positions, keys)
   in if Map.null (foldr Map.delete paths positions)
        then (0, cache)
        else maybe (best, Map.insert cacheKey best cache') (, cache) $
             Map.lookup cacheKey cache

allPaths starts targets mappy =
  Map.fromList $
  map
    (\(pos1, key1) ->
       ( key1
       , mapMaybe
           (\(pos2, key2) ->
              fmap
                (\path ->
                   ( key2
                   , length path - 1
                   , Set.fromList $
                     map toLower $ filter isAsciiUpper $ map (mappy Map.!) path))
                (findPath mappy pos2 (Set.singleton (0, [pos1]))))
           (delete (pos1, key1) targets)))
    (starts ++ targets)

main = do
  f <- readFile "./18.txt"
  let mappy =
        Map.fromList $
        concatMap (\(y, line) -> map (\(x, f) -> ((x, y), f)) line) $
        zip [0 ..] $ map (zip [0 ..]) $ lines f
  let start = head $ filter ((== '@') . snd) $ Map.toList mappy
  let keys = filter (isAsciiLower . snd) $ Map.toList mappy
  let paths = allPaths [start] keys mappy
  print $ fst $ run (Set.singleton '@') Set.empty paths Map.empty
  let start2 = zip [(39, 39), (39, 41), (41, 39), (41, 41)] "!@$%"
  let mappy2 =
        Map.union
          (Map.fromList $
           zip [(40, 40), (40, 41), (41, 40), (39, 40), (40, 39)] (repeat '#')) $
        Map.union (Map.fromList start2) mappy
  let paths2 = allPaths start2 keys mappy2
  print $ fst $ run (Set.fromList $ map snd start2) Set.empty paths2 Map.empty
