import Control.Exception.Base (assert)
import Data.Char (isAsciiUpper)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

surroundings (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

block = Set.singleton '#'

isOuterPortal (x, y) = x <= 2 || y <= 2 || x >= 120 || y >= 120

isPortal = isAsciiUpper . Set.findMin

findPath portals mappy to nextPositions =
  let ((steps, path), nextPositions') = Set.deleteFindMin nextPositions
      pos = head path
      (nextPositions'', mappy') =
        foldl
          (\(p, m) pos' ->
             let field = Map.findWithDefault block pos' mappy
                 pos'' =
                   case Map.lookup field portals of
                     Just portal -> head $ filter (/= pos) portal
                     Nothing -> pos'
              in if field /= block
                   then ( Set.insert (steps + 1, pos'' : path) p
                        , Map.delete pos' m)
                   else (p, m))
          (nextPositions', mappy)
          (surroundings pos)
   in if Set.null nextPositions
        then Nothing
        else if pos == to
               then Just path
               else findPath portals mappy' to nextPositions''

findPath2 portals mappy to nextPositions seen =
  let ((steps, path, depth), nextPositions') = Set.deleteFindMin nextPositions
      pos = head path
      (nextPositions'', seen') =
        foldl
          (\(p, seen) pos' ->
             let field = Map.findWithDefault block pos' mappy
                 otherPortal = filter (/= pos) $ portals ! field
                 pos'' =
                   if isPortal field && pos /= to
                     then head $ assert (length otherPortal == 1) otherPortal
                     else pos'
              in if field /= block &&
                    Set.notMember (pos', depth) seen &&
                    not (isPortal field && isOuterPortal pos && depth == 0) &&
                    field /= Set.fromList "ZZ"
                   then ( Set.insert
                            ( steps + 1
                            , pos'' : path
                            , depth +
                              if isPortal field
                                then if isOuterPortal pos
                                       then -1
                                       else 1
                                else 0)
                            p
                        , Set.insert (pos', depth) seen)
                   else (p, seen))
          (nextPositions', seen)
          (surroundings pos)
   in if Set.null nextPositions
        then Nothing
        else if pos == to && depth == 0
               then Just path
               else findPath2 portals mappy to nextPositions'' seen'

main = do
  f <- readFile "./20.txt"
  let mappy =
        Map.fromList $
        concatMap (\(y, line) -> map (\(x, f) -> ((x, y), f)) line) $
        zip [0 ..] $ map (zip [0 ..]) $ lines f
  let mappy' =
        Map.mapWithKey
          (\pos f ->
             if not $ isAsciiUpper f
               then Set.singleton f
               else Set.fromList $
                    filter
                      isAsciiUpper
                      (f :
                       map
                         (\p -> Map.findWithDefault '#' p mappy)
                         (surroundings pos)))
          mappy
  let portals =
        Map.fromListWith (++) $
        map
          (\(pos, name) ->
             ( name
             , map fst $
               filter ((== '.') . snd) $
               map (\pos -> (pos, Map.findWithDefault '#' pos mappy)) $
               surroundings pos)) $
        filter (\(_, f) -> isPortal f) $ Map.toList mappy'
  let [start] = portals ! Set.fromList "AA"
  let [end] = portals ! Set.fromList "ZZ"
  let mappy'' = Map.filter (/= Set.fromList "AA") mappy'
  print $
    subtract 1 $
    length $
    fromJust $ findPath portals mappy'' end (Set.singleton (0, [start]))
  print $
    subtract 1 $
    length $
    fromJust $
    findPath2 portals mappy'' end (Set.singleton (0, [start], 0)) Set.empty
