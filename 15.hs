import Control.Exception.Base (assert)
import Data.List ((\\))
import Data.List.Split (splitOneOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (trace)

(!!?) xs index = Map.findWithDefault 0 index xs

interp ip program base input output =
  let opcode = program !!? ip
   in if opcode `mod` 100 == 99 || opcode `mod` 10 == 3 && null input
        then (opcode `mod` 100 == 99, reverse output, ip, program, base)
        else let opbit n = opcode `div` (10 ^ n) `mod` 10
                 op = opbit 0
                 op1 = program !!? (ip + 1)
                 op2 = program !!? (ip + 2)
                 op3 = program !!? (ip + 3)
                 opoutput =
                   (if op == 3
                      then op1 +
                           (if opbit 2 == 2
                              then base
                              else 0)
                      else op3 +
                           (if opbit 4 == 2
                              then base
                              else 0))
                 op1' =
                   case opbit 2 of
                     0 -> program !!? op1
                     1 -> op1
                     2 -> program !!? (op1 + base)
                 op2' =
                   case opbit 3 of
                     0 -> program !!? op2
                     1 -> op2
                     2 -> program !!? (op2 + base)
                 out =
                   case op of
                     1 -> Just $ op1' + op2'
                     2 -> Just $ op1' * op2'
                     3 -> Just $ head input
                     4 -> Nothing
                     5 -> Nothing
                     6 -> Nothing
                     7 ->
                       Just $
                       if op1' < op2'
                         then 1
                         else 0
                     8 ->
                       Just $
                       if op1' == op2'
                         then 1
                         else 0
                     9 -> Nothing
                     _ -> trace (show opcode) undefined
                 input' =
                   if op == 3
                     then tail input
                     else input
                 ip' =
                   case op of
                     5
                       | op1' /= 0 -> op2'
                     5 -> ip + 3
                     6
                       | op1' == 0 -> op2'
                     6 -> ip + 3
                     _ ->
                       ip +
                       if op == 1 || op == 2 || op == 7 || op == 8
                         then 4
                         else 2
                 output' =
                   if op == 4
                     then op1' : output
                     else output
                 program' =
                   case out of
                     Nothing -> program
                     Just out' -> Map.insert opoutput out' program
                 base' =
                   if op == 9
                     then base + op1'
                     else base
              in (if False
                    then trace
                           ("ip " ++
                            show ip ++
                            " opcode " ++
                            show opcode ++
                            " op1 " ++
                            show op1 ++
                            " op1' " ++
                            show op1' ++
                            " op2 " ++
                            show op2 ++
                            " opoutput " ++
                            show opoutput ++
                            " out " ++
                            show out ++
                            " output " ++
                            show output' ++
                            " base " ++
                            show base ++ " [1000]=" ++ show (program !!? 1000)
                            -- ++ " " ++ show program'
                            )
                    else id) $
                 interp ip' program' base' input' output'

updatePos (x, y) 1 = (x, y - 1)
updatePos (x, y) 2 = (x, y + 1)
updatePos (x, y) 3 = (x - 1, y)
updatePos (x, y) 4 = (x + 1, y)

showSeen seen =
  let left = minimum $ map fst $ Set.elems seen
      top = minimum $ map snd $ Set.elems seen
      right = maximum $ map fst $ Set.elems seen
      bottom = maximum $ map snd $ Set.elems seen
   in unlines $
      map
        (\y ->
           map
             (\x ->
                if Set.member (x, y) seen
                  then 'X'
                  else ' ')
             [left .. right])
        [top .. bottom]

move seen nextPositions =
  let ((steps, pos, direction, ip, program, base), nextPositions') =
        Set.deleteFindMin nextPositions
      (False, [status], ip', program', base') =
        interp ip program base [direction] []
      pos' = updatePos pos direction
   in trace (showSeen seen) $
      if Set.member pos' seen || status == 0
        then move seen nextPositions'
        else if status == 2
               then (pos, steps + 1)
               else move
                      (Set.insert pos' seen)
                      (foldl
                         (\m direction' ->
                            Set.insert
                              ( steps + 1
                              , pos'
                              , direction'
                              , ip'
                              , program'
                              , base')
                              m)
                         nextPositions'
                         [1 .. 4])

makeMap mappy wall nextPositions =
  let ((steps, pos, direction, ip, program, base), nextPositions') =
        Set.deleteFindMin nextPositions
      (False, [status], ip', program', base') =
        interp ip program base [direction] []
      pos' = updatePos pos direction
      (mappy', wall') =
        if status == 0
          then (mappy, Set.insert pos' wall)
          else (Set.insert pos' mappy, wall)
   in if null nextPositions
        then mappy
        else if Set.member pos' mappy || Set.member pos' wall || status == 0
               then makeMap mappy wall nextPositions'
               else makeMap
                      mappy'
                      wall'
                      (foldl
                         (\m direction' ->
                            Set.insert
                              ( steps + 1
                              , pos'
                              , direction'
                              , ip'
                              , program'
                              , base')
                              m)
                         nextPositions'
                         [1 .. 4])

fill remaining positions =
  if null remaining
    then 0
    else let (remaining', positions') =
               foldl
                 (\(r, p) pos ->
                    if Set.member pos r
                      then ( Set.delete pos r
                           , updatePos pos 1 :
                             updatePos pos 2 :
                             updatePos pos 3 : updatePos pos 4 : p)
                      else (r, p))
                 (remaining, [])
                 positions
          in 1 + fill remaining' positions'

main = do
  f <- readFile "./15.txt"
  let program = Map.fromList (zip [0 ..] (map read $ splitOneOf "," f))
  let next =
        Set.fromList $
        map (\direction -> (0, (0, 0), direction, 0, program, 0)) [1 .. 4]
  let (pos, steps) = move (Set.singleton (0, 0)) next
  print steps
  let mappy = makeMap (Set.singleton (0, 0)) Set.empty next
  putStrLn $ showSeen mappy
  print $ fill mappy [pos]
