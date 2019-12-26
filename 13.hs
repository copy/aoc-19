import Data.List.Split (splitOneOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
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

go program =
  let (True, output, _, _, _) = interp 0 program 0 [] []
   in handle output Map.empty
  where
    handle (x:y:id:rest) map = handle rest (Map.insert (x, y) id map)
    handle [] map = map

showGame mappy score =
  let left = minimum $ map fst $ Map.keys mappy
      top = minimum $ map snd $ Map.keys mappy
      right = maximum $ map fst $ Map.keys mappy
      bottom = maximum $ map snd $ Map.keys mappy
   in concat
        [ "Score: "
        , show score
        , "\n"
        , unlines $
          map
            (\y ->
               map
                 (\x ->
                    case Map.lookup (x, y) mappy of
                      Just 1 -> 'X'
                      Just 2 -> 'O'
                      Just 3 -> 'X'
                      Just 4 -> 'O'
                      _ -> ' ')
                 [left .. right])
            [top .. bottom]
        ]

go2 ip program base mappy =
  let initialDecision = -1 -- before first frame is rendered
      paddlePosition =
        maybe initialDecision (fst . fst) $
        listToMaybe $ filter ((== 3) . snd) $ Map.toList mappy
      ballPosition =
        maybe 0 (fst . fst) $
        listToMaybe $ filter ((== 4) . snd) $ Map.toList mappy
      direction = signum (ballPosition - paddlePosition)
      (finished, output, ip', program', base') =
        interp ip program base [direction] []
      (mappy', score) = handle output mappy 0
   in trace (showGame mappy' score) $
      if finished
        then score
        else go2 ip' program' base' mappy'
  where
    handle (-1:0:score:rest) map _ = handle rest map score
    handle (x:y:id:rest) map score =
      handle rest (Map.insert (x, y) id map) score
    handle [] mappy score = (mappy, score)

main = do
  f <- readFile "./13.txt"
  let program = Map.fromList (zip [0 ..] (map read $ splitOneOf "," f))
  print $
    sum $
    map
      (\x ->
         if x == 2
           then 1
           else 0) $
    Map.elems $ go program
  let program2 = Map.insert 0 2 program
  print $ go2 0 program2 0 Map.empty
