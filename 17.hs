import Data.Char (chr, ord)
import Data.List.Split (splitOneOf)
import qualified Data.Map.Strict as Map
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
                     n -> trace (show n) undefined
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

run program =
  let (True, output, ip', program', base') = interp 0 program 0 [] []
   in output

vadd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

smul (x, y) d = (x * d, y * d)

part1 mappy =
  let sections =
        filter
          (\(pos, v) ->
             v == '#' &&
             all
               (\p -> Map.lookup (pos `vadd` p) mappy == Just '#')
               [(-1, 0), (1, 0), (0, 1), (0, 1)]) $
        Map.toList mappy
   in sum $ map (\((x, y), _) -> x * y) sections

turnRight (dx, dy) =
  case (dx, dy) of
    (-1, 0) -> (0, -1)
    (0, -1) -> (1, 0)
    (1, 0) -> (0, 1)
    (0, 1) -> (-1, 0)

turnLeft = turnRight . turnRight . turnRight

tracy position direction mappy =
  let (x, y) = position
      left = turnLeft direction
      right = turnRight direction
      isPath p = Map.lookup p mappy == Just '#'
      move
        | isPath (position `vadd` left) = Just (left, "L")
        | isPath (position `vadd` right) = Just (right, "R")
        | otherwise = Nothing
   in case move of
        Nothing -> []
        Just (direction', turn) ->
          let steps =
                length $
                takeWhile
                  (\n -> isPath (position `vadd` smul direction' n))
                  [1 ..]
              position' = position `vadd` smul direction' steps
           in (turn, steps) : tracy position' direction' mappy

run2 program input =
  let (True, output, ip', program', base') = interp 0 program 0 input []
   in last output

main = do
  f <- readFile "./17.txt"
  let program = Map.fromList (zip [0 ..] (map read $ splitOneOf "," f))
  let output = map chr $ run program
  let mappy =
        Map.fromList $
        concatMap (\(y, line) -> map (\(x, f) -> ((x, y), f)) line) $
        zip [0 ..] $ map (zip [0 ..]) $ lines output
  putStrLn output
  print $ part1 mappy
  let program' = Map.insert 0 2 program
  let start = fst $ head $ filter ((== '^') . snd) $ Map.toList mappy
  print $ tracy start (0, -1) mappy
  let code =
        run2 program' $
        map ord $
        unlines
          [ "A,C,A,C,B,C,B,A,C,B"
          , "R,4,R,10,R,8,R,4"
          , "R,4,L,12,R,6,L,12"
          , "R,10,R,6,R,4"
          , "n"
          ]
  print code
