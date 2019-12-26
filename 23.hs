import Data.List (foldl')
import Data.List.Split (splitOneOf)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)

(!!?) xs index = Map.findWithDefault 0 index xs

interp ip program base input output =
  let opcode = program !!? ip
   in if opcode `mod` 100 == 99 ||
         opcode `mod` 10 == 3 && null input || length output >= 3
        then (opcode `mod` 100 == 99, reverse output, input, ip, program, base)
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
                 --if null output'
                   --then (False, [], input', ip', program', base')
                 interp ip' program' base' input' output'

readOutput [] = []
readOutput (a:x:y:rest) = (a, x, y) : readOutput rest

run computers =
  let (outputs, computers') =
        foldl
          (\(outputs, programs') (id, (input, ip, program, base)) ->
             let (False, output, input', ip', program', base') =
                   interp
                     ip
                     program
                     base
                     (if null input
                        then [-1]
                        else input)
                     []
              in ( readOutput output : outputs
                 , (id, (input', ip', program', base')) : programs'))
          ([], []) $
        Map.toList computers
      outputs' = concat outputs
      goal = filter (\(a, _, _) -> a == 255) outputs'
   in case goal of
        (255, _, y):_ -> y
        _ ->
          run $
          foldl'
            (\computers (a, x, y) ->
               Map.adjust
                 (\(input, ip, program, base) ->
                    (input ++ [x, y], ip, program, base))
                 a
                 computers)
            (Map.fromList computers')
            outputs'

run2 nat lastSendNat computers =
  let (outputs, computers') =
        foldl
          (\(outputs, programs') (id, (input, ip, program, base)) ->
             let (False, output, input', ip', program', base') =
                   interp
                     ip
                     program
                     base
                     (if null input
                        then [-1]
                        else input)
                     []
              in ( readOutput output : outputs
                 , (id, (input', ip', program', base')) : programs'))
          ([], []) $
        Map.toList computers
      outputs' = concat outputs
      nats =
        map (\(_, x, y) -> (x, y)) $ filter (\(a, _, _) -> a == 255) outputs'
      nat' =
        if null nats
          then nat
          else last nats
   in if null outputs'
        then if lastSendNat == nat'
               then snd lastSendNat
               else run2 nat' nat' $
                    Map.adjust
                      (\([], ip, program, base) ->
                         ([fst nat', snd nat'], ip, program, base))
                      0 $
                    Map.fromList computers'
        else run2 nat' lastSendNat $
             foldl'
               (\computers (a, x, y) ->
                  Map.adjust
                    (\(input, ip, program, base) ->
                       (input ++ [x, y], ip, program, base))
                    a
                    computers)
               (Map.fromList computers')
               outputs'

main = do
  f <- readFile "./23.txt"
  let program = Map.fromList (zip [0 ..] (map read $ splitOneOf "," f))
  let computers = Map.fromList $ map (\i -> (i, ([i], 0, program, 0))) [0 .. 49]
  print $ run computers
  print $ run2 (-1, -2) (-1, -1) computers
