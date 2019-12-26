import Data.List.Split (splitOneOf)
import Debug.Trace (trace)

interp ip program input output =
  let opcode:_ = drop ip program
   in if opcode `mod` 100 == 99
        then Just (output, program)
        else let op1:_ = drop (ip + 1) program
                 op2:_ = drop (ip + 2) program
                 opoutput:_ = drop (ip + 3) program
                 op1' =
                   if opcode `div` 100 `mod` 10 == 1
                     then op1
                     else program !! op1
                 op2' =
                   if opcode `div` 1000 `mod` 10 == 1
                     then op2
                     else program !! op2
                 out =
                   case opcode `mod` 10 of
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
                     _ -> trace (show opcode) undefined
                 input' =
                   if opcode `mod` 10 == 3
                     then tail input
                     else input
                 ip' =
                   case opcode `mod` 10 of
                     5 ->
                       if op1' /= 0
                         then op2'
                         else ip + 3
                     6 ->
                       if op1' == 0
                         then op2'
                         else ip + 3
                     _ ->
                       ip +
                       if opcode `mod` 10 == 1 ||
                          opcode `mod` 10 == 2 ||
                          opcode `mod` 10 == 7 || opcode `mod` 10 == 8
                         then 4
                         else 2
                 output' =
                   if opcode `mod` 10 == 4
                     then op1' : output
                     else output
                 program' =
                   case out of
                     Nothing -> program
                     Just out ->
                       take opoutput program ++
                       [out] ++ drop (opoutput + 1) program
              in trace
                   ("opcode " ++
                    show opcode ++
                    " op1 " ++
                    show op1 ++
                    " op2 " ++
                    show op2 ++
                    " output " ++
                    show output ++
                    " " ++
                    show (opcode `div` 100 `mod` 10) ++
                    " " ++ show (opcode `div` 1000 `mod` 10)) $
                 interp ip' program' input' output'

main = do
  f <- readFile "./5.txt"
  let line = map (read :: String -> Int) $ splitOneOf "," f
  print $ interp 0 line [1] []
  print $ interp 0 line [5] []
