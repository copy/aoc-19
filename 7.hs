import Control.Exception.Base (assert)
import Data.List (maximumBy, permutations)
import Data.List.Split
import Data.Ord (comparing)
import Debug.Trace (trace)

interp ip program input output =
  let opcode = program !! ip
   in if opcode `mod` 100 == 99
        then (True, output, ip, program)
        else if opcode `mod` 100 == 3 && null input
               then (False, output, ip, program)
               else let op1 = program !! (ip + 1)
                        op2 = program !! (ip + 2)
                        opoutput =
                          program !!
                          (ip +
                           (if opcode `mod` 10 == 3
                              then 1
                              else 3))
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
                                   " opout " ++
                                   show out ++
                                   " output " ++
                                   show output' ++
                                   " " ++
                                   show (program !! 16) ++ " " ++ show program')
                           else id) $
                        interp ip' program' input' output'

go' [] input program = input
go' (n:ns) input program =
  let (True, [out], _, _) = interp 0 program [n, input] []
   in assert (n <= 4) $ go' ns out program

go program = map (\ns -> (go' ns 0 program, ns)) (permutations [0, 1, 2, 3, 4])

go2 program =
  map
    (\ns ->
       let (ips, programs) =
             unzip $
             map
               (\n ->
                  let (False, [], ip', program') = interp 0 program [n] []
                   in (ip', program'))
               ns
        in (go2' 0 ips programs 0, ns))
    (permutations [5, 6, 7, 8, 9])

go2' input (ip:ips) (program:programs) n =
  let (finished, [out], ip', program') = interp ip program [input] []
   in if finished && n `mod` 5 == 4
        then out
        else go2' out (ips ++ [ip']) (programs ++ [program']) (n + 1)

main = do
  f <- readFile "7.txt"
  let program = map (read :: String -> Int) $ splitOneOf "," f
  print $ maximumBy (comparing fst) $ go program
  print $ maximumBy (comparing fst) $ go2 program
