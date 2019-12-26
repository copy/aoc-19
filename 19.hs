import Control.Exception.Base (assert)
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

run program input =
  let (True, [output], _, _, _) = interp 0 program 0 input []
   in output

searchWidth left top program =
  let upperBound width =
        if test width
          then upperBound (2 * width)
          else width
      f minWidth maxWidth =
        assert (maxWidth > minWidth) $
        assert (test minWidth) $
        assert (not $ test maxWidth) $
        if minWidth + 1 == maxWidth
          then minWidth
          else let mid = minWidth + ((maxWidth - minWidth) `div` 2)
                in if test mid
                     then f mid maxWidth
                     else f minWidth mid
      test w = run program [left + w, top] == 1
   in assert (test 0) $ f 1 (upperBound 1)

search program y leftStart ray =
  let x = head $ dropWhile (\x -> run program [x, y] == 0) [leftStart ..]
      width = searchWidth x y program
      topRight = Map.findWithDefault 0 (y - 99) ray
   in if topRight - x >= 99
        then (x, y - 99)
        else search program (y + 1) x (Map.insert y (x + width) ray)

main = do
  f <- readFile "./19.txt"
  let program = Map.fromList (zip [0 ..] (map read $ splitOneOf "," f))
  print $
    length $ filter (== 1) [run program [x, y] | x <- [0 .. 49], y <- [0 .. 49]]
  let (x, y) = search program 25 0 Map.empty
  print (x * 10000 + y)
