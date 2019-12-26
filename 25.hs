{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

import Data.Char (chr, ord)
import Data.List (foldl')
import Data.List.Split (splitOneOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (trace)

(!!?) xs index = Map.findWithDefault 0 index xs

member x (y:ys) = x == y || member x ys
member _ [] = False

interp ip program base input output =
  let opcode = program !!? ip
   in if opcode `mod` 100 == 99 || opcode `mod` 10 == 3 && null input
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
                 interp ip' program' base' input' output'

directions = ["north", "east", "south", "west"]

run next screensSeen seen =
  let ((steps, action, items, ip, program, base), next') =
        Set.deleteFindMin next
      (finished, output, _, ip', program', base') =
        interp ip program base (map ord (action ++ "\n")) []
      availableItems = map (drop 2) $ filter isItem $ lines $ map chr output
      isItem "- infinite loop" = False
      isItem ('-':' ':rest) = not $ rest `member` directions
      isItem _ = False
      isInvalid =
        any
          (\line ->
             line `member`
             [ "You don't have that item."
             , "You can't go that way."
             , "You don't see that item here."
             ])
          (lines $ map chr output)
      location =
        unwords $
        filter
          (\case
             '=':('=':_) -> True
             _ -> False)
          (lines $ map chr output)
      next'' =
        foldl'
          (\next action ->
             let parts = words action
              in Set.insert
                   ( steps + 1
                   , action
                   , case parts of
                       [_] -> items
                       "take":item -> Set.insert (unwords item) items
                   , ip'
                   , program'
                   , base')
                   next)
          next'
          (map ("take " ++) availableItems ++ directions)
   in if null next
        then 42
        else (if output `Set.member` screensSeen || isInvalid
                then id
                else trace (map chr output))
               run
               (if isInvalid || finished || Set.member (location, items) seen
                  then next'
                  else next'')
               (Set.insert output screensSeen)
               (Set.insert (location, items) seen)

main = do
  f <- readFile "./25.txt"
  let program =
        Map.fromList
          (zip [0 ..] (map (read :: String -> Int) $ splitOneOf "," f))
  print $
    run
      (Set.fromList $ map (0, , Set.empty, 0, program, 0) directions)
      Set.empty
      Set.empty
