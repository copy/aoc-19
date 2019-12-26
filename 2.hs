import Control.Exception.Base (assert)
import Data.List.Split (splitOneOf)

interp offset program =
  let opcode:op1:op2:output:_ = drop offset program
   in if opcode == 99
        then Just program
        else let op1' = program !! op1
                 op2' = program !! op2
                 out =
                   if opcode == 1
                     then op1' + op2'
                     else op1' * op2'
                 program' =
                   take output program ++ [out] ++ drop (output + 1) program
              in if (opcode == 1 || opcode == 2) &&
                    op1 < length program && op2 < length program
                   then interp (offset + 4) program'
                   else Nothing

configure noun verb program = take 1 program ++ [noun, verb] ++ drop 3 program

interp2 noun verb program =
  case interp 0 (configure noun verb program) of
    Just (19690720:_) -> 100 * noun + verb
    _ ->
      if noun == 99
        then assert (verb <= 99) $ interp2 0 (verb + 1) program
        else interp2 (noun + 1) verb program

main = do
  contents <- readFile "./2.txt"
  let lines' = map (read :: String -> Int) $ splitOneOf "," contents
  print lines'
  print $ interp 0 lines'
  print $ interp2 0 0 lines'
