{-# LANGUAGE TupleSections #-}

import qualified Data.Set as Set

coords = [(x, y) | x <- [0 .. 4], y <- [0 .. 4]]

step mappy =
  Set.fromList $
  filter
    (\(x, y) ->
       let count =
             sum
               [ field (x, y + 1)
               , field (x + 1, y)
               , field (x - 1, y)
               , field (x, y - 1)
               ]
        in if field (x, y) == 1
             then count == 1
             else count == 1 || count == 2)
    coords
  where
    field (x, y) = fromEnum $ Set.member (x, y) mappy

score mappy =
  sum $
  map (\(x, y) -> 2 ^ (x + 5 * y)) $
  filter (`Set.member` mappy) $ concatMap (\y -> map (, y) [0 .. 4]) [0 .. 4]

run map seen =
  if Set.member map seen
    then score map
    else let map' = step map
          in run map' (Set.insert map seen)

score2 maps = sum $ map length maps

step2 maps = f Set.empty (Set.empty : maps ++ [Set.empty])
  where
    f previous [last] = [single previous last Set.empty]
    f previous (current:next:rest) =
      single previous current next : f current (next : rest)
    single outer mappy inner =
      Set.fromList $
      filter
        (\(x, y) ->
           let count =
                 sum
                   [ field (x, y + 1) (x, y)
                   , field (x + 1, y) (x, y)
                   , field (x - 1, y) (x, y)
                   , field (x, y - 1) (x, y)
                   ]
            in if field (x, y) (0, 0) == 1
                 then count == 1
                 else count == 1 || count == 2)
        (filter (/= (2, 2)) coords)
      where
        (!) m p = fromEnum $ Set.member p m
        field (-1, _) _ = outer ! (1, 2)
        field (5, _) _ = outer ! (3, 2)
        field (_, -1) _ = outer ! (2, 1)
        field (_, 5) _ = outer ! (2, 3)
        field (2, 2) (2, 1) = sum $ map (\i -> inner ! (i, 0)) [0 .. 4]
        field (2, 2) (2, 3) = sum $ map (\i -> inner ! (i, 4)) [0 .. 4]
        field (2, 2) (1, 2) = sum $ map (\i -> inner ! (0, i)) [0 .. 4]
        field (2, 2) (3, 2) = sum $ map (\i -> inner ! (4, i)) [0 .. 4]
        field (x, y) _ = mappy ! (x, y)

main = do
  f <- readFile "./24.txt"
  let mappy =
        Set.fromList $
        map fst $
        filter ((== '#') . snd) $
        concatMap (\(y, line) -> map (\(x, f) -> ((x, y), f)) line) $
        zip [0 ..] $ map (zip [0 ..]) $ lines f
  print $ run mappy Set.empty
  print $ score2 $ (!! 200) $ iterate step2 [mappy]
