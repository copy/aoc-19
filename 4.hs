import Data.List (group, sort)

filty xs = xs == sort xs

filty2 xs = any ((>= 2) . length) $ group xs

filty3 xs = any ((== 2) . length) $ group xs

main = do
  print $
    length $ filter (\x -> filty x && filty2 x) $ map show [273025 .. 767253]
  print $
    length $ filter (\x -> filty x && filty3 x) $ map show [273025 .. 767253]
