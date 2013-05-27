module Add where

add x y = x + y

add2 x y 
  | x == 0 = y
  | y == 0 = x
  | otherwise = 1 + add2 (x - 1) y

add3 0 y = y
add3 x 0 = x
add3 x y = 1 + add3 (x - 1) y
