module Add where

{-
  Several ways to define the add function

  add2 demonstrates the use of guard clauses
  read | x == 0 = as "when x is equal to zero"

  add3 demonstrates how you can use multiple definitions of the 
  same function.  Functions are matched top to bottom.  Run ghci -Wall 
  to get warnings when you have an incomplete pattern match
-}


add x y = x + y

add2 x y = z
  where
    z = x + y

add3 x y 
  | isZero x = y
  | isZero y = x
  | otherwise = 1 + add3 (x - 1) y
  where
    isZero z = z == 0

add4 0 y = y
add4 x 0 = x
add4 x y = 1 + add3 (x - 1) y

{- Note to the observant, these fall over hideously when numbers are negative! -}
