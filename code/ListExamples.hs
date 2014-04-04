module ListExamples where

-- List Examples (1) - Length

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + length xs

-- List Examples (2) - Building new lists
addOne :: [Int] -> [Int]
addOne [] = []
addOne (x:xs) = (x + 1) : (addOne xs)


-- List Examples (3) - Including only some elements
greaterThan5 :: [Int] -> [Int]
greaterThan5 [] = []
greaterThan5 (x:xs)
  | x > 5     = x : greaterThan5 xs
  | otherwise = greaterThan5 xs

-- Higher Order Functions
mymap :: (a -> b) -> [a] -> [b]
mymap f []     = []
mymap f (x:xs) = f x : mymap f xs

-- more succinct definition
-- point free (no args), partial application (+ 1)
addOne' = map (+ 1)

