module WritingListFunctions where

import Test.QuickCheck

myContains :: Eq a => a -> [a] -> Bool
myContains e []     = False
myContains e (x:xs)
  | e == x          = True
  | otherwise       = myContains e xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fn []     = []
myFilter fn (x:xs)
  | fn x           = x : myFilter fn xs
  | otherwise      = myFilter fn xs

myReverseNaive :: [a] -> [a] -- (suprisingly hard!)
myReverseNaive [] = []
myReverseNaive (x:xs) = myReverseNaive xs ++ [x]

myReverse :: [a] -> [a]
myReverse xs = myReverse' xs []
  where
    myReverse' []     ys = ys
    myReverse' (x:xs) ys = myReverse' xs (x:ys)

myReduce :: (a -> b -> b) -> b -> [a] -> b
myReduce f z []     = z 
myReduce f z (x:xs) = f x (foldr f z xs) 

-- I can test whether these are correct using QuickCheck
-- A property based testing framework that asserts invariants
prop_oracle_reverse :: Eq a => [a] -> Bool
prop_oracle_reverse xs = myReverse xs == reverse xs

prop_reverse_preserves_length :: [a] -> Bool
prop_reverse_preserves_length xs = length (myReverse xs) == length xs

-- Run tests with
-- quickCheck prop_oracle_reverse
-- quickCheck prop_reverse_preserves_length
-- See more with verboseCheck instead of quickCheck

-- See what happens when it fails by removing `x` in the myReverse definition
