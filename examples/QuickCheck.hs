module QuickCheck where

import Test.QuickCheck
import Data.List (sort)

myReverse :: [a] -> [a]
myReverse zs = myReverse' zs []
    where
      myReverse' [] ys = ys
      myReverse' (x:xs) ys = myReverse' xs (x : ys)

prop_LengthSame :: [Int] -> Bool
prop_LengthSame xs = length (myReverse xs) == length xs

badSort :: Ord a => [a] -> [a]
badSort [] = []
badSort xs = minimum xs : tail xs

prop_BadSortWorks :: [Int] -> Bool
prop_BadSortWorks xs = sort xs == badSort xs

main :: IO ()
main = do
  putStrLn "Running Property Tests"
  verboseCheck prop_LengthSame
  verboseCheck prop_BadSortWorks