module Types where

-- Type Constructors
data Point = PointC Int Int

isOrigin :: Point -> Bool
isOrigin (PointC 0 0) = True
isOrigin _            = False

-- Sum Types
data Shape = Circle Double 
           | Rectangle Double Double
           | Square Double

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
arae (Square s) = s * s

-- Recursive polymorphic types
data MyList a = Empty
              | Cons a (MyList a)

myLength :: MyList a -> Int
myLength Empty = 0
myLength (Cons x xs) = 1 + myLength xs

-- Record syntax

data Person = PersonC 
            {
              firstName :: String,
              lastName :: String
            }