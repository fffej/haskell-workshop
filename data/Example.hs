module Example where

import FootballStats

import Data.List
import Data.Ord

goalsScored :: GameResult -> Int
goalsScored g = homeGoals g + awayGoals g

totalFouls :: GameResult -> Int
totalFouls g = homeFouls g + awayFouls g