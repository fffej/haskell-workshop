module Example where

import FootballStats

import Data.List
import Data.Ord

-- Basic functions with pattern of duplication
bad_goalsScored :: GameResult -> Int
bad_goalsScored g = goals (homeStatistics g) + goals (awayStatistics g)

bad_totalFouls :: GameResult -> Int
bad_totalFouls g = fouls (homeStatistics g) + fouls (awayStatistics g)

-- Introduce a higher order function capturing the behaviour
total :: (Statistics -> Int) -> GameResult -> Int
total f g = f (homeStatistics g) + f (awayStatistics g)

-- combine simple functions to produce readable set of functions
-- Are these functions truly necessary?
totalGoals :: GameResult -> Int
totalGoals = total goals

totalFouls :: GameResult -> Int
totalFouls = total fouls

