module Example where

import FootballStats

import Prelude hiding (compare)
import Data.List
import Data.Ord hiding (compare)

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

-- Total is one kind of relationship, but what about defining win lose draw?
data Result = Win
            | Draw
            | Lose deriving (Show,Eq)

data LeagueEntry = LeagueEntry String [Result] deriving (Show)

-- If I carry on in this vein, I'm going to need to write a lot of code
-- for each and every type of variable
bad_homeResult :: GameResult -> Result
bad_homeResult g
  | goals (homeStatistics g) > goals (awayStatistics g) = Win
  | goals (homeStatistics g) < goals (awayStatistics g) = Lose
  | otherwise                                           = Draw
                                       

-- Generalize key functionality, firstly compare two results
fromOrdering :: Ordering -> Result
fromOrdering EQ = Draw
fromOrdering GT = Win
fromOrdering LT = Lose

compare :: (Statistics -> Int) -> Statistics -> Statistics -> Result
compare f s1 s2 = fromOrdering (comparing f s1 s2)

opposite :: Result -> Result
opposite Win = Lose
opposite Draw = Draw
opposite Lose = Win

homeResult :: GameResult -> Result
homeResult g = compare goals (homeStatistics g) (awayStatistics g)

-- Use function composition to write simpler code
awayResult :: GameResult -> Result
awayResult = opposite . homeResult

resultsForTeam :: [GameResult] -> String -> LeagueEntry
resultsForTeam xs team = LeagueEntry team results
  where
    homeMatches = filter (\x -> homeTeam x == team) xs
    awayMatches = filter (\x -> awayTeam x == team) xs
    homeResultsFn g = compare goals (homeStatistics g) (awayStatistics g)
    awayResultsFn = opposite . homeResultsFn 
    results = map homeResultsFn homeMatches ++ map awayResultsFn awayMatches