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

compare :: (Statistics -> Int) -> GameResult -> Result
compare f g = fromOrdering (comparing f (homeStatistics g) (awayStatistics g))

opposite :: Result -> Result
opposite Win = Lose
opposite Draw = Draw
opposite Lose = Win

homeResult :: GameResult -> Result
homeResult = compare goals 

-- Use function composition to write simpler code
awayResult :: GameResult -> Result
awayResult = opposite . homeResult

-- Next up, try to write a league table
resultForTeam :: String -> [GameResult] -> LeagueEntry
resultForTeam team rs = LeagueEntry team (foldr (scoreGame (compare goals)) [] rs)
  where
    scoreGame :: (GameResult -> Result) -> GameResult -> [Result] -> [Result]
    scoreGame f game results 
      | homeTeam game == team = f game : results
      | awayTeam game == team = (opposite $ f game) : results
      | otherwise             = results
        
                                
