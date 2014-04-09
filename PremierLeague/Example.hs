module Example where

import FootballStats

import Prelude hiding (compare)
import Data.List
import Data.Ord hiding (compare)

-- Basic functions with pattern of duplication
badGoalsScored :: GameResult -> Int
badGoalsScored g = goals (homeStatistics g) + goals (awayStatistics g)

badTotalFouls :: GameResult -> Int
badTotalFouls g = fouls (homeStatistics g) + fouls (awayStatistics g)

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

fromOrdering :: Ordering -> Result
fromOrdering EQ = Draw
fromOrdering GT = Win
fromOrdering LT = Lose

opposite :: Result -> Result
opposite Win = Lose
opposite Draw = Draw
opposite Lose = Win

compare :: (Statistics -> Int) -> GameResult -> Result
compare f g = fromOrdering (comparing f (homeStatistics g) (awayStatistics g))

homeResult :: GameResult -> Result
homeResult = compare goals

awayResult :: GameResult -> Result
awayResult = opposite . homeResult

data LeagueEntry = LeagueEntry
  {
    team :: String
  , wins :: Int
  , losses :: Int
  , draws :: Int
  } deriving (Show)


recordResult :: LeagueEntry -> Result -> LeagueEntry
recordResult le Win = le { wins = wins le + 1 }
recordResult le Lose = le { losses = losses le + 1 }
recordResult le Draw = le { draws = draws le + 1 }

mkEntry :: String -> LeagueEntry
mkEntry team = LeagueEntry team 0 0 0

leagueTable :: [GameResult] -> [LeagueEntry]
leagueTable games = foldr updateTable (emptyLeague games) games

emptyLeague :: [GameResult] -> [LeagueEntry]
emptyLeague games = map mkEntry $ nub $ map homeTeam games

updateTable :: GameResult -> [LeagueEntry] -> [LeagueEntry]
updateTable game table = update (awayTeam game) ar $
                         update (homeTeam game) hr table
  where
    hr = homeResult game
    ar = awayResult game

update :: String -> Result -> [LeagueEntry] -> [LeagueEntry]
update _ _ [] = []
update teamName result (x:xs)
  | team x == teamName  = recordResult x result : update teamName result xs
  | otherwise           = x : update teamName result xs
