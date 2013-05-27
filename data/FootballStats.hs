module FootballStats where

import HistoricalStats

import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

import Data.List
import Data.Ord

data FootballResult = FootballResult 
  {
    homeTeam :: String
  , awayTeam :: String
  , homeGoals :: Int
  , awayGoals :: Int
  } deriving (Show,Eq)

-- Take the entire content, strip the header and turn into a list of football results
parseFile :: String -> [FootballResult]
parseFile xs = map toResult (tail $ lines xs)

-- take an individual line and turn it into a football result
toResult :: String -> FootballResult
toResult xs = FootballResult 
              {
                homeTeam = get "HomeTeam"
              , awayTeam = get "AwayTeam"
              , homeGoals = getInt "FTHG"
              , awayGoals = getInt "FTAG"
              }
    where
      columns = splitOn "," xs
      get name = columns !! (statIndex name)
      getInt name = read (get name) :: Int 

statIndex :: String -> Int
statIndex name = fromJust (lookup name statisticsIndices)

-- A huge list of available statistics
statisticTypes :: [String]
statisticTypes = 
    [
     "Div", 
     "Date", 
     "HomeTeam", 
     "AwayTeam",
     "FTHG", -- full time home team goals
     "FTAG", -- full time away team goals
     "FTR",  -- full time result (H = home win, D = draw, A = away win)
     "HTHG", -- half time home goals
     "HTAG", -- half time away goals
     "HTR",  -- half time result
     "Referee", 
     "HS", -- home team shots
     "AS", -- away team shots
     "HST", -- home shots on target
     "AST", -- away team shots on target
     "HF", -- home team fouls committed 
     "AF",  -- away team fouls commited
     "HC", -- home team corners
     "AC", -- away team corners
     "HY", -- home yellow cards
     "AY", -- away yellow cards
     "HR", -- home team reds
     "AR"  -- away team reds
    ]

statisticsIndices :: [(String,Int)]
statisticsIndices = zip statisticTypes [0..]