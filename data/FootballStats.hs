module FootballStats where

import HistoricalStats

import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

import Data.Ord
import Data.List

data GameResult = GameResult 
  {
    homeTeam :: String
  , awayTeam :: String
  , homeGoals :: Int
  , awayGoals :: Int
  , halfTimeHomeGoals :: Int
  , halfTimeAwayGoals :: Int
  , referee :: String
  , homeShots :: Int
  , awayShots :: Int
  , homeShotsOnTarget :: Int
  , awayShotsOnTarget :: Int
  , homeFouls :: Int
  , awayFouls :: Int
  , homeCorners :: Int
  , awayCorners :: Int
  , homeYellows :: Int
  , awayYellows :: Int
  , homeReds :: Int
  , awayReds :: Int
  } deriving (Show,Eq)

results2011 :: [GameResult]
results2011 = parse season2011

results2012 :: [GameResult]
results2012 = parse season2012

-- Take the entire content, strip the header and turn into a list of football results
parse :: String -> [GameResult]
parse xs = map toResult (tail $ lines xs)

-- take an individual line and turn it into a football result
toResult :: String -> GameResult
toResult xs = GameResult 
              {
                homeTeam = get "HomeTeam"
              , awayTeam = get "AwayTeam"
              , homeGoals = getInt "FTHG"
              , awayGoals = getInt "FTAG"
              , halfTimeHomeGoals = getInt "HTHG"
              , halfTimeAwayGoals = getInt "HTAG"
              , referee = get "Referee"
              , homeShots = getInt "HS"
              , awayShots = getInt "AS"
              , homeShotsOnTarget = getInt "HST"
              , awayShotsOnTarget = getInt "AST"
              , homeFouls = getInt "HF"
              , awayFouls = getInt "AF"
              , homeCorners = getInt "HC"
              , awayCorners = getInt "AC"
              , homeYellows = getInt "HY"
              , awayYellows = getInt "AY"
              , homeReds = getInt "HR"
              , awayReds = getInt "AR"
              }
    where
      columns = splitOn "," xs
      get name = columns !! statIndex name
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
