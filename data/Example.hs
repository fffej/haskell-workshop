module Example where

import FootballStats

import Data.List
import Data.Ord

-- Team and points
type LeagueTable = [(String,Int)]

leagueTable :: [GameResult] -> LeagueTable
leagueTable xs = foldl' updateLeagueTable initialTable xs
  where
    initialTable = zip (nub (map homeTeam xs)) [0..]

update :: String -> Int -> LeagueTable -> LeagueTable
update team delta []     = []
update team delta (x:xs)
  | fst x == team = (fst x, snd x + delta) : update team delta xs
  | otherwise = x : update team delta xs

-- Inflexible - home and away team points disgusting
updateLeagueTable :: LeagueTable -> GameResult -> LeagueTable
updateLeagueTable lt fr = update at atp $ update ht htp lt
  where
    ht = homeTeam fr
    at = awayTeam fr
    htp = homeTeamPoints fr
    atp = awayTeamPoints fr

homeTeamPoints :: GameResult -> Int
homeTeamPoints fr
  | halfTimeHomeGoals fr >  halfTimeAwayGoals fr = 3
  | halfTimeHomeGoals fr == halfTimeAwayGoals fr = 1
  | otherwise                                    = 0

awayTeamPoints fr
  | x == 3  = 0
  | x == 1  = 1
  | x == 0  = 3
  where
    x = homeTeamPoints fr
