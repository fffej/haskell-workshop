module SumTypes where

data Result = Win | Lose | Draw deriving (Show)

data Game = GameC
            {
              homeTeam :: String
            , homeScore :: Int
            , awayTeam :: String
            , awayScore :: Int
            }

homeTeamResult :: Game -> Result
homeTeamResult game
  | homeScore game > awayScore game = Win
  | awayScore game > homeScore game = Lose
  | otherwise                       = Draw

flipResult :: Result -> Result
flipResult Win  = Lose
flipResult Lose = Win
flipResult Draw = Draw

-- :t (.)
-- (.) is function composition
awayTeamResult :: Game -> Result
awayTeamResult = flipResult . homeTeamResult
