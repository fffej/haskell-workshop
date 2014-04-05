module RecordSyntax where

data Game = GameC
            {
              homeTeam :: String
            , homeScore :: Int
            , awayTeam :: String
            , awayScore :: Int
            } deriving (Show)

result :: Game -> String 
result game
  | homeScore game > awayScore game = (homeTeam game) ++ " wins"
  | awayScore game > homeScore game = (awayTeam game) ++ " wins"
  | otherwise                       = "Draw"

example :: Game
example = GameC "Everton" 4 "Manchester United" 0
          
homeTeamScores :: Game -> Game
homeTeamScores g = g { homeScore = homeScore g + 1 }
