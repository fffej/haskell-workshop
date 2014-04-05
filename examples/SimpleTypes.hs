module SimpleTypes where

-- Game is the type
-- GameC is the type signature
data Game = GameC String Int String Int
            
-- Function type signatures use the type
result :: Game -> String

-- Function definitions "Deconstruct" values with the type constructor
result (GameC t1 s1 t2 s2)
  | s1 > s2   = t1 ++ " wins"
  | s2 > s1   = t2 ++ " wins"
  | otherwise = "Draw"

example :: Game
example = GameC "Everton" 4 "Manchester United" 0
