module Main where

import           Chapter23

data CoordCheck
  = Valid
  | NotValid
  deriving (Show, Read, Eq)

main :: IO ()
main = do
  putStrLn "Enter Top Left Coordinate (x,y)"
  coord1 <- fmap readCoord getLine
  putStrLn "Enter Bottom Right Coordinate (x,y)"
  coord2 <- fmap readCoord getLine
  putStrLn "Please chose a start State (1-3)"
  let coordValid = checkCoords coord1 coord2
  case coordValid of
    NotValid ->
      putStrLn
        "left coordinate must have smaller x and y values than right coordinate"
    Valid -> do
      option <- getLine
      let selection = lookup option startoptions
      case selection of
        Nothing -> do
          putStrLn "Not a valid selection"
        Just startState -> animateGrid coord1 coord2 startState

checkCoords :: Coord -> Coord -> CoordCheck
checkCoords (x1, y1) (x2, y2)
  | x1 <= x2 && y1 <= y2 = Valid
  | otherwise = NotValid

readCoord :: String -> Coord
readCoord = read
