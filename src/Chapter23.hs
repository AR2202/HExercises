module Chapter23 
(startingCells


  ,startState1
  ,startState2
  ,startState3
  ,animateGrid
  ,startoptions
  ,Coord
  ,neighbours
 

  )
  where
import Control.Comonad.Store
import Data.List

import Graphics.Gloss
import Graphics.Gloss.Data.Color





--Excersise Chapter 23
--Category Theory for programmers (Bartosz Milewski)

-- Types------------------------------------------

data Cell = Dead | Alive deriving (Show, Eq, Read)

type Coord = (Int,Int)

data InitialStates = InitialStates {cellwidth :: Float
                                   ,transformfunc :: (Coord -> (Float,Float))
                                   ,displaycoords :: [Coord]
                                   ,initialiving  :: [Coord]
                                   }
                                   

--preparing Start Values-----------------------------------

startingCells :: [Coord] -> Store Coord Cell
startingCells living = mkStore living (0,0)

checkCell :: [Coord] -> Coord -> Cell
checkCell living coord
  |coord `elem` living = Alive
  |otherwise = Dead


mkStore :: [Coord] -> Coord -> Store Coord Cell
mkStore livingCells ind = store checkFunction ind
  where checkFunction = checkCell livingCells





--logic of updating one cell-------------------------

  
neighbours (x,y) = filter (/= (x,y)) [(x1,y1)| x1<-[x-1..x+1],y1<-[y-1..y+1]]



checkNeighbours :: Store Coord Cell -> [Cell]
checkNeighbours cells = experiment neighbours cells

liveNeighbours :: Store Coord Cell -> Int
liveNeighbours = length. (filter (== Alive)) .checkNeighbours

neighboursToCellState :: Store Coord Cell -> Cell
neighboursToCellState cells
   |liveNs <  2 = Dead
   |liveNs >  3 = Dead
   |liveNs == 3 = Alive
   |liveNs == 2 =  (if (cellState == Alive) then Alive else Dead)
   where  cellState = extract cells
          liveNs    = liveNeighbours cells
          


--taking one step-------------------------------------------------


step :: (Store Coord Cell, [Coord]) -> (Store Coord Cell, [Coord])
step (cells, living) = (newCells, newLiving)
  where newCells  = updateStore living $ extend neighboursToCellState cells
        newLiving = updateLivingCells living newCells

updateLivingCells :: [Coord] -> Store Coord Cell -> [Coord]
updateLivingCells living currentCells = filter (\x->(peek x currentCells) == Alive) $ nub $ living ++ livingNeighbours
  where livingNeighbours = living >>= neighbours 


updateStore currentLiving currentCells = mkStore newLiving ind
  where newLiving = updateLivingCells currentLiving currentCells
        ind       = pos currentCells

--Display--------------------------------------------------------

display1 Alive = '@'
display1 Dead = '.'

displayGrid ::[[Coord]] -> Store Coord Cell -> [String]
displayGrid coords cellstore = (map.map) display1 $ (map.map) flippedPeek coords
 where flippedPeek = flip peek $ cellstore

corners2grid :: Coord -> Coord -> [[Coord]]
corners2grid (x1,y1)(x2,y2)  = groupBy (\x y -> snd x == snd y) $ [(x,y)|y<-[y1..y2],x<-[x1..x2]]

printGrid
  :: Coord
     -> Coord
     -> Store Coord Cell
     -> IO ()
printGrid lefttop rightbottom cellstore = mapM_ putStrLn $ grid
  where grid = displayGrid coordlist cellstore
        coordlist = corners2grid lefttop rightbottom

--Animate---------------------------------------------------------

printGridCLI numsteps lefttop rightbottom startingdata =
  sequence_ $ map disp $ take numsteps $iterate step startingdata
  where disp = printGrid lefttop rightbottom . fst

corners2coords :: Coord -> Coord -> [Coord]
corners2coords (x1,y1)(x2,y2)  =  [(x,y)|y<-[y1..y2],x<-[x1..x2]]


cell2picture Alive = (color aliveColor) . filledSquarePic
cell2picture Dead =  (color deadColor) . emptySquarePic

cells2picture :: Float -> [Coord] -> Store Coord Cell -> (Coord -> (Float,Float)) -> Picture
cells2picture cellwidth coords cellstore f  = pictures $ map cell2Pic coords
  where cell2Pic coord = placePicture cellwidth (cell2picturefun coord) (transformedCoord coord)
        cell2picturefun coord = cell2picture $ flippedPeek coord
        flippedPeek = flip peek $ cellstore
        transformedCoord coord = cellcoord2picturecoord f coord


placePicture cellwidth picturefun (xcoord, ycoord) = translate x y (picturefun cellwidth)
    where x =  xcoord * cellwidth 
          y = ycoord * cellwidth

cellcoord2picturecoord :: (Coord -> (Float,Float)) -> Coord -> (Float,Float)
cellcoord2picturecoord f coord = f coord

makeTransformFunc :: Coord -> Coord -> (Coord -> (Float,Float))
makeTransformFunc lefttop rightbottom = \(x,y) -> (fromIntegral x-leftx-halfn,fromIntegral y-lefty-halfm)
  where leftx = fromIntegral $ fst lefttop
        lefty  = fromIntegral $ snd lefttop
        rightx = fromIntegral $ fst rightbottom
        righty = fromIntegral $ snd rightbottom
        halfn = (rightx - leftx)/2
        halfm = (righty - lefty)/2

corners2n lefttop rightbottom = ((fst rightbottom) - (fst lefttop)) +1

corners2m lefttop rightbottom = ((snd rightbottom) - (snd lefttop)) +1

screenWidth :: Int -> Int -> Int
screenWidth n m
  |n > m     = maxWindowDim
  |otherwise = (maxWindowDim * n) `div` m

screenHeight :: Int -> Int -> Int
screenHeight n m
  |m > n     = maxWindowDim
  |otherwise = (maxWindowDim * m) `div` n



cellWidth :: Int -> Int -> Float
cellWidth n m = fromIntegral screenW / fromIntegral n
  where screenW = screenWidth n m

maxWindowDim :: Int
maxWindowDim = 800



backgroundColor = makeColor 0 0 0 255

deadColor = makeColorI 255 50 50 255

aliveColor = makeColorI 50 100 255 255

filledSquarePic :: Float -> Picture
filledSquarePic size = rectangleSolid size size

emptySquarePic :: Float ->Picture
emptySquarePic size = rectangleWire size size

time2step :: Float -> Int
time2step = floor

step2pic initialstates numstep = cells2picture cellw coord newstore f
  
  where newstore = fst $(iterate step startingdata) !! numstep
        startingdata = (cellstore,startingliving)
        startingliving = initialiving initialstates
        cellstore = mkStore startingliving (0,0)
        coord = displaycoords initialstates
        f = transformfunc initialstates
        cellw = cellwidth initialstates


window n m = InWindow "Game Of Life" (screenWidth n m, screenHeight n m) (100, 100)
--
--animateGrid :: IO ()
animateGrid lefttop rightbottom livingStart = animate win backgroundColor floatToPic
  where floatToPic    = \time -> step2pic initialstates $ time2step time
        win           = window n m
        n             = corners2n lefttop rightbottom
        m             = corners2m lefttop rightbottom
        initialstates = InitialStates {cellwidth     = cellWidth n m
                                      ,transformfunc = makeTransformFunc lefttop rightbottom
                                      ,displaycoords = corners2coords lefttop rightbottom
                                      ,initialiving  = livingStart
                                      }

--Some start values-----------------------------------------------------------------------------------
startState1 :: [Coord]
startState1 = [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5)]

startState2 :: [Coord]
startState2 = [(1,1),(2,1),(3,1),(4,1),(5,1),(1,2),(1,3),(1,4),(1,5),(2,5),(3,5),(4,5),(5,5),(5,4),(5,3),(5,2)]


startState3 :: [Coord]
startState3 = [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(0,5),(1,4),(2,3),(3,4),(5,2)]


startState4 :: [Coord]
startState4 = [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(0,5),(1,4),(2,3),(3,2),(5,1)]


startoptions = [("1",startState1),("2",startState2),("3",startState3)]

