
{-#LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
module LogisticRegression
(y,
m,
b,
yReader,
Point(..),
runReader,
epoch,
weightReader,
testpoints,
createRandomPoints,
trainRandomPoints,
trainAndTest

)
where
import Control.Monad.Reader
import Data.List
import System.Random

type Weight = (Float,Float,Float)
type Point = (Float,Float)

instance Num Weight where
    (+) (x0,x1,x2) (w0,w1,w2) = (w0+x0,  w1+x1,  w2+x2)
    (-) (x0,x1,x2)(w0,w1,w2)=(x0-w0,x1-w1,x2-w2)
    (*) (x0,x1,x2) (w0,w1,w2) = (w2*x1 - w1*x2, w2*x0 - w0*x2,w1*x0-w0*x1)
    negate (w0,w1,w2)=(negate w0,negate w1, negate w2)
    abs (w0,w1,w2) = (abs w0, abs w1, abs w2)
    fromInteger x = (fromInteger x, 0.0, 0.0)
    signum (w0,w1,w2) = (signum w0, signum w1, signum w2)
--Creating the line for the target function
m :: Point -> Point -> Float
m (x1,x2)(z1,z2) = (x2 - z2)/(x1 - z1)
b :: Point -> Point -> Float
b (x1,x2) (z1,z2) = x2 -x1 *(x2-z2)/(x1-z1)
line point1 point2 = \x -> m point1 point2 * x + b point1 point2
dotProd :: Weight -> Weight -> Float
dotProd (x0,x1,x2) (w0,w1,w2) = w0*x0 + w1*x1 + w2*x2
prod :: Float -> Weight  -> Weight
prod yn (x0,x1,x2)  = (x0*yn,x1*yn,x2*yn)
sub (x0,x1,x2)(w0,w1,w2)=(x0-w0,x1-w1,x2-w2)
point2weight :: Point -> Weight
point2weight (x1,x2) =(1,x1,x2)
--yLine x m1 b1= m1*x + b1
y yLine (x1,x2) = if yLine x1 < x2 then (-1) else 1
crossEntropy1 yLine w x = log (1+ exp(-1*y1 *dotProd xw w))
    where y1 = y yLine x
          xw = point2weight x
          
crossEntropy yLine w points = (*) 0.01 $ sum $ map (crossEntropy1 yLine w) points
dCrossEntropy1 :: (Float->Float) -> Weight -> Point -> Weight
dCrossEntropy1 yLine w x = prod (-1/(1+ exp(y1* dotProd xw w)))(prod y1 xw ) 
    where y1 = y yLine x
          xw = point2weight x
updateWeight yLine  eta w x =  w - prod  eta (dCrossEntropy1 yLine w x) 
epoch yLine eta = foldl' (updateWeight yLine eta) 


yReader :: Point -> Reader (Float->Float) Float
yReader point = do
    fx<-ask
    return(y fx point)

weightReader :: [Point] -> Reader (Float ->Float) Weight
weightReader points = do
    targetFunc <-ask
    let pointpermutations = pointPermutations points
    let weights = finalWeights 0.01 targetFunc 0.01 (0,0,0) pointpermutations
    return weights

errReader :: [Point] -> Weight-> Reader (Float ->Float) Float
errReader points weights = do
    targetFunc <-ask
    let testError = crossEntropy  targetFunc weights points
    return testError


finalWeights stopcrit yLine eta weight0 pointpermutations   
    |vecval (weight1 -weight0)<stopcrit = weight1
    |otherwise = finalWeights stopcrit yLine eta weight1 $ tail  pointpermutations
        where 
            weight1 = epoch yLine eta weight0 $head pointpermutations
            vecval (w0,w1,w2) = sqrt $ w0**2 + w1**2 + w2**2
testpoints :: [Point]
testpoints = [(0.1,1),(0,0),(0.2,-1),(0.3,-0.3),(0.5,-0.5)]

createRandomPoints = do
    randList1 :: [Float] <- forM [1 .. 100] $ \_i -> randomRIO (-1,1)
    randList2 :: [Float] <- forM [1 .. 100] $ \_i -> randomRIO (-1,1)
    return $ zip randList1 randList2

trainRandomPoints = do
    randompoints <- createRandomPoints
    let weights = runReader (weightReader randompoints)  (\x->0.5*x+0.5)
    print weights

trainAndTest = do
    randompoints <- createRandomPoints
    let point1 = randompoints !!0
    let point2 = randompoints !!1

    let target = line point1 point2
    trainpoints <- createRandomPoints
    let weights = runReader (weightReader trainpoints)  target
    print weights
    testpoints <- createRandomPoints
    let testError = runReader (errReader testpoints weights)  target
    print testError
   
pointPermutations = cycle . permutations 
