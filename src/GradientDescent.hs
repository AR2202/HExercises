{-#LANGUAGE FlexibleContexts#-}
module GradientDescent
(
errorfunc,
nIterations,
solution,
coordinateDescent
)
where
--Homework problems 5 of Learning from Data
--Problems can be found here
--https://work.caltech.edu/homeworks.html
errorfunc u v = (u * exp v - 2 * v* exp (-u))^2
partialByU u v = 2*(exp v + 2*v * exp (-u))*(u * exp v -2*v*exp(-u))
partialByV u v = 2 * (u* exp v - 2 * exp (-u)) * (u * exp v - 2 *v*exp(-u))
updateU alpha (u, v) = u - alpha * partialByU u v
updateV alpha (u, v)  = v - alpha * partialByV u v
updateFirst alpha (u,v) = (updateU alpha (u,v),v)
updateSecond alpha (u,v) = (u,updateV alpha (u,v))
updateBoth alpha (u,v) = (updateU alpha (u, v), updateV alpha (u, v) )
updateCoordinate  alpha = updateSecond alpha . updateFirst alpha
iterations alpha = iterate (updateBoth alpha)
nIterations n alpha point = iterations alpha point !! n 
coordinateDescent n alpha point = iterate (updateCoordinate alpha) point !! n

solution = undefined
