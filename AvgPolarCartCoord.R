# Showing that the mean of the displacement in polar coordinates
# will be different than the mean of the displacement in cartesian coordinates
library(shape)
library(CircStats)

set.seed(510)
cartCoord <- cbind(rnorm(10,2,2), rnorm(10,5,2))
par(pty="s")
plot(cartCoord, pch=20, xlim=c(-5,10), ylim=c(-5,10))
abline(h=0,v=0,lty=2)
meanXY <- colMeans(cartCoord)
Arrows(0, 0, meanXY[1], meanXY[2], lcol='blue', lwd=2,
       code=2, arr.length=0.1, arr.width=0.1,arr.type="triangle",
       arr.adj = 1)

#########################################
# Amount of displacment
r <- sqrt(rowSums(cartCoord^2))
meanR <- mean(r)

# Different length values
meanR
sqrt(sum(meanXY^2))

#########################################
# Angle of displacement
theta <- atan2(cartCoord[,2],cartCoord[,1])
meanTheta <- circ.mean(theta)
# Different angle values
meanTheta
atan2(meanXY[2],meanXY[1])

# Covert back to cartesian coordinate
meanX <- meanR * cos(meanTheta)
meanY <- meanR * sin(meanTheta)
Arrows(0, 0, meanX, meanY, lcol='red', lwd=2,
       code=2, arr.length=0.1, arr.width=0.1,arr.type="triangle",
       arr.adj = 1)


