######################################
# Simulate data set
# Simulate cartesian coordinate
# These could be position in terms of Northing and Easting

# Number of points to simulate
set.seed(764)
n <- 6
cartCoord <- cbind(rnorm(n,5,6),rnorm(n,0,6))
colnames(cartCoord) <- c("x","y")
plot(cartCoord, pch=20, xlim=c(-10,25), ylim=c(-12,12))
# Create a text version to display on plot
cartCoordText <- paste("(", round(cartCoord[,1],1), ", ", round(cartCoord[,2],1), ")", sep="")
text(cartCoord[,1],cartCoord[,2],labels=cartCoordText, cex=0.6, pos=2)
abline(h=0,v=0, lty=2, col="grey")

# To get the distance from (0,0)
# we use the simple pythagore theorem
# r^2 = x^2 + y^2
r <- sqrt(rowSums(cartCoord^2))

# Get the angle use SOH CAH TOA
# In Quadrant I (+ve x and +ve y)
# tan(theta) = y/x
# theta = arctan(y/x)
# In R atan2(y,x) gives the angle between the x axis and the vector
# from origin to (x,y) in radian
theta <- atan2(cartCoord[,2], cartCoord[,1])

polarCoord <- cbind(r,theta) # In radian
polarCoordText <- paste("(", round(r,1), ", ", round(theta,1), ")", sep="")
text(cartCoord[,1],cartCoord[,2],labels=polarCoordText, cex=0.6, pos=4, col="red")
