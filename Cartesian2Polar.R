######################################
# Simulate data set
# Simulate cartesian coordinate
# These could be position in terms of Northing and Easting

# Number of points to simulate
cartCoord <- cbind(c(1,sqrt(0.5),0,-sqrt(0.5),-1,-sqrt(0.5),0,sqrt(0.5)),
                   c(0,sqrt(0.5),1,sqrt(0.5),0,-sqrt(0.5),-1,-sqrt(0.5)))
colnames(cartCoord) <- c("x","y")
par(pty="s", mar=c(5,3,1,1))
plot(cartCoord, pch=20, xlim=c(-2,2), ylim=c(-2,2))
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
# In R atan2(x,y) gives the angle between the y axis and the vector
# from origin to (x,y) in radian
# Y-axis can be interpreted as North
# Thus positive angles represent a turn to the east
# and negative angle represent a turn to the west.
theta <- atan2(cartCoord[,1],cartCoord[,2])

polarCoord <- cbind(r,theta) # In radian
polarCoordText <- paste("(", round(r,1), ", ", round(theta,1), ")", sep="")
text(cartCoord[,1],cartCoord[,2],labels=polarCoordText, cex=0.6, pos=4, col="red")

# If you want the turn from 0 -2*pi, instead of -pi-pi
theta2pi <- theta + ifelse(theta<0,2*pi,0)
polarCoord2piText <- paste("(", round(r,1), ", ", round(theta2pi,1), ")", sep="")
text(cartCoord[,1],cartCoord[,2],labels=polarCoord2piText, cex=0.6, pos=1, col="blue")

# If you want the turn in degree instead of radian
thetaDeg <- theta2pi/(2*pi) * 360
polarCoordDegText <- paste("(", round(r,1), ", ", round(thetaDeg,1), ")", sep="")
text(cartCoord[,1],cartCoord[,2],labels=polarCoordDegText, cex=0.6, pos=3, col="green")


