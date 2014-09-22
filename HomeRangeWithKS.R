library(ks)
library(sp) # To create a spatial polygon
library(rgeos) # To get area


########################################
# Simulate a multimodal distribution 
# with one of the peak wuth a whole in the middle


# Two modes
# We can think of x as Easting/Lon part of animal locations
x <- c(rnorm(500,5,3), rnorm(300,25,3))
# We can think of y as Norting/Lat part of animal locations
y <- c(rnorm(800,5,3))
xy <- cbind(x,y)

# remove the location in the middle of first mode
xy <- xy[-which((dnorm(x,5,3) + dnorm(y,5,3)) > 0.2),]

# Making 4 peaks with 2 holes
xy <- rbind(xy,xy + matrix(c(0, 20), nrow=nrow(xy), ncol=2, byrow=TRUE))
plot(xy, pch=20, cex=0.5, col="grey")

# Create a spatial object, just to plots nicely
xySP <- data.frame(xy)
coordinates(xySP) <- ~x+y

###############################
# Calculatin home range size using ks package

# Estimate the bandwith (here using the plug in method)
H.pi <- Hpi(x=xy)
# Estimate the kernel density and compute the contour
# The default value for grid size is 151 for each x and y
# but if we want the estimate to be more precise we can increase the 
# size of the grid, it's going to take a longer time to estimate
fhat <- kde(x=xy, H=H.pi, compute.cont=TRUE, gridsize=rep(250,2))

# To plot the kernel
# plot(xy, pch=20, cex=0.5, col="grey")
# plot(fhat, add=TRUE)

# To plot the 95% isopleth of the kernel (cont is =(1-prob)*100, where prob here would be 0.95)
par(mar=c(1,1,1,1))
plot(xySP, pch=20, cex=0.5, col="grey")
contour(x = fhat$eval.points[[1]],
             y = fhat$eval.points[[2]],
             z = fhat$estimate, levels = fhat$cont["5%"], add=TRUE)

# This is not a perfect model, because the edge of the inside circles are
# much sharper for the observation than the estimated kernel
# But for the pupose of the excercise, let's ignore the problem

# We can get the contour line coordinates by using contourLines
# The object returned by kde, fhat, will have the coordinates
# of the points where the kernel was estimated in eval.points.
# fhat$estimate will have the estimated value for the kernel
# and fhat$cont will have the values associated with the contour of interest.
# contourLines will return the eval.points that have values associated with levels. 
# It also returns the level value
contour.95 <- contourLines(x = fhat$eval.points[[1]],
                           y = fhat$eval.points[[2]],
                           z = fhat$estimate, levels = fhat$cont["5%"])
# Note that if the kernel has multiple peaks and hole within a peak
# contour.95 will be a list with multiple elements.
# In this case we have 3 elements. The two peaks and the hole in the first peak.
length(contour.95)

# We want to get the information from each element of the list
pp <- list(length=length(contour.95))
plot(xySP, pch=20, cex=0.5, col="grey")
for(i in 1:length(contour.95)){
  # Create a data.frame with the coordinates of the fist contour line
  poly <- with(contour.95[[i]], data.frame(x,y))
  # close the polygon by repeating the first coordinates
  poly <- rbind(poly, poly[1,])
  pp[[i]] <- Polygon(poly, hole=FALSE) # Set to FALSE, although we want to be able to differentiate between hole and not hole
  plot(SpatialPolygons(list(Polygons(list(pp[[i]]), ID=i))), border=i, add=TRUE)
}

# Is there a polygon within the other polygon, which would be a hole?
isContained <- vector(length=length(contour.95))
# Only do this is you have more than one polygon
if(length(isContained) > 1){
  for(i in 1:length(contour.95)){
    plot(xySP, pch=20, cex=0.5, col="grey")
    spPoly1 <- SpatialPolygons(list(Polygons(pp[-i], ID=1)))
    plot(spPoly1, add=TRUE, border="red")
    spPoly2 <- SpatialPolygons(list(Polygons(pp[i], ID=1)))
    plot(spPoly2, add=TRUE, border="pink")
    isContained[i] <- gContains(spPoly1,spPoly2)
  }  
}

# For the polygons that are contained
holeID <- (1:length(contour.95))[isContained]
if(length(holeID)>0){
  plot(xySP, pch=20, cex=0.5, col="grey")
  spPoly1 <- SpatialPolygons(list(Polygons(pp[-holeID], ID=1)))
  plot(spPoly1, add=TRUE, border="red")
  spPoly2 <- SpatialPolygons(list(Polygons(pp[holeID], ID=1)))
  plot(spPoly2, add=TRUE, border="blue")
  spPoly <- gDifference(spPoly1,spPoly2)
  plot(spPoly, add=TRUE, col="yellow")
}else{
  plot(xySP, pch=20, cex=0.5, col="grey")
  spPoly <- SpatialPolygons(list(Polygons(pp, ID=1)))
  plot(spPoly, add=TRUE, col="yellow")
}

# Get Are of home range
gArea(spPoly)


# To test whether this is correct
# let's do it by hand
# We know that the id of the hole polygon is 3 & 4, see
# holeID
# area1 <- gArea(SpatialPolygons(list(Polygons(pp[1], ID=1))))
# area2 <- gArea(SpatialPolygons(list(Polygons(pp[2], ID=1))))
# area3 <- gArea(SpatialPolygons(list(Polygons(pp[3], ID=1))))
# area4 <- gArea(SpatialPolygons(list(Polygons(pp[4], ID=1))))
# area5 <- gArea(SpatialPolygons(list(Polygons(pp[5], ID=1))))
# area6 <- gArea(SpatialPolygons(list(Polygons(pp[6], ID=1))))
# sum(area1,area2,area5,area6)-sum(area3, area4) # OK
