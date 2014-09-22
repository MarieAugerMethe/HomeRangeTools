library(ks)
library(sp) # To create a spatial polygon
library(rgeos) # To get area

###############################
# Calculatin home range size using ks package

# function that creates the home range polygon
HRpolygon <- function(xy){
  # Estimate the bandwith (here using the plug in method)
  H.pi <- Hpi(x=xy)
  # Estimate the kernel density and compute the contour
  # The default value for grid size is 151 for each x and y
  # but if we want the estimate to be more precise we can increase the 
  # size of the grid, it's going to take a longer time to estimate
  fhat <- kde(x=xy, H=H.pi, compute.cont=TRUE, gridsize=rep(250,2))
  
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
  
  # We want to get the information from each element of the list
  pp <- list(length=length(contour.95))
  for(i in 1:length(contour.95)){
    # Create a data.frame with the coordinates of the fist contour line
    poly <- with(contour.95[[i]], data.frame(x,y))
    # close the polygon by repeating the first coordinates
    poly <- rbind(poly, poly[1,])
    pp[[i]] <- Polygon(poly, hole=FALSE) # Set to FALSE, although we want to be able to differentiate between hole and not hole
  }
  
  # Is there a polygon within the other polygon, which would be a hole?
  isContained <- vector(length=length(contour.95))
  # Only do this is you have more than one polygon
  if(length(isContained) > 1){
    for(i in 1:length(contour.95)){
      spPoly1 <- SpatialPolygons(list(Polygons(pp[-i], ID=1)))
      spPoly2 <- SpatialPolygons(list(Polygons(pp[i], ID=1)))
      isContained[i] <- gContains(spPoly1,spPoly2)
    }  
  }
  
  # For the polygons that are contained
  holeID <- (1:length(contour.95))[isContained]
  if(length(holeID)>0){
    spPoly1 <- SpatialPolygons(list(Polygons(pp[-holeID], ID=1)))
    spPoly2 <- SpatialPolygons(list(Polygons(pp[holeID], ID=1)))
    spPoly <- gDifference(spPoly1,spPoly2)
  }else{
    spPoly <- SpatialPolygons(list(Polygons(pp, ID=1)))
  }
  return(spPoly)
}


########################################
# Simulate a multimodal distribution 
# with 4 of the peaks, two of which have a hole in the middle


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


HR <- HRpolygon(xy)
layout(1)
plot(xySP, pch=20, cex=0.5, col="darkgrey")
plot(HR, add=TRUE, col=rgb(0,0,0,0.2))
# Obviously not a perfect model here

# Get Are of home range
gArea(HR)


########################################
# Simulate a unimodal distribution 
x <- rnorm(500, 2, 2)
y <- rnorm(500, 2, 2)
xy <- cbind(x,y)

xySP <- data.frame(xy)
coordinates(xySP) <- ~x+y

HR <- HRpolygon(xy)
layout(1)
plot(xySP, pch=20, cex=0.5, col="darkgrey")
plot(HR, add=TRUE, col=rgb(0,0,0,0.2))
# Obviously not a perfect model here

# Get Are of home range
gArea(HR)

