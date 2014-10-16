HomeRangeTools
==============

Tools to look at animal home ranges and spatial data.

A few home made tools to look at home range size. The main file (HomeRangeWithKW.R) calculates home range area using the ks package. Using the ks package is useful when you want to estimate the bandwidth using methods such as plug-in. While the ks package is useful to estimate the bandwidth it's not straightforward how to estimate the area of the kernel. This is especially complicated with we have a distribution with multiple peaks and holes.
There are 2 additional files that look at converting cartesian (Easting,Northing) to polar coordinates (Step length, Absolute turning angle). Cartesian2Polar.R converts cartesian to polar coordinates. AvgPolarCartCoord.R shows that the average of the cartesian will not give the same answer as the average polar coordinates. 
Finally, the file OneSampleTestForMeanAngle.R has a one-sample test for the mean angle that test whether the mean angle is significantly different from the expected mean. This test is based on Zar (2014) p.670 
