#Nearest Neighbour Distance
nearestNeighbour <- nndist(kma.ppp)
N <- kma.ppp$n
#Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
#Change the column name to “Distance”
colnames(nearestNeighbour) = "Distance"
#Calculate the nearest neighbor statistic to test for a random spatial distribution.
#mean nearest neighbour
nnd <- sum(nearestNeighbour)/N
#mean nearest neighbour for random spatial distribution
#define the study area with extent
kma.ext2 <- as.matrix(extent(pm2.5.tracts))
window2 <- as.owin(list(xrange = kma.ext2[1,], yrange = kma.ext2[2,]))
kma.ppp2 <- ppp(x = kma$x, y = kma$y, window = window2)
kma.ppp2
plot(kma.ppp2, pch = "+", cex = 0.5)

#Use the gArea function to get the area
gareaSA <- rgeos::gArea(pm2.5.tracts)
pointDensity <- N/gareaSA 
r.nnd = 1/(2*(pointDensity)^(1/2))
d.nnd = 1.07453 /((pointDensity)^(1/2))
R = nnd/r.nnd
SE.NND <- 0.26136/((N*pointDensity)^(1/2))
z = (nnd - r.nnd)/ (SE.NND)
