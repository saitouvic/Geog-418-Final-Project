#intersect the two datasets


#convert the crimes data type to factor
pm2.5.tracts$PM25 <- as.factor(pm2.5.tracts$PM25)
levels(pm2.5.tracts$PM25)

kma <- pm2.5
kma$x <- coordinates(kma)[,1]
kma$y <- coordinates(kma)[,2]

#check for and remove duplicated points
#first, finds zero distance among points to see if there are any duplicates
zd <- zerodist(kma)
zd

#if there are duplicates, remove them
kma <- remove.duplicates(kma)

#create an "extent" object which can be used to create the observation window for spatstat
kma.ext <- as.matrix(extent(kma)) 

#observation window
window <- as.owin(list(xrange = kma.ext[1,], yrange = kma.ext[2,]))

#create ppp oject from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)

#####
###KERNEL DENSITY ESTIMATION
#2D (gaussian) kernel, compare how bandwidth (sigma) selection influences the point density estimates
#since data are projected, sigma is represented in metres
#eps is the width and height of the pixels (1000m X 1000m)
#coerce to a SpatialGridDataFrame for plotting
sigmasizeA <- 5000
sigmasizeB <- 1000
pixel.size <- c(1000,1000)
kde.A <- density(kma.ppp, sigma = sigmasizeA, at = "pixels", eps = pixel.size)
kde.SG <- as(kde.A, "SpatialGridDataFrame")
kde.B <- density(kma.ppp, sigma = sigmasizeB, at = "pixels", eps = pixel.size)
kde.SG <- cbind(kde.SG, as(kde.B, "SpatialGridDataFrame"))  
                
#***CHOOSE SOME OTHER SIGMA VALUES FOR SENSITIVITY ANALYSIS***
names(kde.SG) <- c("Size5000", "Size1000")
#plot
#opens a new plot window
spplot(kde.SG)
#can see how the bandwidth selection influences the density estimates
summary(kde.SG)
#use cross-validation to get the bandwidth that minimizes MSE
bw.d <- bw.diggle(kma.ppp)
#plot the “optimal” bandwidth
plot(bw.d, ylim=c(-10, 10), main="Sigma vs Standard Deviation of sigma")
min(bw.d)
#density using the cross-validation bandwidth
kde.bwo <- density(kma.ppp, sigma = min(bw.d), at = "pixels", eps = c(100, 100))
plot(kde.bwo, add = TRUE)



##kfunction
k.fun <- Kest(kma.ppp, correction = "Ripley")
plot(k.fun)

#use simulation to test the point pattern against CSR
k.fun.e <- envelope(kma.ppp, Kest, nsim = 99, correction = "Ripley")
plot(k.fun.e)
