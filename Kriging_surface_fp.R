#################################################
##Spatial Interpolation with Kriging

# Define the trend model
f.0 <- as.formula(pm2.5.tracts$PM25 ~ 1) 

#Create variogram
var.smpl <- variogram(f.0, pm2.5.tracts, cloud = FALSE) #what is cloud = FALSE
dat.fit.exp  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                              vgm(psill=300, model="Exp", range=15000, nugget=0))
plot(var.smpl, dat.fit.exp)

dat.fit.sph  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                              vgm(psill=300, model="Sph", range=25000, nugget=0))
plot(var.smpl, dat.fit.sph)


# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige(f.0, pm2.5.tracts, grd, dat.fit.sph)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, census.tracts)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="BuPu",  
            title="Predicted PM2.5 value in Metro Vancouver \n(in ug/m^3)") +
  tm_shape(pm2.5.tracts) + tm_dots(size=0.01) +
  tm_legend(legend.outside=TRUE)

r   <- raster(dat.krg, layer="var1.var")
r.m <- mask(r, census.tracts)

tm_shape(r.m) + 
  tm_raster(n=10, palette ="Reds",
            title="Variance map for mean PM2.5 \n(in squared ug/m^3))") +tm_shape(pm2.5.tracts) + tm_dots(size=0.02) +
  tm_legend(legend.outside=TRUE)

r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- mask(r, census.tracts)

tm_shape(r.m) + tm_raster(n=10, palette ="BuPu",title="95% CI map \n(in ug/m^3)" )
+ tm_shape(pm2.5.tracts) + tm_dots(size=0.02) + tm_legend(legend.outside=TRUE
                                                          