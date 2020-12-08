library(rgdal)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(tmap)
library(gstat)
library(raster)    # Used to clip out thiessen polygons
library(sp)

#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(pm2.5.tracts, "regular", n=5000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
# Create SpatialPixel object:
gridded(grd)     <- TRUE  
# Create SpatialGrid object:
fullgrid(grd)    <- TRUE  
#Reproject the grid:
proj4string(grd) <- proj4string(income.tracts)


##Spatial Interpolation with IDW
mrg.tab.pm25 <- sp::merge(census.tracts, pm2.5.tracts, by = "DAUID", all.x = FALSE) 
pm25.spdf = mrg.tab.pm25[!is.na(mrg.tab.pm25$PM25),]

head(pm25.spdf)
#IDW Interpolation
P.idw <- gstat::idw(pm25.spdf$PM25 ~ 1, pm25.spdf, newdata=grd, idp=5)
r       <- raster(P.idw)
r.m     <- mask(r, census.tracts)

tm_shape(r.m) + 
  tm_raster(n=10,palette = "BuPu",
            title="PM2.5 in Metro Vancouver") + 
  tm_shape(pm25.spdf) + tm_dots(size=0.01) +
  tm_legend(legend.outside=TRUE)


income.tracts$pm25 <- round(extract(r, income.tracts, fun = mean)[,1], 5)

##make sure if each polygon in income.tracts has a single pm2.5 values
##based on the values from interpolated surface
map_Income <- tm_shape(income.tracts) +
  tm_polygons(col = "pm25",
              title = "pm25",
              style = "fisher",
              palette = "Greens", n = 8) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"))
map_Income
###The map looks like thay have it 
###while a few polygons do have a missing value 