#Libraries
install.packages("spgwr")
install.packages("spatstat")
install.packages("tmap")
install.packages("gstat")
install.packages("sf")
install.packages("raster")
install.packages("rgdal")
install.packages("e1071")
install.packages("spdep")


library(spgwr)
library(spatstat)
library(tmap)
library(gstat)
library(sf)
library(raster)
library(rgdal)
library(c)
library(spdep)

#Set working directory
dir <- "/Users/goshu/Desktop/final_assignment"
setwd(dir)

#Reading in particulate matter dataset
#Read in PM2.5 data:
pm2.5 <- readOGR("./gsfile/Census/Pm25Sample.shp") 
pm2.5 <- spTransform(pm2.5, CRS("+init=epsg:26910"))

#Reading in dissemination tract and income data
#Read in census income data:
income <- read.csv("./gsfile/Census/Income.csv")  
#Select only ID and Income columns:
colnames(income) <- c("DAUID", "Income") 
#Read in dissemination tract shapefile:
census.tracts <- readOGR("./gsfile/Census/BC_DA.shp") 
#Merge income and dissemination data:
income.tracts <- merge(census.tracts,income, by = "DAUID") 
#Determine the number of columns in the dataframe:
nrow(income.tracts)
#Remove NA values:
income.tracts <- income.tracts[!is.na(income.tracts$Income),]
#Reproject the data:
income.tracts <- spTransform(income.tracts, CRS("+init=epsg:26910"))


tmaptools::palette_explorer()
tmap_mode("view")

#Create choropleth map of income:
map_Income <- tm_shape(income.tracts) +
  tm_polygons(col = "Income",
              title = "Median Income",
              style = "jenks",
              palette = "Greens", n = 8) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"))

map_Income


#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(pm2.5, "regular", n=5000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
# Create SpatialPixel object:
gridded(grd)     <- TRUE  
# Create SpatialGrid object:
fullgrid(grd)    <- TRUE  
#Reproject the grid:
proj4string(grd) <- proj4string(income.tracts)
