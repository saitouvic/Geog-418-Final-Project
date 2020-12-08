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
library(e1071)
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
census.tracts <- spTransform(census.tracts, CRS("+init=epsg:26910"))
#Merge income and dissemination data:
income.tracts <- merge(census.tracts,income, by = "DAUID") 
#Determine the number of columns in the dataframe:
nrow(income.tracts)
#Remove NA values:
income.tracts <- income.tracts[!is.na(income.tracts$Income),]
#Reproject the data:
income.tracts <- spTransform(income.tracts, CRS("+init=epsg:26910"))

pm2.5.tracts <- merge(census.tracts,pm2.5, by = "DAUID")
pm2.5.tracts <- pm2.5.tracts[!is.na(pm2.5.tracts$PM25),]

#tmaptools::palette_explorer()
tmap_mode("view")

#Create choropleth map of income:
map_Income <- tm_shape(income.tracts) +
  tm_polygons(col = "Income",
              title = "Median Income",
              style = "jenks",
              palette = "Greens", n = 8) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"))

map_Income

#map income tracts
#tm_shape(income.tracts) + 
# tm_polygons() +
#  tm_shape(pm2.5) +
#  tm_dots(col="PM25", palette = "Greens", n = 5,  
#         title="Sampled Ozone \n(in ppm)", size=0.2) + 
#  tm_legend(legend.outside=TRUE)

#map pm2.5 tracts
#map_pm2.5 <- tm_shape(pm2.5.tracts) +
#  tm_polygons(col = "PM25",
#             title = "PM2.5 in Vancouver",
#              style = "fisher",
#              palette = "Greens", n = 8) +
#  tm_legend(legend.position = c("LEFT", "BOTTOM"))
#map_pm2.5

##Descriptive statistics
##Neighbourhood 
##Income Data
##Objective 1: Spatial Segregation of Income (Global Moran's I) from lab3
##Point pattern analysis for PM2.5 data from lab2
##Spatial Interpolation from lab4
##IDW?? justify the choice


