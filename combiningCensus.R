#These steps will help you combine the outputs 
#from your spatial interpolation with your income data.
# Convert your interpolation into a raster and map it:
r <- raster(r.m)
sufaceMap <- tm_shape(r) + 
  tm_raster(n=10, palette = "viridis",
            title="PM 2.5 \n(in ppm)") +
  tm_shape(pm2.5.tracts) + tm_dots(size=0.02)
sufaceMap

#If you have too many cells, 
#you can reduce the number by aggregating values
agg <- aggregate(r.m, fact=4, fun=mean)
#if choose fact=2, 2*2 cells will be aggregated into one cell

#Extract average pm2.5 for each polygon
income.tracts$pm2.5 <- round(extract(agg, income.tracts, fun = mean)[,1], 5)

