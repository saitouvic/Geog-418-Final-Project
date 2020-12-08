######Linear Regression##########
#Let's say your dataset with both PM2.5 and Income 
#are stored in a dataset called income.tracts.
#Plot income and PM2.5 from the income.tracts dataset you created
plot(income.tracts$pm25~income.tracts$Income)

#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
income.tracts.no0 <-  income.tracts[which(income.tracts$pm25 > 0), ]

#Now plot the data again
plot(income.tracts.no0$pm25~income.tracts.no0$Income)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(income.tracts.no0$pm25~income.tracts.no0$Income)
#Add the regression model to the plot you created
plot(income.tracts.no0$pm25~income.tracts.no0$Income)
abline(lm.model, col = "red")
#Get the summary of the results
summary(lm.model)

#add the fitted values to your spatialpolygon dataframe
income.tracts.no0$predictlm <- lm.model$fitted.values

#You want to determine if the model residuals are spatially clustered. 
#add the residuals to your spatialpolygon dataframe
income.tracts.no0$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(income.tracts.no0)

#Now, create choropleth map of residuals
map_resid <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "residuals",
              title = "residuals",
              style = "jenks",
              palette = "viridis", n = 10)

map_resid


##residuals are spatially independent 
#global moran's I 
#if violated, use geographically weighted regression

##Global Moran's I and local Moran's I 
#######################
residual.nb <- poly2nb(income.tracts.no0)
residual.net <- nb2lines(residual.nb, coords=coordinates(income.tracts.no0))
crs(residual.net) <- crs(income.tracts.no0)

tm_shape(income.tracts.no0) + tm_borders(col='lightgrey') + 
  tm_shape(residual.net) + tm_lines(col='red')

########################

residual.lw <- nb2listw(residual.nb, zero.policy = TRUE, style = "W")
print.listw(residual.lw, zero.policy = TRUE)

########################

########################
numeric_version(income.tracts.no0, strict = TRUE)
mi <- moran.test(income.tracts.no0$residuals, residual.lw, zero.policy = TRUE)
mi

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$residuals))
}
moran.range(residual.lw)


mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]

z <- (eI - mI)/(var)^(1/2)

########################  