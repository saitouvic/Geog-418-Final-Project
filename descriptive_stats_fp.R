##Descriptive stats for income and pollution data
install.packages("lubridate")
install.packages("gtable")
install.packages("gridExtra")
install.packages("grid")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("bcmaps")
install.packages('bcmapsdata', repos='https://bcgov.github.io/drat/')
install.packages("maps")

library(lubridate)
library(gtable)
library(gridExtra)
library(grid)
library(ggplot2)
library(dplyr)
library(bcmaps)
library(bcmapsdata)
library(raster)
library(maps)

#Mean
meanIncome <- mean(income.tracts$Income, na.rm = TRUE) 
meanPollution<- mean(pm2.5$PM25, na.rm = TRUE) 

#Standard Deviation
sdIncome <- sd(income.tracts$Income, na.rm = TRUE) 
sdPollution<- sd(pm2.5$PM25, na.rm = TRUE) 

#Mode
modeIncome <- as.numeric(names(sort(table(income.tracts$Income), decreasing = TRUE))[1]) 
modePollution <- as.numeric(names(sort(table(pm2.5$PM25), decreasing = TRUE))[1]) 

#Median
medIncome <- median(income.tracts$Income, na.rm = TRUE)
medPollution <- median(pm2.5$PM25, na.rm = TRUE)

#Skewness
skewIncome <- skewness(income.tracts$Income, na.rm = TRUE)[1]
skewPollution <- skewness(pm2.5$PM25, na.rm = TRUE)[1]

#Kurtosis
kurtIncome <- kurtosis(income.tracts$Income, na.rm = TRUE)[1]
kurtPollution <- kurtosis(pm2.5$PM25, na.rm = TRUE)[1]

#CoV
CoVIncome <- (sdIncome / meanIncome) * 100
CoVPollution <- (sdPollution / meanPollution) * 100

#Normal distribution test
normIncome_PVAL <- shapiro.test(income.tracts$Income)$p.value
normPollution_PVAL <- shapiro.test(pm2.5$PM25)$p.value

#####
#Create a table of descriptive stats

samples = c("Income", "Pollution") #Create an object for the labels
means = c(meanIncome, meanPollution) #Create an object for the means
sd = c(sdIncome, sdPollution) #Create an object for the standard deviations
median = c(medIncome, medPollution) #Create an object for the medians
mode <- c(modeIncome, modePollution) #Create an object for the modes
skewness <- c(skewIncome, skewPollution) #Create an object for the skewness
kurtosis <- c(kurtIncome, kurtPollution) #Create an object for the kurtosis
CoV <- c(CoVIncome, CoVPollution) #Create an object for the CoV
normality <- c(normIncome_PVAL, normPollution_PVAL) #Create an object for the normality PVALUE
##Check table values for sigfigs?
means <- round(means, 3)
sd <- round(sd, 3)
median <- round(median, 3)
mode <- round(mode, 3)
skewness <- round(skewness, 3)
kurtosis<- round(kurtosis, 3)
CoV <- round(CoV, 3)

data.for.table1 = data.frame(samples, means, sd, median, mode)
data.for.table2 = data.frame(samples, skewness, kurtosis, CoV, normality)

#Make table 1
table1 <- tableGrob(data.for.table1, c("","")) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Table 1: Measure of Dispersion for Income and Pollution data", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)


table2 <- tableGrob(data.for.table2, c("",""))
t2Caption <- textGrob("Table 2: Relative Position for Income and Pollution data", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)



grid.arrange(table1, newpage = TRUE)
grid.arrange(table2, newpage = TRUE)

#Printing a table
png("Measureofdispersion_data.png") #Create an object to print the table to
grid.arrange(table1, newpage = TRUE)
dev.off() #Print table

png("Relative_position_data.png") #Create an object to print the table to
grid.arrange(table2, newpage = TRUE) #Create table
dev.off()


sum(census.tracts$Shape_Area)

##Global Moran's I and local Moran's I 
#######################
head(income.tracts@data)
incomeCleanCols <- c("DAUID","CDNAME","CDTYPE", "CSDUID", "CSDNAME",
                  "CSDTYPE", "CCSUID", "ERUID","ERNAME", "CMAPUID", "CMAUID",
                  "CMANAME", "SACTYPE", "CTUID","CTNAME", "PRUID",
                  "Shape_Leng", "Shape_Area", "Income")

incomeClean <- income.tracts[incomeCleanCols]
income.nb <- poly2nb(income.tracts)
income.net <- nb2lines(income.nb, coords=coordinates(income.tracts))
crs(income.net) <- crs(income.tracts)

tm_shape(income.tracts) + tm_borders(col='lightgrey') + 
  tm_shape(income.net) + tm_lines(col='red')


income.nb2 <- poly2nb(income.tracts, queen = FALSE)
income.net2 <- nb2lines(income.nb2, coords=coordinates(income.tracts))
crs(income.net2) <- crs(income.tracts)

tm_shape(income.tracts) + tm_borders(col='lightgrey') + 
  tm_shape(income.net) + tm_lines(col='blue', lwd = 2) +
  tm_shape(income.net2) + tm_lines(col='yellow', lwd = 2)
########################

income.lw <- nb2listw(income.nb, zero.policy = TRUE, style = "W")
print.listw(income.lw, zero.policy = TRUE)

########################

########################
numeric_version(income.tracts, strict = TRUE)
mi <- moran.test(income.tracts$Income, income.lw, zero.policy = TRUE)
mi

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$Income))
}
moran.range(income.lw)


mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]

z <- (eI - mI)/(var)^(1/2)

########################  
lisa.test <- localmoran(income.tracts$Income, income.lw)

income.tracts$Ii <- lisa.test[,1]
income.tracts$E.Ii<- lisa.test[,2]
income.tracts$Var.Ii<- lisa.test[,3]
income.tracts$Z.Ii<- lisa.test[,4]
income.tracts$P<- lisa.test[,5]
########################
map_LISA <- tm_shape(income.tracts) + 
  tm_polygons(col = "Ii", 
              title = "Local Moran's I", 
              style = "fisher", 
              palette = "Greens", n = 10) 


map_LISA
########################
moran.plot(income.tracts$Income, income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Income", 
           ylab="Spatially Lagged Income", quiet=NULL)
########################

