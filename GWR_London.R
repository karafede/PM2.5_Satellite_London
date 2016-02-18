library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(raster)   ## To convert an "Extent" object to a "SpatialPolygons" object.
library(rgeos)
library(rgdal)
library(plyr)
library(tcltk2)
library(dplyr)
setwd <- "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe"

PM25_London_2009_2011 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_10km_London_2009-2011.csv",
                                  header = TRUE) 

PM25_London_2009_2011$x <- PM25_London_2009_2011$Lon # define x & y as longitude and latitude
PM25_London_2009_2011$y <- PM25_London_2009_2011$Lat

coordinates(PM25_London_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(PM25_London_2009_2011)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid 1km resolution

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(PM25_London_2009_2011, pch = 1, col = "red", cex = 1)

idw <- idw(formula = PM25 ~ 1, locations = PM25_London_2009_2011, 
           newdata = grd)  # apply idw model for the data (interpolation)

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_1km")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_1km_2009-11_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_1km))

############   UK AIR ################################################################

PM25_London_UK_AIR_2010_2012 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_UK_AIR_London.csv",
                              header = TRUE) 

PM25_London_UK_AIR_2010_2012$x <- PM25_London_UK_AIR_2010_2012$longitude
PM25_London_UK_AIR_2010_2012$y <- PM25_London_UK_AIR_2010_2012$latitude

coordinates(PM25_London_UK_AIR_2010_2012) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(PM25_London_UK_AIR_2010_2012)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(PM25_London_UK_AIR_2010_2012, pch = 1, col = "red", cex = 1)

idw <- idw(formula = PM25_AVG ~ 1, locations = PM25_London_UK_AIR_2010_2012, 
           newdata = grd)  # apply idw model for the data (interpolation)

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_UK_AIR_AVG_1km")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_AIR_AVG_2010_2012_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_UK_AIR_AVG_1km))



############ PCM data #############################################################

PM25_pcm_London_2009_2011 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/pcm_data/PCM_2009-11_AVG_London.csv",
                             header = TRUE) 

PM25_pcm_London_2009_2011$x <- PM25_pcm_London_2009_2011$Lon
PM25_pcm_London_2009_2011$y <- PM25_pcm_London_2009_2011$Lat

coordinates(PM25_pcm_London_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(PM25_pcm_London_2009_2011)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(PM25_pcm_London_2009_2011, pch = 1, col = "red", cex = 1)

idw <- idw(formula = pcm_2009_11 ~ 1, locations = PM25_pcm_London_2009_2011, 
           newdata = grd)  # apply idw model for the data (interpolation)

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_pcm_1km")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/pcm_data/PM25_pcm_2009_11_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_pcm_1km))



############   UK ALTITUDE from UK AIR #############################################################

Altitude_London <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Altitude_London.csv",
                                         header = TRUE) 

Altitude_London$x <- Altitude_London$Lon
Altitude_London$y <- Altitude_London$Lat

coordinates(Altitude_London) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(Altitude_London)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(Altitude_London, pch = 1, col = "red", cex = 1)

idw <- idw(formula = Altitude ~ 1, locations = Altitude_London, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "Altitude")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Altitude_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = Altitude))


############  Land Cover UK ######################################################
#### MODIS data 2007 ############################################################

Land_Cover_London <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Land_Cover_London.csv",
                          header = TRUE)  

x <- as.data.frame(Land_Cover_London$X) 
y <- as.data.frame(Land_Cover_London$Y)

##### only keep urban fraction  Lancover == 13 #########

Land_Cover_London[Land_Cover_London < 13]<- 0
Land_Cover_London[Land_Cover_London > 13]<- 0


Land_Cover_London <- cbind(x,y, (Land_Cover_London$LANDCOVERU/13)*100)  ### urban fraction(%)
colnames(Land_Cover_London) <- c("x", "y", "Land_URB")


coordinates(Land_Cover_London) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(Land_Cover_London)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(Land_Cover_London, pch = 1, col = "red", cex = 1)

idw <- idw(formula = Land_URB ~ 1, locations = Land_Cover_London, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "Land_URB")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Land_Cover_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = Land_URB))



#####################################################################################
################# NO3 from CMAQ model ###############################################

NO3_2009_2011 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/NO3_2009_2011_London.csv",
                          header = TRUE) 

NO3_2009_2011$x <- NO3_2009_2011$Lon # define x & y as longitude and latitude
NO3_2009_2011$y <- NO3_2009_2011$Lat

coordinates(NO3_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(NO3_2009_2011)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(NO3_2009_2011, pch = 1, col = "red", cex = 1)

idw <- idw(formula = NO3_AVG ~ 1, locations = NO3_2009_2011, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "NO3_AVG")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/NO3_2009_2011_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = NO3_AVG))



################# SO4 from CMAQ model ###############################################

SO4_2009_2011 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/SO4_2009_2011_London.csv",
                          header = TRUE) 

SO4_2009_2011$x <- SO4_2009_2011$Lon # define x & y as longitude and latitude
SO4_2009_2011$y <- SO4_2009_2011$Lat

coordinates(SO4_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(SO4_2009_2011)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(SO4_2009_2011, pch = 1, col = "red", cex = 1)

idw <- idw(formula = SO4_AVG ~ 1, locations = SO4_2009_2011, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "SO4_AVG")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/SO4_2009_2011_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = SO4_AVG))




################# POC from CMAQ model ###############################################

POC_2009_2011 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/POC_2009_2011_London.csv",
                          header = TRUE) 

POC_2009_2011$x <- POC_2009_2011$Lon # define x & y as longitude and latitude
POC_2009_2011$y <- POC_2009_2011$Lat

coordinates(POC_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(POC_2009_2011)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(POC_2009_2011, pch = 1, col = "red", cex = 1)

idw <- idw(formula = POC_AVG ~ 1, locations = POC_2009_2011, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "POC_AVG")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/POC_2009_2011_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = POC_AVG))



################# SOAA from CMAQ model ###############################################

SOAA_2009_2011 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/SOAA_2009_2011_London.csv",
                          header = TRUE) 

SOAA_2009_2011$x <- SOAA_2009_2011$Lon # define x & y as longitude and latitude
SOAA_2009_2011$y <- SOAA_2009_2011$Lat

coordinates(SOAA_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(SOAA_2009_2011)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(SOAA_2009_2011, pch = 1, col = "red", cex = 1)

idw <- idw(formula = SOAA_AVG ~ 1, locations = SOAA_2009_2011, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "SOAA_AVG")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/SOAA_2009_2011_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = SOAA_AVG))


################# PM25 from CMAQ model ###############################################

PM25_2009_2011 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/PM25_2009_2011_London.csv",
                           header = TRUE) 

PM25_2009_2011$x <- PM25_2009_2011$Lon # define x & y as longitude and latitude
PM25_2009_2011$y <- PM25_2009_2011$Lat

coordinates(PM25_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(PM25_2009_2011)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(PM25_2009_2011, pch = 1, col = "red", cex = 1)

idw <- idw(formula = PM25_AVG ~ 1, locations = PM25_2009_2011, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_AVG")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/PM25_2009_2011_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_AVG))

############  Topography UK ######################################################

Topography_London <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Topography_London.csv",
                             header = TRUE) 

Topography_London$x <- Topography_London$X
Topography_London$y <- Topography_London$Y

coordinates(Topography_London) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(Topography_London)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(Topography_London, pch = 1, col = "red", cex = 1)

idw <- idw(formula = ETOPO1GRAY ~ 1, locations = Topography_London, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "TOPO_London")  # give names to the modelled variables

write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Topography_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = TOPO_London))


############## Geographycally Weighted Regression ###########################
#############################################################################

library(spgwr)
library(AICcmodavg)
library("usdm")
library("nortest")
library(usdm)
library(nortest)

# mydata <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_1km_London_2010-12.csv",header = TRUE)

OE_PM25 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_1km_2009-11_interp.csv",header = TRUE)
In_situ_PM25 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_AIR_AVG_2010_2012_interp.csv",header = TRUE)
PM25_pcm <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/pcm_data/PM25_pcm_2009_11_London_interp.csv", header = TRUE)
Land_Cover <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Land_Cover_London_interp.csv",header = TRUE)
Altitude_London <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Altitude_London_interp.csv",header = TRUE)
TOPOGRAPHY <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Topography_London_interp.csv",header = TRUE)

NO3 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/NO3_2009_2011_London_interp.csv",header = TRUE)
SO4 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/SO4_2009_2011_London_interp.csv",header = TRUE)
POC <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/POC_2009_2011_London_interp.csv",header = TRUE)
SOAA <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/SOAA_2009_2011_London_interp.csv",header = TRUE)
PM25 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/PM25_2009_2011_London_interp.csv",header = TRUE)


# Linear Regression Satellite PM2.5 vs PM25 from pcm model

 plot(PM25_pcm$PM25_pcm_1km ~ OE_PM25$PM25_1km )
 fit <- lm(PM25_pcm$PM25_pcm_1km ~ OE_PM25$PM25_1km )
 abline(fit)
 summary(fit) # show results


 # BIAS <- In_situ_PM25$PM25_UK_AIR_AVG_1km - OE_PM25$PM25_1km
 BIAS <- PM25_pcm$PM25_pcm_1km - OE_PM25$PM25_1km  ### use pcm data as surrogate for UK-AIR ground measurements data
 
 PM25_UK_AIR <- In_situ_PM25$PM25_UK_AIR_AVG_1km
 PM25_UK_pcm <- PM25_pcm$PM25_pcm_1km
 OE <- OE_PM25$PM25_1km  ### satellite optical estimation PM2.5
 ED <- TOPOGRAPHY$TOPO_London - Altitude_London$Altitude
 URB <- Land_Cover$Land_URB
 NIT <- NO3$NO3_AVG
 SULF <- SO4$SO4_AVG
 PC <- (POC$POC_AVG)
 SOA <- SOAA$SOAA_AVG
 PM25_CMAQ <- PM25$PM25_AVG
 
 
# NITRATE <- (NIT/PM25_CMAQ)*PM25_UK_pcm - (NIT/PM25_CMAQ)*OE
# SULFATE <- (SULF/PM25_CMAQ)*PM25_UK_pcm - (SULF/PM25_CMAQ)*OE
# PRIM_CARBON <- (PC/PM25_CMAQ)*PM25_UK_pcm - (PC/PM25_CMAQ)*OE
# SEC_ORG_AERO <- (SOA/PM25_CMAQ)*PM25_UK_pcm - (SOA/PM25_CMAQ)*OE
  
# NIT <- NIT/PM25_UK_pcm
# SO4 <- SO4/PM25_UK_pcm
# PC <- PC/PM25_UK_pcm
# SOA <- SOA/PM25_UK_pcm
  
NITRATE <- (NIT/PM25_CMAQ)*OE
SULFATE <- (SULF/PM25_CMAQ)*OE
PRIM_CARBON <- (PC/PM25_CMAQ)*OE
SEC_ORG_AERO <- (SOA*PM25_CMAQ)*OE
# AAA <- ((PM25_UK_pcm - PM25_CMAQ)/PM25_UK_pcm)*100  ## not working
  AAA <- PM25_UK_pcm - PM25_CMAQ  ### OK
# AAA <- ((PM25_UK_pcm - PM25_CMAQ)/PM25_UK_pcm)*OE  ## not working really good



mydata <- cbind(OE_PM25$Lon, OE_PM25$Lat, OE, BIAS, PM25_UK_pcm,
                 AAA, URB, ED, NITRATE, SULFATE, PRIM_CARBON,
                 SEC_ORG_AERO, PM25_CMAQ)
 
 colnames(mydata) <- c("Lon", "Lat", "OE", "BIAS", "PM25_UK_pcm", "AAA", "URB",
                       "ED", "NITRATE", "SULFATE", "PRIM_CARBON",
                       "SEC_ORG_AERO", "PM25_CMAQ")
 
 mydata <- as.data.frame(mydata)
 
 X <- as.numeric(mydata$Lon)
 Y <- as.numeric(mydata$Lat)
 
#  URB <- as.numeric(mydata$URB)
#  ED <- as.numeric(mydata$ED)
#  NIT <- as.numeric(mydata$NIT)
#  SO4 <- as.numeric(mydata$SO4)
#  PC <- as.numeric(mydata$PC)
#  SOA <- as.numeric(mydata$SOA)
#  PM25_CMAQ <- as.numeric(mydata$PM25_CMAQ)

 # Create New Dataframe with categorical variables accounted, for regression modelling

# mydata1 <- data.frame(BIAS, X, Y, URB, PC)
 mydata1 <- data.frame(BIAS, X, Y, URB, AAA) 
#  mydata1 <- data.frame(BIAS, X, Y, URB, NITRATE)
# mydata1 <- data.frame(BIAS, X, Y, URB, NITRATE, PRIM_CARBON, AAA, SEC_ORG_AERO, SULFATE)
# mydata1 <- data.frame(BIAS, X, Y, URB, NIT, PC, SOA)
# mydata1 <- data.frame(BIAS, X, Y, URB, NIT, SO4)
# mydata1 <- data.frame(BIAS, X, Y,URB)

mydata1[is.na(mydata1)] <- 0
 
 
 nn <- 100/nrow(mydata1)
 memory.limit()
 # memory.limit(size=50000)
 
  gwr100 <- gwr(BIAS ~ URB + AAA, data=mydata1,
                coords=cbind(mydata1$X, mydata1$Y), 
                adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)
 

 #  gwr100 <- gwr(BIAS ~ URB + PRIM_CARBON, data=mydata1,
#                coords=cbind(mydata1$X, mydata1$Y), 
#                adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)
  
 # gwr100 <- gwr(BIAS ~ URB, data=mydata1,
 #               coords=cbind(mydata1$X, mydata1$Y), 
 #               adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)
 
#     gwr100 <- gwr(BIAS ~ URB + NITRATE + PRIM_CARBON + SULFATE, data=mydata1,
#           coords=cbind(mydata1$X, mydata1$Y), 
#             adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)
 
#    gwr100 <- gwr(BIAS ~ URB + NITRATE + PRIM_CARBON + SULFATE + 
#                  SEC_ORG_AERO, data=mydata1,
#                  coords=cbind(mydata1$X, mydata1$Y), 
#                  adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)

 #   gwr100 <- gwr(BIAS ~ URB + NITRATE + SULFATE, data=mydata1,
 #                coords=cbind(mydata1$X, mydata1$Y), 
 #                adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)
 
 
#     gwr100 <- gwr(BIAS ~ URB + NITRATE, data=mydata1,
#                   coords=cbind(mydata1$X, mydata1$Y), 
#                   adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)
   
   
gwr100
 
write.csv(gwr100$SDF, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/GWR100_London_PM25_1km.csv")
 
GWR_BIAS <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/GWR100_London_PM25_1km.csv",header = TRUE)
 
GWR_BIAS$In_situ <- mydata$PM25_UK_pcm
GWR_BIAS$OE_PM25 <- mydata$OE
GWR_BIAS$URB_GWR <- (mydata1$URB)*(GWR_BIAS$URB)  ###urban fraction ug/m3/% 
GWR_BIAS$AAA_GWR <- (mydata1$AAA)*(GWR_BIAS$AAA)
# GWR_BIAS$ED_GWR <- (mydata1$ED)*(GWR_BIAS$ED)  ### ug/m3/m 
# GWR_BIAS$NITRATE_GWR <- (mydata1$NITRATE)*(GWR_BIAS$NITRATE)
# GWR_BIAS$SULFATE_GWR <- (mydata1$SULFATE)*(GWR_BIAS$SULFATE)
# GWR_BIAS$PRIM_CARBON_GWR <- (mydata1$PRIM_CARBON)*(GWR_BIAS$PRIM_CARBON)
# GWR_BIAS$SEC_ORG_AERO_GWR <- (mydata1$SEC_ORG_AERO)*(GWR_BIAS$SEC_ORG_AERO)

 # df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$URB_GWR, GWR_BIAS$NITRATE_GWR)
 
#   df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$URB_GWR, GWR_BIAS$NITRATE_GWR,
#                     GWR_BIAS$PRIM_CARBON_GWR, GWR_BIAS$SEC_ORG_AERO_GWR)
 
#   df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$URB_GWR, GWR_BIAS$NITRATE_GWR,
#                    GWR_BIAS$PRIM_CARBON_GWR, GWR_BIAS$SEC_ORG_AERO_GWR,
#                    GWR_BIAS$SULFATE_GWR)

df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$URB_GWR, GWR_BIAS$AAA_GWR)
 
AOE <- rowSums(df, na.rm=T)
GWR_AOE <- cbind(GWR_BIAS, AOE)
 
write.csv(GWR_AOE, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/GWR_AOE_London_%.csv")
 
 
