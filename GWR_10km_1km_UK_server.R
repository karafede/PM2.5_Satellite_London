
library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(raster)   ## To convert an "Extent" object to a "SpatialPolygons" object.
library(rgeos)
library(rgdal)
library(plyr)
library(dplyr)

# Set a working directory as in gisdev server
setwd("/SAN/data/Satellite_PM25")
setwd("C:/RICARDO-AEA/Donkelaar_1Km/server")

######## Load Satellite data from Donkelaar et al. #################################

  PM25_UK_2009_2011 <- read.csv("PM25_UK_2009_2011_clean.csv", header = TRUE) 
  
  PM25_UK_2009_2011$x <- PM25_UK_2009_2011$Lon # define x & y as longitude and latitude
  PM25_UK_2009_2011$y <- PM25_UK_2009_2011$Lat
  
  coordinates(PM25_UK_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
 
  x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
  
  ### expand points to grid 1km resolution
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                     y = seq(from = y.range[1], to = y.range[2], by = 0.1))  
  
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
 
 # ### apply idw model for the data (interpolation)
  idw <- idw(formula = PM25 ~ 1, locations = PM25_UK_2009_2011, 
             newdata = grd)  
  
  idw.output = as.data.frame(idw)  # output is defined as a data table
  names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_1km")  # give names to the modelled variables
  
  write.csv(idw.output, file = "PM25_UK_2009_2011_10km.csv", row.names=FALSE)
  
  
 ############   UK AIR ################################################################
#   
#   PM25_UK_AIR_2010_2012 <- read.csv("PM25_UK_AVG_AIR_2010-12.csv", header = TRUE) 
#   
#   PM25_UK_AIR_2010_2012$x <- PM25_UK_AIR_2010_2012$Lon
#   PM25_UK_AIR_2010_2012$y <- PM25_UK_AIR_2010_2012$Lat
#   
#   coordinates(PM25_UK_AIR_2010_2012) = ~x + y  ## Set spatial coordinates to create a Spatial object:
#   
#   x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
#   y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
#   
#   grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
#                      y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
#   coordinates(grd) <- ~x + y
#   gridded(grd) <- TRUE
#   
#   idw <- idw(formula = PM25_UK_AIR_AVG ~ 1, locations = PM25_UK_AIR_2010_2012, 
#              newdata = grd)  # apply idw model for the data (interpolation)
#   
#   idw.output = as.data.frame(idw)  # output is defined as a data table
#   names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_UK_AIR_AVG_1km")  # give names to the modelled variables
#   
#   write.csv(idw.output, file = "PM25_EN_AIR_AVG_2010_2012_interp.csv", row.names=FALSE)
#  
 
 ############ PCM data #############################################################
 
#  PM25_pcm_2009_2011 <- read.csv("PCM_2009_2011_AVG_WGS84.csv", header = TRUE) 
#  
#  PM25_pcm_2009_2011$x <- PM25_pcm_2009_2011$Lon
#  PM25_pcm_2009_2011$y <- PM25_pcm_2009_2011$Lat
#  
#  coordinates(PM25_pcm_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
#  
#  x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
#  y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
#  
#  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
#                     y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
#  
#  coordinates(grd) <- ~x + y
#  gridded(grd) <- TRUE
#  
#  idw <- idw(formula = pcm_2009_11 ~ 1, locations = PM25_pcm_2009_2011, 
#             newdata = grd)  # apply idw model for the data (interpolation)
#  
#  idw.output = as.data.frame(idw)  # output is defined as a data table
#  names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_pcm_1km")  # give names to the modelled variables
#  
#  write.csv(idw.output, file = "PM25_pcm_EN_2009_2011_interp.csv", row.names=FALSE)
#  
#  
#  ############   UK ALTITUDE from UK AIR #############################################################
#  
#  Altitude_UK <- read.csv("Altitude_UK_AIR_2010-12.csv", header = TRUE) 
#  
#  Altitude_UK$x <- Altitude_UK$Lon
#  Altitude_UK$y <- Altitude_UK$Lat
#  
#  coordinates(Altitude_UK) = ~x + y  ## Set spatial coordinates to create a Spatial object:
#  
#  x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
#  y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
#  
#  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
#                     y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
#  
#  coordinates(grd) <- ~x + y
#  gridded(grd) <- TRUE
#  
#  idw <- idw(formula = Altitude ~ 1, locations = Altitude_UK, 
#             newdata = grd)  # apply idw model for the data
#  
#  idw.output = as.data.frame(idw)  # output is defined as a data table
#  names(idw.output)[1:3] <- c("Lon", "Lat", "Altitude")  # give names to the modelled variables
#  
#  write.csv(idw.output, file = "Altitude_EN_interp.csv", row.names=FALSE)
#  
#  
 ############  Topography UK ######################################################
#  
#  Topography_UK <- read.csv("Topography_UK_WGS84.csv",
#                            header = TRUE) 
#  
#  Topography_UK$x <- Topography_UK$X
#  Topography_UK$y <- Topography_UK$Y
#  
#  coordinates(Topography_UK) = ~x + y  ## Set spatial coordinates to create a Spatial object:
#  
#  x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
#  y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
#  
#  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
#                     y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
#  
#  coordinates(grd) <- ~x + y
#  gridded(grd) <- TRUE
#  
#  idw <- idw(formula = ETOPO1GRAY ~ 1, locations = Topography_UK, 
#             newdata = grd)  # apply idw model for the data
#  
#  idw.output = as.data.frame(idw)  # output is defined as a data table
#  names(idw.output)[1:3] <- c("Lon", "Lat", "TOPO_EN")  # give names to the modelled variables
#  
#  write.csv(idw.output, file = "Topography_EN_interp.csv", row.names=FALSE)
#  
#  
#  
#  ############  Land Cover UK ######################################################
#  #### MODIS data 2007 ############################################################
#  
#  Land_Cover_UK <- read.csv("Land_Cover_no_water.csv", header = TRUE)  
#  
#  x <- as.data.frame(Land_Cover_UK$X) 
#  y <- as.data.frame(Land_Cover_UK$Y)
#  
#  ##### only keep urban fraction  Lancover == 13 #########
#  
#  Land_Cover_UK[Land_Cover_UK < 13]<- 0
#  Land_Cover_UK[Land_Cover_UK > 13]<- 0
#  
#  Land_Cover_UK <- cbind(x,y, (Land_Cover_UK$LANDCOVERU/13)*100)  ### urban fraction(%)
#  colnames(Land_Cover_UK) <- c("x", "y", "Land_URB")
#  
#  coordinates(Land_Cover_UK) = ~x + y  ## Set spatial coordinates to create a Spatial object:
#  
#  x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
#  y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
#  
#  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
#                     y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid
#  
#  coordinates(grd) <- ~x + y
#  gridded(grd) <- TRUE
#  
#  idw <- idw(formula = Land_URB ~ 1, locations = Land_Cover_UK, 
#             newdata = grd)  # apply idw model for the data
#  
#  idw.output = as.data.frame(idw)  # output is defined as a data table
#  names(idw.output)[1:3] <- c("Lon", "Lat", "Land_URB")  # give names to the modelled variables
#  
#  write.csv(idw.output, file = "Land_Cover_EN_interp.csv", row.names=FALSE)
#  
#  


################# PM25 from CMAQ model ###############################################

 PM25_2009_2011 <- read.csv("PM25_CMAQ_2009_2011.csv", header = TRUE) 
 
 PM25_2009_2011$x <- PM25_2009_2011$Lon # define x & y as longitude and latitude
 PM25_2009_2011$y <- PM25_2009_2011$Lat
 
 coordinates(PM25_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
 
 x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
 y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
 
 grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1),
                    y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid (1km)
 
 coordinates(grd) <- ~x + y
 gridded(grd) <- TRUE
 
 idw <- idw(formula = PM25_AVG ~ 1, locations = PM25_2009_2011, 
            newdata = grd)  # apply idw model for the data
 
 idw.output = as.data.frame(idw)  # output is defined as a data table
 names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_AVG")  # give names to the modelled variables
 
 write.csv(idw.output, file = "PM25_CMAQ_2009_2011_10km.csv", row.names=FALSE)
# 
# 
# ################# NO3 from CMAQ model ###############################################
# 
# NO3_2009_2011 <- read.csv("PM25_NO3_CMAQ_2009_2011.csv", header = TRUE) 
# 
# NO3_2009_2011$x <- NO3_2009_2011$Lon # define x & y as longitude and latitude
# NO3_2009_2011$y <- NO3_2009_2011$Lat
# 
# coordinates(NO3_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
# 
# x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
# y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
# 
# grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
#                    y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid (1km)
# 
# coordinates(grd) <- ~x + y
# gridded(grd) <- TRUE
# 
# 
# idw <- idw(formula = NO3_AVG ~ 1, locations = NO3_2009_2011, 
#            newdata = grd)  # apply idw model for the data
# 
# idw.output = as.data.frame(idw)  # output is defined as a data table
# names(idw.output)[1:3] <- c("Lon", "Lat", "NO3_AVG")  # give names to the modelled variables
# 
# write.csv(idw.output, file = "NO3_CMAQ_2009_2011_EN_interp.csv", row.names=FALSE)
# 
# 
# ################# SO4 from CMAQ model ###############################################
# 
# SO4_2009_2011 <- read.csv("PM25_SO4_CMAQ_2009_2011.csv", header = TRUE) 
# 
# SO4_2009_2011$x <- SO4_2009_2011$Lon # define x & y as longitude and latitude
# SO4_2009_2011$y <- SO4_2009_2011$Lat
# 
# coordinates(SO4_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
# 
# x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
# y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
# 
# grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
#                    y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid (1km)
# 
# coordinates(grd) <- ~x + y
# gridded(grd) <- TRUE
# 
# idw <- idw(formula = SO4_AVG ~ 1, locations = SO4_2009_2011, 
#            newdata = grd)  # apply idw model for the data
# 
# idw.output = as.data.frame(idw)  # output is defined as a data table
# names(idw.output)[1:3] <- c("Lon", "Lat", "SO4_AVG")  # give names to the modelled variables
# 
# write.csv(idw.output, file = "SO4_CMAQ_2009_2011_EN_interp.csv", row.names=FALSE)
# 
# 
# 
# ################# POC from CMAQ model ###############################################
# 
# POC_2009_2011 <- read.csv("PM25_POC_CMAQ_2009_2011.csv", header = TRUE) 
# 
# POC_2009_2011$x <- POC_2009_2011$Lon # define x & y as longitude and latitude
# POC_2009_2011$y <- POC_2009_2011$Lat
# 
# coordinates(POC_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
# 
# x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
# y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
# 
# grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
#                    y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid (1km)
# 
# coordinates(grd) <- ~x + y
# gridded(grd) <- TRUE
# 
# idw <- idw(formula = POC_AVG ~ 1, locations = POC_2009_2011, 
#            newdata = grd)  # apply idw model for the data
# 
# idw.output = as.data.frame(idw)  # output is defined as a data table
# names(idw.output)[1:3] <- c("Lon", "Lat", "POC_AVG")  # give names to the modelled variables
# 
# write.csv(idw.output, file = "POC_CMAQ_2009_2011_EN_interp.csv", row.names=FALSE)
# 
# 
# 
# ################# SOAA from CMAQ model ###############################################
# 
# SOAA_2009_2011 <- read.csv("PM25_SOAA_CMAQ_2009_2011.csv",
#                            header = TRUE) 
# 
# SOAA_2009_2011$x <- SOAA_2009_2011$Lon # define x & y as longitude and latitude
# SOAA_2009_2011$y <- SOAA_2009_2011$Lat
# 
# coordinates(SOAA_2009_2011) = ~x + y  ## Set spatial coordinates to create a Spatial object:
# 
# x.range <- as.numeric(c(-5.8, 1.801))  # min/max longitude of the interpolation area
# y.range <- as.numeric(c(49.883, 55.832))  # min/max latitude of the interpolation area
# 
# grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
#                    y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid (1km)
# 
# coordinates(grd) <- ~x + y
# gridded(grd) <- TRUE
# 
# idw <- idw(formula = SOAA_AVG ~ 1, locations = SOAA_2009_2011, 
#            newdata = grd)  # apply idw model for the data
# 
# idw.output = as.data.frame(idw)  # output is defined as a data table
# names(idw.output)[1:3] <- c("Lon", "Lat", "SOAA_AVG")  # give names to the modelled variables
# 
# write.csv(idw.output, file = "SOAA_CMAQ_2009_2011_EN_interp.csv", row.names=FALSE)
# 
# 

library(spgwr)
library(AICcmodavg)
library("usdm")
library("nortest")
library(usdm)
library(nortest)


OE_PM25 <- read.csv("PM25_EN_1km_2009_2011_interp.csv",header = TRUE)
In_situ_PM25 <- read.csv("PM25_EN_AIR_AVG_2010_2012_interp.csv",header = TRUE)
PM25_pcm <- read.csv("PM25_pcm_EN_2009_2011_interp.csv", header = TRUE)
Land_Cover <- read.csv("Land_Cover_EN_interp.csv",header = TRUE)
Altitude_London <- read.csv("Altitude_EN_interp.csv",header = TRUE)
TOPOGRAPHY <- read.csv("Topography_EN_interp.csv",header = TRUE)

NIT <- read.csv("NO3_CMAQ_2009_2011_EN_interp.csv",header = TRUE)
SO4 <- read.csv("SO4_CMAQ_2009_2011_EN_interp.csv",header = TRUE)
POC <- read.csv("POC_CMAQ_2009_2011_EN_interp.csv",header = TRUE)
SOAA <- read.csv("SOAA_CMAQ_2009_2011_EN_interp.csv",header = TRUE)
PM25 <- read.csv("PM25_CMAQ_2009_2011_EN_interp.csv", header = TRUE)

BIAS <- PM25_pcm$PM25_pcm_1km - OE_PM25$PM25_1km  ### use pcm data as surrogate for UK-AIR ground measurements data

PM25_UK_AIR <- In_situ_PM25$PM25_UK_AIR_AVG_1km
PM25_UK_pcm <- PM25_pcm$PM25_pcm_1km
OE <- OE_PM25$PM25_1km  ### satellite optical estimation PM2.5
ED <- TOPOGRAPHY$TOPO_EN - Altitude_London$Altitude
URB <- Land_Cover$Land_URB
NIT <- NIT$NO3_AVG
SO4 <- SO4$SO4_AVG
PC <- POC$POC_AVG
SOA <- SOAA$SOAA_AVG
PM25_CMAQ <- PM25$PM25_AVG

# NIT <- (NIT/PM25_CMAQ)*PM25_UK_pcm - (NIT/PM25_CMAQ)*OE
# SO4 <- (SO4/PM25_CMAQ)*PM25_UK_pcm - (SO4/PM25_CMAQ)*OE
# PC <- (PC/PM25_CMAQ)*PM25_UK_pcm - (PC/PM25_CMAQ)*OE
# SOA <- (SOA/PM25_CMAQ)*PM25_UK_pcm - (SOA/PM25_CMAQ)*OE

# NIT <- NIT/PM25_UK_pcm
# SO4 <- SO4/PM25_UK_pcm
# PC <- PC/PM25_UK_pcm
# SOA <- SOA/PM25_UK_pcm

NIT <- (NIT/PM25_CMAQ)*OE
SO4 <- (SO4/PM25_CMAQ)*OE
PC <- (PC/PM25_CMAQ)*OE
SOA <- (SOA*PM25_CMAQ)*OE

mydata <- cbind(OE_PM25$Lon, OE_PM25$Lat, OE, BIAS, 
                PM25_UK_pcm, URB, ED, NIT, SO4, PC, SOA, PM25_CMAQ)

colnames(mydata) <- c("Lon", "Lat", "OE", "BIAS", "PM25_UK_pcm", "URB",
                      "ED", "NIT", "SO4", "PC", "SOA", "PM25_CMAQ")

mydata <- as.data.frame(mydata)

X <- as.numeric(mydata$Lon)
Y <- as.numeric(mydata$Lat)

URB <- as.numeric(mydata$URB)
ED <- as.numeric(mydata$ED)
NIT <- as.numeric(mydata$NIT)
SO4 <- as.numeric(mydata$SO4)
PC <- as.numeric(mydata$PC)
SOA <- as.numeric(mydata$SOA)
PM25_CMAQ <- as.numeric(mydata$PM25_CMAQ)

# Create New Dataframe with categorical variables accounted, for regression modelling

# mydata1 <- data.frame(BIAS, X, Y,URB, ED)
# mydata1 <- data.frame(BIAS, X, Y, URB, PM25_CMAQ) 
#  mydata1 <- data.frame(BIAS, X, Y, URB, NIT)
#  mydata1 <- data.frame(BIAS, X, Y, URB, NIT, PC, SOA, SO4)
# mydata1 <- data.frame(BIAS, X, Y, URB, NIT, PC, SOA)
# mydata1 <- data.frame(BIAS, X, Y, URB, NIT, SO4)
mydata1 <- data.frame(BIAS, X, Y,URB)

mydata1[is.na(mydata1)] <- 0

nn <- 100/nrow(mydata1)

gwr100 <- gwr(BIAS ~ URB, data=mydata1,
              coords=cbind(mydata1$X, mydata1$Y), 
              adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)

# gwr100 <- gwr(BIAS ~ URB, data=mydata1,
#               coords=cbind(mydata1$X, mydata1$Y), 
#               adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)

#     gwr100 <- gwr(BIAS ~ URB + NIT + PC + SOA, data=mydata1,
#           coords=cbind(mydata1$X, mydata1$Y), 
#             adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)

#   gwr100 <- gwr(BIAS ~ URB + NIT + PC + SOA + SO4, data=mydata1,
#                 coords=cbind(mydata1$X, mydata1$Y), 
#                 adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)

#   gwr100 <- gwr(BIAS ~ URB + NIT + SO4, data=mydata1,
#                coords=cbind(mydata1$X, mydata1$Y), 
#                adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)


#   gwr100 <- gwr(BIAS ~ URB + NIT, data=mydata1,
#                 coords=cbind(mydata1$X, mydata1$Y), 
#                 adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)

# gwr100

write.csv(gwr100$SDF, file = "GWR100_England_PM25_1km.csv")

GWR_BIAS <- read.csv("GWR100_England_PM25_1km.csv",header = TRUE)

GWR_BIAS$In_situ <- mydata$PM25_UK_pcm
GWR_BIAS$OE_PM25 <- mydata$OE
GWR_BIAS$URB_GWR <- (mydata1$URB)*(GWR_BIAS$URB)  ###urban fraction ug/m3/% 
# GWR_BIAS$ED_GWR <- (mydata1$ED)*(GWR_BIAS$ED)  ### ug/m3/m 
# GWR_BIAS$NIT_GWR <- (mydata1$NIT)*(GWR_BIAS$NIT)
#  GWR_BIAS$SO4_GWR <- (mydata1$SO4)*(GWR_BIAS$SO4)
# GWR_BIAS$PC_GWR <- (mydata1$PC)*(GWR_BIAS$PC)
#  GWR_BIAS$SOA_GWR <- (mydata1$SOA)*(GWR_BIAS$SOA)
# GWR_BIAS$PM25_CMAQ_GWR <- (mydata1$PM25_CMAQ)*(GWR_BIAS$PM25_CMAQ)

df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$URB_GWR)

# df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$URB_GWR, GWR_BIAS$NIT_GWR)

#     df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$URB_GWR, GWR_BIAS$NIT_GWR,
#                     GWR_BIAS$PC_GWR, GWR_BIAS$SOA_GWR)

#    df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$URB_GWR, GWR_BIAS$NIT_GWR,
#                     GWR_BIAS$PC_GWR, GWR_BIAS$SOA_GWR, GWR_BIAS$SO4_GWR)

# df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$UGWR_BIAS$SO4_GWRRB_GWR, GWR_BIAS$NIT_GWR,
#               GWR_BIAS$PC_GWR, )

#  df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$URB_GWR,
#                   GWR_BIAS$NIT_GWR,GWR_BIAS$SO4_GWR)

AOE <- rowSums(df, na.rm=T)
GWR_AOE <- cbind(GWR_BIAS,AOE)

write.csv(GWR_AOE, file = "GWR_AOE_England_URB.csv")


############ MAPPING ##########################################################
###############################################################################

### Read shape file #########

dir <- "/SAN/data/Satellite_PM25/UK_shp"

### shapefile for local authorities in England
shp <- readOGR(dsn = dir, layer = "UK_lad")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
# names(shp)

shp@data$name <- 1:nrow(shp)

# PM25_UK_Sat <- idw.output[,1:3]
PM25_UK_Sat <- read.csv("PM25_UK_1km_2009_2011_interp.csv", header = TRUE) 

# #### define projection for PM25_London_Sat dataframe
crs <- projection(shp) ### get projections from shp file

PM25_UK_Sat <- SpatialPointsDataFrame(PM25_UK_Sat[,1:2], PM25_UK_Sat, 
                                      proj4string=CRS(crs)) 

# plot(shp)
# points(PM25_UK_Sat, pch=10)

#### Points into inside polygons
pts.poly <- over(PM25_UK_Sat, shp[,"id"])
PM25_UK_Sat$id <- pts.poly$id

# Aggregate by zone/polygon
###  Make a dataframe ###
Sat_data_points <- PM25_UK_Sat@data

Sat_data_points <- Sat_data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(pm25_mean = mean(PM25_1km)) 
#    dplyr::ungroup()


# Join aggregation to polygons 
shp@data <- shp@data %>% 
  left_join(Sat_data_points, "id")

# Filter out polygons with no data
shp <- subset(shp, !is.na(pm25_mean))

# Export shp file

writeOGR(shp,"Local Authorities_England",
         "LA_Join_PM25_UK_Sat_2009_2011", driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

Sat_data_points <- shp@data[,c("id","name","pm25_mean")]
row.names(Sat_data_points) <- row.names(shp)

shp <- SpatialPolygonsDataFrame(shp, data=Sat_data_points)

#### Write GeoJSON for Leaflet application ############################
# ----- Write data to GeoJSON

# ----- Write data to GeoJSON
dir <- "/SAN/data/Satellite_PM25"
leafdat<-paste(dir, "/",  ".UK_geojson_PM25_1km_Sat_2009_2011", sep="") 

####  ATT !!!!! erase existing .geojson file when re-runing code ######
writeOGR(shp, leafdat, layer="", driver="GeoJSON")  ## erase existing .geojson file when re-runing code 


 
