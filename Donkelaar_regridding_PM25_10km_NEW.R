
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


### Load PM2.5 satellite derived data from Donkelaar 2001-2006, Relative Humidity 50% #####
### UK region 10km resolution #####

# PM25_UK <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_UK_clean_British.csv",
  #                  header = TRUE)  

# PM25_UK <- read.csv("C:/Ricardo-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_UK_clean_British.csv",
#                    header = TRUE)
# PM25_UK$new_variable <- PM25_UK[,2]

# PM25_UK_2010_2012 <- read.csv("C:/Ricardo-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_UK_2010_2012_clean_British.csv",
  #                  header = TRUE)  


# PM25_UK_2010_2012 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_UK_2010_2012_clean.csv",
 #                             header = TRUE) 

# PM25_UK_AIR_AVG_2010_12 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_UK_AVG_AIR_2010-12.csv",
  #                            header = TRUE) 


######## Read Shapefile London Polygons & Border #####################
#### PM25 10km sat data, Landcover URB, Topography ###################

# dir <- "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border"
# London_Border <- readOGR(dsn = dir, layer = "London_Border_WGS84")
# PM25_UK_2010_2012 <- readOGR(dsn = dir, layer = "PM25_UK_2010-12_clean")
# names(PM25_UK_2010_2012)


#Land_Cover_London <- readOGR(dsn = dir, layer = "Land_Cover_London")
# Topography_UK <- readOGR(dsn = dir, layer = "Topography_UK_poly")

## Clip the data from satellite
# London_PM25_10km <- PM25_UK_2010_2012[London_Border, ]
# plot(London_Border)
# points(London_PM25_10km)

# Land_CoverLondon <- Land_Cover_London[London_Border, ]
# plot(London_Border)
# points(Land_Cover_UK_London)

# Topography_London <- Topography_UK[London_Border, ]
# plot(London_Border)
# points(Topography_London)


### save data ########

# London_PM25_10km <- as.data.frame(London_PM25_10km)
# Land_Cover_London <- as.data.frame(Land_Cover_London)
# Topography_London <- as.data.frame(Topography_London)

# write.csv(London_PM25_10km, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/London_PM25_10km.csv", row.names=FALSE)
# write.csv(Land_Cover_London, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Land_Cover_London.csv", row.names=FALSE)
# write.csv(Topography_London, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Topography_London.csv", row.names=FALSE)


######## Load Satellite data from Donkelaar et al. #################################

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

library(rgdal)
library(ggplot2)
library(maptools)
library(plyr)
library(leafletR)
library(rgeos) #for simplification
library(sp)
library(rbison)
library(devtools)
library(rgbif)
library(RColorBrewer)
library(spatialEco)

###################### Merge data into Local Authorities shapefiles ############
################################################################################


dir <- "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Local Authorities_England"
### shapefile for local authorities in England
shp <- readOGR(dsn = dir, layer = "UK_lad")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
names(shp)


head(shp@data)  # polygons
shp@data$name <- 1:nrow(shp)

PM25_London_Sat <- idw.output[,1:3]

#### define projection for PM25_London_Sat dataframe

crs <- projection(shp) ### get projections from shp file
# coordinates(PM25_London_Sat) = ~ Lon+Lat
PM25_London_Sat <- SpatialPointsDataFrame(PM25_London_Sat[,1:2], PM25_London_Sat, 
                                   proj4string=CRS(crs)) 

summary(PM25_London_Sat) 


plot(shp)
points(PM25_London_Sat, pch=10)

#### Points into inside polygons
 #pts.poly <- over(PM25_London_Sat, shp[,"id"])
pts.poly <- point.in.poly(PM25_London_Sat, shp) ### requre Library (SpatialEco)
# PM25_London_Sat$id <- pts.poly$id


# pts.poly <- point.in.poly(PM25_London_Sat, shp)   #### assign PM25 sat data to Local Authorities polygons
# writeOGR(pts.poly,"C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Local Authorities_England",
#          "LA_Join_PM25_London_Sat_2009_2011", driver = "ESRI Shapefile")


head(pts.poly@data)

###  Make a dataframe ###
# data_points <- PM25_London_Sat@data

# Aggregate by zone/polygon
data_points <- pts.poly@data 
names(data_points)

data_points <- data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(pm25_mean = mean(PM25_1km)) %>% 
  dplyr::ungroup()


# Join aggregation to polygons 
shp@data <- shp@data %>% 
  left_join(data_points, "id")

# Filter out polygons with no data
shp <- subset(shp, !is.na(pm25_mean))


# Transform projection system (British projections)
# shp <- spTransform(shp, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
plot(shp)

# Export shp file
head(shp@data)
writeOGR(shp,"C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Local Authorities_England",
         "LA_Join_PM25_London_Sat_2009_2011", driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


data_points <- shp@data[,c("id","name","pm25_mean")]
row.names(data_points) <- row.names(shp)

shp <- SpatialPolygonsDataFrame(shp, data=data_points)

row.names(shp)
row.names(data_points)


#### Write GeoJSON for Leaflet application ############################
# ----- Write data to GeoJSON
dir <- "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet"
leafdat<-paste(dir, "/",  ".geojson_PM25_1km_Sat_2009_2011", sep="") 
leafdat

####  ATT !!!!! erase existing .geojson file when re-runing code ######
writeOGR(shp, leafdat, layer="", driver="GeoJSON")  ## erase existing .geojson file when re-runing code 


# ----- Create the cuts
pal <- brewer.pal(10,"PiYG")

names(shp)
cuts_PM25_sat <-  round(quantile(shp$pm25_mean, probs = seq(0, 1, 0.1), na.rm = TRUE),0)
cuts_PM25_sat[1]<- 8  ### lower value in the scale

# ----Popout from the maps
popup_PM25_sat <- c("LA(id)","PM2.5_Sat_2009_2011") 

# ----Define Styles

sty_PM25_sat <- styleGrad(prop="PM2.5_Sat_2009_2011", breaks=cuts_PM25_sat,
                          right=FALSE, style.par="col",
                          style.val=pal, leg="PM2.5(ug/m3) Satellite 2009-2011",
                          lwd=1, col = 1.7)

# ----- Create the map and load into browser

map_PM25_sat <- leaflet(data=leafdat, dest=dir, style = sty_PM25_sat,
                             title="PM25_sat", base.map="osm",
                             incl.data=TRUE,  popup = popup_PM25_sat)



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




#####################################################################################
################# NIT from EMEP model ###############################################

EMEP_modelled <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/EMEP_Modelled/EMEP_data.csv",
                                  header = TRUE) 

EMEP_modelled$x <- EMEP_modelled$Lon # define x & y as longitude and latitude
EMEP_modelled$y <- EMEP_modelled$Lat

coordinates(EMEP_modelled) = ~x + y  ## Set spatial coordinates to create a Spatial object:
plot(EMEP_modelled)

x.range <- as.numeric(c(-1.37, 1.090))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(51, 51.889))  # min/max latitude of the interpolation area

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.01),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.01))  # expand points to grid (1km)

coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

plot(grd, cex = 1.5, col = "grey")
points(EMEP_modelled, pch = 1, col = "red", cex = 1)

##### Sulphate

idw <- idw(formula = SO4_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "SO4_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/EMEP_Modelled/SO4_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = SO4_London))

##### Nitrate

idw <- idw(formula = NO3_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "NO3_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/EMEP_Modelled/NO3_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = NO3_London))

##### Primary Carbonaceous (PC)

idw <- idw(formula = EC_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "EC_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/EMEP_Modelled/EC_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = EC_London))

##### SOA (Secondary Organic Aerosols)

idw <- idw(formula = ASOA_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "ASOA_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/EMEP_Modelled/ASOA_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = ASOA_London))

idw <- idw(formula = BSOA_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "BSOA_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/EMEP_Modelled/BSOA_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = BSOA_London))

ASOA <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/ASOA_London_2013_interp.csv",
                  header = TRUE) 
BSOA <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/BSOA_London_2013_interp.csv",
                 header = TRUE) 
SOA <- cbind(ASOA$Lon, ASOA$Lon, ((ASOA$ASOA_London)+(BSOA$BSOA_London)))
colnames(SOA) <- c("Lon", "Lat", "SOA") 
write.csv(SOA, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/EMEP_Modelled/SOA_London_2013_interp.csv", row.names=FALSE)


##### PM2.5 from EMEP modeled

idw <- idw(formula = PM25_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/SATELLITE_STUFF/Donkelaar_1Km/EMEP_Modelled/PM25_EMEP_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_London))


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
 
 


########################################################################################
################ Regression plots ######################################################

library(gclus)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(gtools)

PM25_GWR <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/GWR_AOE_London_URB+NIT+SO4+PC+SOA.csv", header = TRUE)
PM25_GWR <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/GWR_AOE_London_URB_new.csv", header = TRUE)

summary <- cbind(PM25_pcm$PM25_pcm_1km,               ### pcm
                 # In_situ_PM25$PM25_UK_AIR_AVG_1km,     ### in situ
                 PM25$PM25_AVG,                       ### CMAQ
                 OE_PM25$PM25_1km,                    ### Satellite
                 PM25_GWR$AOE)

colnames(summary) <- c("PCM PM25", "CMAQ PM25", "Sat PM25", "Adj Sat PM25")
summary <- as.data.frame(summary)


# summary %>% 
#   ggplot(aes(PCM_PM25, CMAQ_PM25, group = 1)) + 
#   geom_point(size = 5) + 
#   stat_smooth(method = "lm") +
#   theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
#   theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
#   ggtitle("PM25 CMAQ vs PM25 PCM") + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size=18))



regrline = function(x,y) {
  points(x,y,pch=".")
  # abline(line(x,y),col="blue")  ### add a smooth line
  abline(lsfit(x,y),col="red")   ### add a regression line
}


panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(1, 0, 1, 0))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  #Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.5,cex = 3,  paste("r =",txt))
  #text(.5, .75, Signif)
}



panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}


panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}


# Basic Scatterplot Matrix
pairs(summary, 
      lower.panel = regrline,
      # lower.panel = panel.smooth,
      upper.panel = panel.cor,
      # diag.panel =  panel.hist,
      main="Simple Scatterplot Matrix",
      cex.labels = 3, font.labels = 1)


summary.r <- abs(cor(summary)) # get correlations
summary.col <- dmat.color(summary.r) # get colors
# reorder variables so those with highest correlation are closest to the diagonal
summary.o <- order.single(summary.r) 

cpairs(summary, summary.o, panel.colors=summary.col, gap=.5,
        # lower.panel = regrline, 
       # diag.panel =  panel.hist,
       upper.panel = panel.cor,
       main="Variables Ordered and Colored by Correlation",
       cex.labels = 3, font.labels = 1)

