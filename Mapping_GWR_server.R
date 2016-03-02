
library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(raster)   ## To convert an "Extent" object to a "SpatialPolygons" object.
library(rgeos)
library(rgdal)
library(plyr)
library(dplyr)
library(leaflet)
library(sp)
library(htmltools)
library(ncdf4)
library(htmlwidgets)
library(spatialEco)
library(mapview)

############ MAPPING ##########################################################
###############################################################################

### Read shape file #########

# dir <- "/SAN/data/Satellite_PM25/UK_shp"

setwd("C:/SATELLITE_STUFF/Donkelaar_1Km/server")
dir <- "C:/SATELLITE_STUFF/Donkelaar_1Km/server"

### shapefile for local authorities in England
shp <- readOGR(dsn = dir, layer = "EN_Wales")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
# names(shp)

shp@data$name <- 1:nrow(shp)

PM25_EN_SAT <- read.csv("PM25_EN_1km_2009_2011_interp.csv", header = TRUE)
# Jan_2013_SAT <- subset(Jan_2013_SAT, !is.na(JAN2013) & JAN2013>0)
PM25_EN_SAT_rast <- read.csv("PM25_UK_2009_2011_10km.csv", header = TRUE)

PM25_UK_AIR <- read.csv("PM25_EN_AIR_AVG_2010_2012_interp.csv", header = TRUE)
URB_Cover <- read.csv("Land_Cover_EN_interp.csv", header=TRUE)
Altitude <- read.csv("Altitude_EN_interp.csv", header = TRUE)
Topography <- read.csv("Topography_EN_interp.csv", header = TRUE)
ED <- Topography$TOPO_EN - Altitude$Altitude
pcm_PM25 <- read.csv("PM25_pcm_EN_2009_2011_interp.csv", header = TRUE)
cmaq_PM25 <- read.csv("PM25_CMAQ_2009_2011_EN_interp.csv",header = TRUE)
cmaq_PM25_rast_10km <- read.csv("PM25_CMAQ_2009_2011_10km.csv", header = TRUE)
cmaq_PM25_rast_1km <- read.csv("PM25_CMAQ_2009_2011_EN_interp.csv", header = TRUE)

### original input data are at 10km...then interpolated at 1km #######
# GWR_PM25 <- read.csv("GWR_AOE_1km_England_ALL.csv", header = TRUE)

### use results of GWR at 1km #######################################
GWR_PM25 <- read.csv("GWR_AOE_ENGLAND_clean_1km.csv", header = TRUE)
#  GWR_PM25 <- read.csv("GWR_AOE_ENGLAND_ALL.csv", header = TRUE)
#  ### replace all negative values with zeros ##########################
#  GWR_PM25$AOE[GWR_PM25$AOE < 0 | GWR_PM25$AOE > 20] <- 0
#  write.csv(AAA, file = "GWR_AOE_ENGLAND_clean.csv", row.names=FALSE)
MIN <- min(GWR_PM25$AOE)
MAX <- max(GWR_PM25$AOE)


### use results of GWR at 10km to create raster image #################
GWR_PM25_rast_10km <- read.csv("GWR_AOE_10km_England_ALL.csv", header = TRUE)
### use results of GWR at 1km to create raster image #################
GWR_PM25_rast_1km <- read.csv("GWR_AOE_ENGLAND_clean_1km.csv", header = TRUE)
# GWR_PM25_rast$AOE[GWR_PM25_rast$AOE < 0 | GWR_PM25_rast$AOE > 20] <- NA
# MIN <- min(GWR_PM25_rast$AOE)
# MAX <- max(GWR_PM25_rast$AOE)

GWR_PM25_rast_10km <- cbind(GWR_PM25_rast_10km$coord.x, GWR_PM25_rast_10km$coord.y, 
                        GWR_PM25_rast_10km$AOE)

GWR_PM25_rast_1km <- cbind(GWR_PM25_rast_1km$Lon, GWR_PM25_rast_1km$Lat, 
                       GWR_PM25_rast_1km$AOE)

colnames(GWR_PM25_rast_10km) <- c("Lon", "Lat", "AOE")
GWR_PM25_rast_10km <- as.data.frame(GWR_PM25_rast_10km)
colnames(GWR_PM25_rast_1km) <- c("Lon", "Lat", "AOE")
GWR_PM25_rast_1km <- as.data.frame(GWR_PM25_rast_1km)

# PM25_UK_Sat <- read.csv("PM25_UK_1km_2009_2011_interp.csv", header = TRUE) 

# #### define projection for PM25_London_Sat dataframe
crs <- projection(shp) ### get projections from shp file


PM25_EN_Sat <- cbind(PM25_EN_SAT[,1:3], PM25_UK_AIR[,3], GWR_PM25[,3],
                     pcm_PM25[,3], cmaq_PM25[,3])
colnames(PM25_EN_Sat) <- c("Lon", "Lat", "PM25_1km", "PM25_UK_AIR", "GWR_PM25",
                               "pcm_PM25", "cmaq_PM25")

#### create a raster for PM25 UK-AIR 1Km in England #######

coordinates(PM25_UK_AIR) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(PM25_UK_AIR) <- TRUE
raster_PM25_UK_AIR <- raster(PM25_UK_AIR)
projection(raster_PM25_UK_AIR) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_PM25_UK_AIR)
plot(shp)

#### crop the raster over the shp file ###########################
raster_PM25_UK_AIR_cropped <- crop(raster_PM25_UK_AIR, extent(shp))
raster_PM25_UK_AIR_cropped <- mask(raster_PM25_UK_AIR_cropped, shp)
  
plot(raster_PM25_UK_AIR_cropped)
# plot(shp, add=TRUE, lwd=2)

PM25_UK_AIR_nc <- writeRaster(raster_PM25_UK_AIR_cropped,
                              filename="PM25_UK_AIR.nc",
                              format="CDF", overwrite=TRUE) 
PM25_UK_AIR_nc <- raster("PM25_UK_AIR.nc")



#### create a raster for PM25_Sat 1km in England #######

coordinates(PM25_EN_SAT_rast) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(PM25_EN_SAT_rast) <- TRUE
raster_PM25_EN_SAT_rast <- raster(PM25_EN_SAT_rast)
projection(raster_PM25_EN_SAT_rast) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_PM25_EN_SAT_rast)
# mapview(raster_PM25_EN_SAT)

#### crop the raster over the shp file ###########################
raster_PM25_EN_SAT_rast_cropped <- crop(raster_PM25_EN_SAT_rast, extent(shp))
raster_PM25_EN_SAT_rast_cropped <- mask(raster_PM25_EN_SAT_rast_cropped, shp)

plot(raster_PM25_EN_SAT_rast_cropped)


PM25_EN_SAT_nc <- writeRaster(raster_PM25_EN_SAT_rast_cropped,
                           filename="PM25_EN_SAT_rast.nc",
                           format="CDF", overwrite=TRUE) 
PM25_EN_SAT_nc <- raster("PM25_EN_SAT_rast.nc")



#### create a raster for URB land cover in England #######

coordinates(URB_Cover) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(URB_Cover) <- TRUE
raster_URB_Cover <- raster(URB_Cover)
projection(raster_URB_Cover) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_URB_Cover)

#### crop the raster over the shp file ###########################
raster_URB_Cover_cropped <- crop(raster_URB_Cover, extent(shp))
raster_URB_Cover_cropped <- mask(raster_URB_Cover_cropped, shp)

plot(raster_URB_Cover_cropped)

URB_Cover_nc <- writeRaster(raster_URB_Cover_cropped,
                            filename="URB_Cover_London.nc",
                            format="CDF", overwrite=TRUE) 
URB_Cover_nc <- raster("URB_Cover_London.nc")



#### create a raster for GWR PM25 at 10km (Geographycally weighted regression) #######

coordinates(GWR_PM25_rast_10km) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(GWR_PM25_rast_10km) <- TRUE
raster_GWR_PM25_10km <- raster(GWR_PM25_rast_10km)
projection(raster_GWR_PM25_10km) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_GWR_PM25_10km)
# mapview(raster_GWR_PM25)

#### crop the raster over the shp file ###########################
raster_GWR_PM25_10km_cropped <- crop(raster_GWR_PM25_10km, extent(shp))
raster_GWR_PM25_10km_cropped <- mask(raster_GWR_PM25_10km_cropped, shp)

plot(raster_GWR_PM25_10km_cropped)
mapview(raster_GWR_PM25_10km_cropped)

GWR_PM25_10km_nc <- writeRaster(raster_GWR_PM25_10km_cropped,
                            filename="GWR_PM25_10km.nc",
                            format="CDF", overwrite=TRUE) 
GWR_PM25_10km_nc <- raster("GWR_PM25_10km.nc")



#### create a raster for GWR PM25 at 1km (Geographycally weighted regression) #######

coordinates(GWR_PM25_rast_1km) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(GWR_PM25_rast_1km) <- TRUE
raster_GWR_PM25_1km <- raster(GWR_PM25_rast_1km)
projection(raster_GWR_PM25_1km) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_GWR_PM25_1km)
# mapview(raster_GWR_PM25)

#### crop the raster over the shp file ###########################
raster_GWR_PM25_1km_cropped <- crop(raster_GWR_PM25_1km, extent(shp))
raster_GWR_PM25_1km_cropped <- mask(raster_GWR_PM25_1km_cropped, shp)

plot(raster_GWR_PM25_1km_cropped)
mapview(raster_GWR_PM25_1km_cropped)

GWR_PM25_1km_nc <- writeRaster(raster_GWR_PM25_1km_cropped,
                                filename="GWR_PM25_1km.nc",
                                format="CDF", overwrite=TRUE) 
GWR_PM25_1km_nc <- raster("GWR_PM25_1km.nc")



#### create a raster for PM2.5 pcm #####################################

coordinates(pcm_PM25) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(pcm_PM25) <- TRUE
raster_pcm_PM25 <- raster(pcm_PM25)
projection(raster_pcm_PM25) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_pcm_PM25)


#### crop the raster over the shp file ###########################
raster_pcm_PM25_cropped <- crop(raster_pcm_PM25, extent(shp))
raster_pcm_PM25_cropped <- mask(raster_pcm_PM25_cropped, shp)

plot(raster_pcm_PM25_cropped)

pcm_PM25_nc <- writeRaster(raster_pcm_PM25_cropped,
                           filename="pcm_PM25_raster.nc",
                           format="CDF", overwrite=TRUE) 
pcm_PM25_nc <- raster("pcm_PM25_raster.nc")
# mapview(pcm_PM25_nc) + mapview(raster_PM25_EN_SAT)


#### create a raster for PM2.5 CMAQ 10km #####################################

coordinates(cmaq_PM25_rast_10km) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(cmaq_PM25_rast_10km) <- TRUE
raster_cmaq_PM25_rast_10km <- raster(cmaq_PM25_rast_10km)
projection(raster_cmaq_PM25_rast_10km) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_cmaq_PM25_rast_10km)

#### crop the raster over the shp file ###########################
raster_cmaq_PM25_rast_10km_cropped <- crop(raster_cmaq_PM25_rast_10km, extent(shp))
raster_cmaq_PM25_rast_10km_cropped <- mask(raster_cmaq_PM25_rast_10km_cropped,shp)

plot(raster_cmaq_PM25_rast_10km_cropped)

cmaq_PM25_10km_nc <- writeRaster(raster_cmaq_PM25_rast_10km_cropped,
                            filename="cmaq_PM25_10km_raster.nc",
                            format="CDF", overwrite=TRUE) 
cmaq_PM25_10km_nc <- raster("cmaq_PM25_10km_raster.nc")


#### create a raster for PM2.5 CMAQ 1km #####################################

coordinates(cmaq_PM25_rast_1km) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(cmaq_PM25_rast_1km) <- TRUE
raster_cmaq_PM25_rast_1km <- raster(cmaq_PM25_rast_1km)
projection(raster_cmaq_PM25_rast_1km) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_cmaq_PM25_rast_1km)

#### crop the raster over the shp file ###########################
raster_cmaq_PM25_rast_1km_cropped <- crop(raster_cmaq_PM25_rast_1km, extent(shp))
raster_cmaq_PM25_rast_1km_cropped <- mask(raster_cmaq_PM25_rast_1km_cropped,shp)

plot(raster_cmaq_PM25_rast_1km_cropped)

cmaq_PM25_1km_nc <- writeRaster(raster_cmaq_PM25_rast_1km_cropped,
                                 filename="cmaq_PM25_1km_raster.nc",
                                 format="CDF", overwrite=TRUE) 
cmaq_PM25_1km_nc <- raster("cmaq_PM25_1km_raster.nc")


###################################################################################

#### define projection for PM25_London_Sat dataframe

crs <- projection(shp) ### get projections from shp file

PM25_EN_Sat <- SpatialPointsDataFrame(PM25_EN_Sat[,1:2], PM25_EN_Sat, 
                                      proj4string=CRS(crs)) 

# plot(shp)
# points(PM25_UK_Sat, pch=10)

#### Points into inside polygons
pts.poly <- point.in.poly(PM25_EN_Sat, shp) ### requre Library (SpatialEco)
# pts.poly <- over(PM25_EN_Sat, shp[,"id"])
# PM25_EN_Sat$id <- pts.poly$id

head(pts.poly@data)

# Aggregate by zone/polygon
###  Make a dataframe ###
# Sat_data_points <- PM25_EN_Sat@data

# Aggregate by zone/polygon
Sat_data_points <- pts.poly@data ### only  when running with SatialEco
names(Sat_data_points) ###  only when running with SpatialEco

Sat_data_points <- Sat_data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(pm25_mean = mean(PM25_1km),
                   pm25_mean_UK_AIR = mean(PM25_UK_AIR),
                   pm25_mean_GWR = mean(GWR_PM25),
                   pm25_mean_pcm = mean(pcm_PM25),
                   pm25_mean_cmaq = mean(cmaq_PM25)) %>%
  dplyr::ungroup() ### comment if not using SpatialEco


# Join aggregation to polygons 
shp@data <- shp@data %>% 
  left_join(Sat_data_points, "id")

# Filter out polygons with no data
shp <- subset(shp, !is.na(pm25_mean))

# Export shp file

writeOGR(shp,"Local Authorities_England",
         "LA_Join_PM25_ENGLAND_Sat_2009_2011", driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

Sat_data_points <- shp@data[,c("id","name","pm25_mean","pm25_mean_UK_AIR",
                               "pm25_mean_GWR", "pm25_mean_pcm", "pm25_mean_cmaq")]
row.names(Sat_data_points) <- row.names(shp)

shp <- SpatialPolygonsDataFrame(shp, data=Sat_data_points)

#### Write GeoJSON for Leaflet application ############################
# ----- Write data to GeoJSON

dir <- "/SAN/data/Satellite_PM25"
dir <- "C:/SATELLITE_STUFF/Donkelaar_1Km/server"
leafdat<-paste(dir, "/",  ".ENGLAND_geojson_PM25_1km_Sat_2009_2011", sep="") 

####  ATT !!!!! erase existing .geojson file when re-runing code ######
writeOGR(shp, leafdat, layer="", driver="GeoJSON")  ## erase existing .geojson file when re-runing code 

########################################################################
#### read GeoJSON for Leaflet application ##############################

PM25_sat <- readOGR(".ENGLAND_geojson_PM25_1km_Sat_2009_2011", "OGRGeoJSON")

map_PM25_sat <- leaflet(PM25_sat)

#### colors for maps
qpal_SAT <- colorQuantile("Reds", PM25_sat$pm25_mean, n = 50)
qpal_UK_AIR <- colorQuantile("Reds", PM25_sat$pm25_mean_UK_AIR, n = 50)
qpal_GWR <- colorQuantile("Reds", PM25_sat$pm25_mean_GWR, n = 7)
qpal_pcm <- colorQuantile("Reds", PM25_sat$pm25_mean_pcm, n = 7)
qpal_cmaq <- colorQuantile("Reds", PM25_sat$pm25_mean_cmaq, n = 7)


#### colors for legend (continuous)

pal_SAT <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean)

pal_UK_AIR <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_UK_AIR)

pal_GWR <- colorNumeric(
   palette = "Reds",
   domain = PM25_sat$pm25_mean_GWR)
 
pal_pcm <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_pcm)

pal_cmaq <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_cmaq)

### colors for raster URB land cover (England region)
pal_URB <- colorNumeric(c("#FFFFCC", "#41B6C4","#0C2C84"), getValues(URB_Cover_nc),
                        na.color = "transparent")

### colors for raster GWR_10km
pal_GWR_rast_10km <- colorNumeric(c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000"),
                    getValues(GWR_PM25_10km_nc),na.color = "transparent")

### colors for raster GWR_1km
pal_GWR_rast_1km <- colorNumeric(c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000"),
                             getValues(GWR_PM25_1km_nc),na.color = "transparent")
 
# "#0000FF" "#E5E5FF","#E5E5FF" "#9999FF" "#FFFF00" "#7f7fff"

### colors for raster PM25 UK-AIR
pal_PM25_UK_AIR <- colorNumeric(c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000"),
                             getValues(PM25_UK_AIR_nc),na.color = "transparent")

### colors for raster PM25_sat
pal_PM25_SAT <- colorNumeric(c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000"),
                             getValues(PM25_EN_SAT_nc),na.color = "transparent")

### colors for raster pcm_PM25
pal_pcm_PM25_rast <- colorNumeric(c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000"),
                                  getValues(pcm_PM25_nc),na.color = "transparent")

### colors for raster cmaq_PM25 10km
pal_cmaq_PM25_rast_10km <- colorNumeric(c("#0000FF", "#FFFF00","#FF0000"),
                                   getValues(cmaq_PM25_10km_nc),na.color = "transparent")

### colors for raster cmaq_PM25 1km
pal_cmaq_PM25_rast_1km <- colorNumeric(c("#0000FF", "#FFFF00","#FF0000"),
                                   getValues(cmaq_PM25_1km_nc),na.color = "transparent")


# photoIcon <- makeIcon(
#   iconAnchorX = 12, iconAnchorY = 12,   
#   iconUrl = "https://www.mapbox.com/maki/renders/camera-12@2x.png"
# )

### popup for legend for polygons

popup_html_PM25_sat <- "<div>
<img width=65%, height=65% src='PHOTOPATH' />" ### change size of thelegend
popup_html_PM25_UK_AIR <- "<div>
<img width=70%, height=70% src='PHOTOPATH' />" ### change size of thelegend
popup_html_PM25_GWR <- "<div>
<img width=75%, height=75% src='PHOTOPATH' />" ### change size of thelegend
popup_html_pcm <- "<div>
<img width=75%, height=75% src='PHOTOPATH' />" ### change size of thelegend
popup_html_cmaq <- "<div>
<img width=75%, height=75% src='PHOTOPATH' />" ### change size of thelegend


PM25_sat_url <- "https://www.dropbox.com/s/2dqrk0ygqn6clko/PM25_sat_Legend_EN.png?raw=1"
PM25_UK_AIR_url <- "https://www.dropbox.com/s/nqeggyuof2eba3a/PM25_UK_AIR_Legend_EN.png?raw=1"
PM25_GWR_url <- "https://www.dropbox.com/s/mmk25ml0kwyqw0r/PM25_GWR_Legend_EN.png?raw=1"
pcm_url <- "https://www.dropbox.com/s/iumde5tx41v5cjv/PM25_pcm_Legend_EN.png?raw=1"
cmaq_url <- "https://www.dropbox.com/s/8fuz5k57a76xfxx/PM25_cmaq_Legend_EN.png?raw=1"

# PM25_sat_url  <- 'https://www.mapbox.com/maki/renders/camera-12@2x.png'

popups_html_PM25_sat <- sapply(PM25_sat_url, function(x) { sub('PHOTOPATH', x, popup_html_PM25_sat)})
popups_html_PM25_UK_AIR <- sapply(PM25_UK_AIR_url, function(x) { sub('PHOTOPATH', x, popup_html_PM25_UK_AIR)})
popups_html_PM25_GWR_url <- sapply(PM25_GWR_url, function(x) { sub('PHOTOPATH', x, popup_html_PM25_GWR)})
popups_html_pcm_url <- sapply(pcm_url, function(x) { sub('PHOTOPATH', x, popup_html_pcm)})
popups_html_cmaq_url <- sapply(cmaq_url, function(x) { sub('PHOTOPATH', x, popup_html_cmaq)})

popup_PM25_sat <- paste0(as.vector(popups_html_PM25_sat),
                         "<p><strong>Local Authority: </strong>", 
                         PM25_sat$id, 
                         "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                         PM25_sat$pm25_mean)

popup_PM25_UK_AIR <- paste0(as.vector(popups_html_PM25_UK_AIR),
                            "<p><strong>Local Authority: </strong>", 
                            PM25_sat$id, 
                            "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                            PM25_sat$pm25_mean_UK_AIR)

popup_GWR <- paste0(as.vector(popups_html_PM25_GWR_url),
                         "<p><strong>Local Authority: </strong>", 
                         PM25_sat$id, 
                         "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                         PM25_sat$pm25_mean_GWR)

popup_pcm <- paste0(as.vector(popups_html_pcm_url),
                    "<p><strong>Local Authority: </strong>", 
                    PM25_sat$id, 
                    "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                    PM25_sat$pm25_mean_pcm)

popup_cmaq <- paste0(as.vector(popups_html_cmaq_url),
                     "<p><strong>Local Authority: </strong>", 
                     PM25_sat$id, 
                     "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                     PM25_sat$pm25_mean_cmaq)


####### CREATE map #######################################################

map = map_PM25_sat %>% 
  setView(lng = 1.25, lat = 52.40, zoom = 7) %>%
  
  # Base maps
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  
  # PM2.5 satellite data 
  addPolygons(stroke = TRUE, smoothFactor = 0.2, 
              fillOpacity = 0.5, 
              color = ~ qpal_SAT(pm25_mean),
              weight = 2,
              popup = popup_PM25_sat,
              group = "PM2.5 Satellite") %>%
#          addLegend("bottomright", pal = pal_SAT, values = ~pm25_mean,
#              title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) Sat : </strong>",
#              labFormat = labelFormat(prefix = ""),
#             opacity = 1) %>%
  
  
  # UK AIR interpolated data
  addPolygons(stroke = TRUE, smoothFactor = 0.2, 
              fillOpacity = 0.5, 
              color = ~ qpal_UK_AIR(pm25_mean_UK_AIR),
              weight = 2,
              popup = popup_PM25_UK_AIR,
              group = "PM2.5 UK AIR") %>%
#          addLegend("bottomright", pal = pal_UK_AIR, values = ~pm25_mean_UK_AIR,
#                    title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) UK-AIR : </strong>",
#                 labFormat = labelFormat(prefix = ""),
#                 opacity = 1) %>%
  
# PM2.5 GWR
  addPolygons(stroke = TRUE, smoothFactor = 0.2, 
             fillOpacity = 0.5, 
            color = ~ qpal_GWR(pm25_mean_GWR),
            weight = 2,
              popup = popup_GWR,
              group = "GWR PM2.5") %>%
#             addLegend("bottomright", pal = pal_GWR, values = ~pm25_mean_GWR,
#             title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) GWR: </strong>",
#             labFormat = labelFormat(prefix = ""),
#             opacity = 1) %>%  

  
  # PM2.5 pcm
  addPolygons(stroke = TRUE, smoothFactor = 0.2, 
              fillOpacity = 0.5, 
              color = ~ qpal_pcm(pm25_mean_pcm),
              weight = 2,
              popup = popup_pcm,
              group = "PMC PM2.5 model") %>%
#      addLegend("bottomright", pal = pal_pcm, values = ~pm25_mean_pcm,
#                title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) PCM model : </strong>",
#                labFormat = labelFormat(prefix = ""),
#                opacity = 1) %>%
   
  # PM2.5 cmaq
  addPolygons(stroke = TRUE, smoothFactor = 0.2, 
              fillOpacity = 0.5, 
              color = ~ qpal_cmaq(pm25_mean_cmaq),
              weight = 2,
              popup = popup_cmaq,
              group = "CMAQ PM2.5 model") %>%
#      addLegend("bottomright", pal = pal_cmaq, values = ~pm25_mean_cmaq,
#                title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) CMAQ model : </strong>",
#                labFormat = labelFormat(prefix = ""),
#                opacity = 1) %>%
  
  # URBAN land cover raster
  addRasterImage(URB_Cover_nc, colors = pal_URB, opacity = 0.6,
                 group = "URBAN fraction") %>%
  # PM25_SAT raster
  addRasterImage(PM25_EN_SAT_nc, colors = pal_PM25_SAT, opacity = 0.6,
                 group = "PM2.5 Sat. rast.") %>%
  # PM25_UK_AIR raster
  addRasterImage(PM25_UK_AIR_nc, colors = pal_PM25_UK_AIR, opacity = 0.6,
                 group = "PM2.5 UK-AIR rast.") %>%
  # GWR_10km raster
  addRasterImage(GWR_PM25_10km_nc, colors = pal_GWR_rast_10km, opacity = 0.6,
                 group = "GWR PM2.5. rast. 10km") %>%
  # GWR_1km raster
  addRasterImage(GWR_PM25_1km_nc, colors = pal_GWR_rast_1km, opacity = 0.6,
                 group = "GWR PM2.5. rast. 1km") %>%
  # pcm_PM25_raster
  addRasterImage(pcm_PM25_nc, colors = pal_pcm_PM25_rast, opacity = 0.6,
                 group = "PCM model PM2.5 rast.") %>%
  # cmaq_PM25_raster 10km
  addRasterImage(cmaq_PM25_10km_nc, colors = pal_cmaq_PM25_rast_10km, opacity = 0.5,
                 group = "CMAQ model PM2.5 rast. 10km") %>%
  # cmaq_PM25_raster 1km
  addRasterImage(cmaq_PM25_1km_nc, colors = pal_cmaq_PM25_rast_1km, opacity = 0.6,
                 group = "CMAQ model PM2.5 rast. 1km") %>%
  
  #addMarkers(lng=-0.973, lat=52.030,  
  #               icon = photoIcon, # function providing custom marker-icons
  #              group='Photo markers',
  #             popup = as.vector(popups_html)) %>%
  
  # Layers control
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("PM2.5 Satellite", "PM2.5 UK AIR", "GWR PM2.5",
                      "PMC PM2.5 model", "URBAN fraction", "CMAQ PM2.5 model",
                      "GWR PM2.5. rast. 10km","GWR PM2.5. rast. 1km",
                      "PM2.5 Sat. rast.", "PM2.5 UK-AIR rast.",
                      "PCM model PM2.5 rast.", "CMAQ model PM2.5 rast. 10km",
                      "CMAQ model PM2.5 rast. 1km"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("PM2.5 UK AIR") %>%
  hideGroup("GWR PM2.5") %>%
  hideGroup("PMC PM2.5 model") %>%
  hideGroup("CMAQ PM2.5 model") %>%
  hideGroup("URBAN fraction") %>%
  hideGroup("GWR PM2.5. rast. 10km") %>%
  hideGroup("GWR PM2.5. rast. 1km") %>%
  hideGroup("PM2.5 Sat. rast.") %>%
  hideGroup("PM2.5 UK-AIR rast.") %>%
  hideGroup("PCM model PM2.5 rast.") %>%
  hideGroup("CMAQ model PM2.5 rast. 10km") %>%
  hideGroup("CMAQ model PM2.5 rast. 1km") 

map


#### to export into html file, use the button in the Wiever window: 
#### "save as Web Page"...PM25_Sat_new

 saveWidget(map,
            file="England_PM25_Sat_new.html",
            selfcontained = FALSE)

