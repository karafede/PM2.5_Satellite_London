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
# library(leafletR)
library(leaflet)
library(sp)
library(rbison)
library(devtools)
library(rgbif)
library(RColorBrewer)
library(spatialEco)
library(htmltools)
library(ncdf4)
library(htmlwidgets)
# library(rjson)


###################### Merge data into Local Authorities shapefiles ############
################################################################################


dir <- "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Local Authorities_England"
### shapefile for local authorities in England
shp <- readOGR(dsn = dir, layer = "UK_lad")
#shp <- fromJSON(file = "C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Local Authorities_England/UK_LAD.json")


# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
names(shp)


head(shp@data)  # polygons
shp@data$name <- 1:nrow(shp)

PM25_SAT <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_1km_2009-11_interp.csv", header = TRUE)
PM25_UK_AIR <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_AIR_AVG_2010_2012_interp.csv", header = TRUE)
URB_Cover <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Land_Cover_London_interp.csv", header=TRUE)
ED_London <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/ED_London.csv", header = TRUE)
PM25_pcm <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/pcm_data/PM25_pcm_2009_11_London_interp.csv", header = TRUE)
GWR_PM25_URB <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/GWR_AOE_London_URB_new.csv", header = TRUE)
GWR_PM25 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/GWR_AOE_London.csv", header = TRUE)
pcm_PM25 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/pcm_data/PM25_pcm_2009_11_London_interp.csv", header = TRUE)
cmaq_PM25 <- read.csv("C:/SATELLITE_STUFF/Donkelaar_1Km/cmaq/UK10_2009_2011/PM25_2009_2011_London_interp.csv",header = TRUE)


PM25_London_Sat <- cbind(PM25_SAT[,1:3], PM25_UK_AIR[,3], GWR_PM25_URB[,15],
                         GWR_PM25[,17],pcm_PM25[,3], cmaq_PM25[,3])
colnames(PM25_London_Sat) <- c("Lon", "Lat", "PM25_1km", "PM25_UK_AIR",
                               "GWR_PM25_URB", "GWR_PM25", "pcm_PM25", "cmaq_PM25")

#### create a raster for PM25_Sat 1km in London #######

coordinates(PM25_SAT) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(PM25_SAT) <- TRUE
raster_PM25_SAT <- raster(PM25_SAT)
projection(raster_PM25_SAT) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_PM25_SAT)

PM25_SAT_nc <- writeRaster(raster_PM25_SAT,
                            filename="C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/PM25_SAT.nc",
                            format="CDF", overwrite=TRUE) 
PM25_SAT_nc <- raster("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/PM25_SAT.nc")




#### create a raster for URB land cover in London #######

coordinates(URB_Cover) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(URB_Cover) <- TRUE
raster_URB_Cover <- raster(URB_Cover)
projection(raster_URB_Cover) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_URB_Cover)

URB_Cover_nc <- writeRaster(raster_URB_Cover,
                            filename="C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/URB_Cover_London.nc",
                            format="CDF", overwrite=TRUE) 
URB_Cover_nc <- raster("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/URB_Cover_London.nc")


#### create a raster for GWR URBan only (Geographycally weighted regression) #######

GWR_PM25_URB <- cbind(GWR_PM25_URB$coord.x, GWR_PM25_URB$coord.y, GWR_PM25_URB$AOE)
colnames(GWR_PM25_URB) <- c("Lon", "Lat", "AOE")
GWR_PM25_URB <- as.data.frame(GWR_PM25_URB)
coordinates(GWR_PM25_URB) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(GWR_PM25_URB) <- TRUE
raster_GWR_PM25_URB <- raster(GWR_PM25_URB)
projection(raster_GWR_PM25_URB) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_GWR_PM25_URB)

GWR_PM25_URB_nc <- writeRaster(raster_GWR_PM25_URB,
                               filename="C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/GWR_PM25_URB.nc",
                               format="CDF", overwrite=TRUE) 
GWR_PM25_URB_nc <- raster("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/GWR_PM25_URB.nc")



#### create a raster for GWR URB + SO4 + NO3 + PC + SOA #######
GWR_PM25$AOE <- ifelse(GWR_PM25$AOE < 0, 0, GWR_PM25$AOE)
min(GWR_PM25$AOE)
GWR_PM25 <- cbind(GWR_PM25$coord.x, GWR_PM25$coord.y, GWR_PM25$AOE)
colnames(GWR_PM25) <- c("Lon", "Lat", "AOE")
GWR_PM25 <- as.data.frame(GWR_PM25)
coordinates(GWR_PM25) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(GWR_PM25) <- TRUE
raster_GWR_PM25 <- raster(GWR_PM25)
projection(raster_GWR_PM25) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_GWR_PM25)

GWR_PM25_nc <- writeRaster(raster_GWR_PM25,
                           filename="C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/GWR_PM25.nc",
                           format="CDF", overwrite=TRUE) 
GWR_PM25_nc <- raster("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/GWR_PM25.nc")


#### create a raster for PM2.5 pcm #####################################

coordinates(pcm_PM25) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(pcm_PM25) <- TRUE
raster_pcm_PM25 <- raster(pcm_PM25)
projection(raster_pcm_PM25) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_pcm_PM25)

pcm_PM25_nc <- writeRaster(raster_pcm_PM25,
                           filename="C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/pcm_PM25_raster.nc",
                           format="CDF", overwrite=TRUE) 
pcm_PM25_nc <- raster("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/pcm_PM25_raster.nc")



#### create a raster for PM2.5 CMAQ #####################################

coordinates(cmaq_PM25) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(cmaq_PM25) <- TRUE
raster_cmaq_PM25 <- raster(cmaq_PM25)
projection(raster_cmaq_PM25) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_cmaq_PM25)

cmaq_PM25_nc <- writeRaster(raster_cmaq_PM25,
                           filename="C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/cmaq_PM25_raster.nc",
                           format="CDF", overwrite=TRUE) 
cmaq_PM25_nc <- raster("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/cmaq_PM25_raster.nc")


###################################################################################


#### define projection for PM25_London_Sat dataframe

crs <- projection(shp) ### get projections from shp file
# coordinates(PM25_London_Sat) = ~ Lon+Lat
PM25_London_Sat <- SpatialPointsDataFrame(PM25_London_Sat[,1:2], PM25_London_Sat, 
                                   proj4string=CRS(crs)) 

summary(PM25_London_Sat) 

plot(shp)
points(PM25_London_Sat, pch=10)

 #pts.poly <- over(PM25_London_Sat, shp[,"id"])
pts.poly <- point.in.poly(PM25_London_Sat, shp) ### requre Library (SpatialEco)
# PM25_London_Sat$id <- pts.poly$id


# pts.poly <- point.in.poly(PM25_London_Sat, shp)   #### assign PM25 sat data to Local Authorities polygons
# writeOGR(pts.poly,"C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Local Authorities_England",
#          "LA_Join_PM25_London_Sat_2009_2011", driver = "ESRI Shapefile")

head(pts.poly@data)

# Aggregate by zone/polygon
data_points <- pts.poly@data 
names(data_points)

data_points <- data_points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(pm25_mean = mean(PM25_1km),
                   pm25_mean_UK_AIR = mean(PM25_UK_AIR),
                   pm25_mean_GWR_URB = mean(GWR_PM25_URB),
                   pm25_mean_GWR = mean(GWR_PM25),
                   pm25_mean_pcm = mean(pcm_PM25),
                   pm25_mean_cmaq = mean(cmaq_PM25)) %>% 
  dplyr::ungroup()

###  Make a dataframe ###
# data_points <- PM25_London_Sat@data

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


data_points <- shp@data[,c("id","name","pm25_mean","pm25_mean_UK_AIR",
                           "pm25_mean_GWR_URB", "pm25_mean_GWR",
                           "pm25_mean_pcm", "pm25_mean_cmaq")]
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

PM25_sat <- readOGR("C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/.geojson_PM25_1km_Sat_2009_2011", "OGRGeoJSON")

map_PM25_sat <- leaflet(PM25_sat)

#### colors for maps
qpal_SAT <- colorQuantile("Reds", PM25_sat$pm25_mean, n = 7)
qpal_UK_AIR <- colorQuantile("Reds", PM25_sat$pm25_mean_UK_AIR, n = 7)
qpal_GWR_URB <- colorQuantile("Reds", PM25_sat$pm25_mean_GWR_URB, n = 7)
qpal_GWR_URB_Ions <- colorQuantile("Reds", PM25_sat$pm25_mean_GWR, n = 7)
qpal_pcm <- colorQuantile("Reds", PM25_sat$pm25_mean_pcm, n = 7)
qpal_cmaq <- colorQuantile("Reds", PM25_sat$pm25_mean_cmaq, n = 7)

#### colors for legend (continuous)
pal_SAT <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean)

pal_UK_AIR <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_UK_AIR)

pal_GWR_URB <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_GWR_URB)

pal_GWR_URB_Ions <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_GWR)

pal_pcm <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_pcm)

pal_cmaq <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_cmaq)

### colors for raster URB land cover (London)
pal_URB <- colorNumeric(c("#FFFFCC", "#41B6C4","#0C2C84"), getValues(URB_Cover_nc),
                        na.color = "transparent")
### colors for raster GWR_URB
 pal_GWR_URB_rast <- colorNumeric(c("#9999FF", "#FFFF00","#FF0000"),
                                  getValues(GWR_PM25_URB_nc),na.color = "transparent")

### colors for raster GWR_URB ions
pal_GWR_ions_rast <- colorNumeric(c("#0000FF", "#0000FF", "#7f7fff","#FFFF00", "#FF0000", "#b30000"),
                                  getValues(GWR_PM25_nc),na.color = "transparent")

# "#0000FF" "#E5E5FF","#E5E5FF" "#9999FF" "#FFFF00"

### colors for raster PM25_sat
pal_PM25_SAT <- colorNumeric(c("#9999FF", "#FFFF00","#FF0000"),
                             getValues(PM25_SAT_nc),na.color = "transparent")

### colors for raster pcm_PM25
pal_pcm_PM25_rast <- colorNumeric(c("#9999FF", "#FFFF00","#FF0000"),
                                  getValues(pcm_PM25_nc),na.color = "transparent")
### colors for raster cmaq_PM25
pal_cmaq_PM25_rast <- colorNumeric(c("#9999FF", "#FFFF00","#FF0000"),
                                   getValues(cmaq_PM25_nc),na.color = "transparent")
# photoIcon <- makeIcon(
#   iconAnchorX = 12, iconAnchorY = 12,   
#   iconUrl = "https://www.mapbox.com/maki/renders/camera-12@2x.png"
# )

### popup for legend for polygons

popup_html_PM25_sat <- "<div>
                <img width=65%, height=65% src='PHOTOPATH' />" ### change size of thelegend
popup_html_PM25_UK_AIR <- "<div>
                <img width=70%, height=70% src='PHOTOPATH' />" ### change size of thelegend
popup_html_PM25_GWR_URB <- "<div>
                <img width=70%, height=70% src='PHOTOPATH' />" ### change size of thelegend
popup_html_PM25_GWR_URB_Ions <- "<div>
                <img width=75%, height=75% src='PHOTOPATH' />" ### change size of thelegend
popup_html_pcm <- "<div>
                <img width=75%, height=75% src='PHOTOPATH' />" ### change size of thelegend
popup_html_cmaq <- "<div>
                <img width=75%, height=75% src='PHOTOPATH' />" ### change size of thelegend


PM25_sat_url <- "https://www.dropbox.com/s/98fnr4ug4nmtpwu/PM25_Sat_Legend.png?raw=1"
PM25_UK_AIR_url <- "https://www.dropbox.com/s/cvk227usaqxa70p/PM25_UK_AIR_Legend.png?raw=1"
PM25_GWR_URB_url <- "https://www.dropbox.com/s/iaklfuocgq724th/GWR_URB_Legend.png?raw=1"
PM25_GWR_URB_Ions_url <- "https://www.dropbox.com/s/vplc7wze7wrhydp/GWR_URB_Ions_Legend.png?raw=1"
pcm_url <- "https://www.dropbox.com/s/3epuov8mywc9yye/pcm_PM25_Legend.png?raw=1"
cmaq_url <- "https://www.dropbox.com/s/b4i1ikna3nksrb5/cmaq_PM25_Legend.png?raw=1"

# PM25_sat_url  <- 'https://www.mapbox.com/maki/renders/camera-12@2x.png'

popups_html_PM25_sat <- sapply(PM25_sat_url, function(x) { sub('PHOTOPATH', x, popup_html_PM25_sat)})
popups_html_PM25_UK_AIR <- sapply(PM25_UK_AIR_url, function(x) { sub('PHOTOPATH', x, popup_html_PM25_UK_AIR)})
popups_html_PM25_GWR_URB_url <- sapply(PM25_GWR_URB_url, function(x) { sub('PHOTOPATH', x, popup_html_PM25_GWR_URB)})
popups_html_PM25_GWR_URB_Ions_url <- sapply(PM25_GWR_URB_Ions_url, function(x) { sub('PHOTOPATH', x, popup_html_PM25_GWR_URB_Ions)})
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

popup_GWR_URB <- paste0(as.vector(popups_html_PM25_GWR_URB_url),
                            "<p><strong>Local Authority: </strong>", 
                            PM25_sat$id, 
                            "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                            PM25_sat$pm25_mean_GWR_URB)

popup_GWR <- paste0(as.vector(popups_html_PM25_GWR_URB_Ions_url),
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
#       addLegend("bottomright", pal = pal_SAT, values = ~pm25_mean,
#           title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) Sat : </strong>",
#           labFormat = labelFormat(prefix = ""),
#          opacity = 1) %>%
 
  
  # UK AIR interpolated data
     addPolygons(stroke = TRUE, smoothFactor = 0.2, 
            fillOpacity = 0.5, 
            color = ~ qpal_UK_AIR(pm25_mean_UK_AIR),
            weight = 2,
            popup = popup_PM25_UK_AIR,
            group = "PM2.5 UK AIR") %>%
#       addLegend("bottomright", pal = pal_UK_AIR, values = ~pm25_mean_UK_AIR,
#                 title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) UK-AIR : </strong>",
#              labFormat = labelFormat(prefix = ""),
#              opacity = 1) %>%

  # PM2.5 GWR URB 
  addPolygons(stroke = TRUE, smoothFactor = 0.2, 
              fillOpacity = 0.5, 
              color = ~ qpal_GWR_URB(pm25_mean_GWR_URB),
              weight = 2,
              popup = popup_GWR_URB,
              group = "GWR PM2.5 Urban") %>%
#          addLegend("bottomright", pal = pal_GWR_URB, values = ~pm25_mean_GWR_URB,
#              title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) GWR URB : </strong>",
#              labFormat = labelFormat(prefix = ""),
#             opacity = 1) %>%  
  
  # PM2.5 GWR URB Ions
  addPolygons(stroke = TRUE, smoothFactor = 0.2, 
              fillOpacity = 0.5, 
              color = ~ qpal_GWR_URB_Ions(pm25_mean_GWR),
              weight = 2,
              popup = popup_GWR,
              group = "GWR PM2.5 Urban+Ions") %>%
#             addLegend("bottomright", pal = pal_GWR_URB_Ions, values = ~pm25_mean_GWR,
#                 title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) GWR URB+Ions : </strong>",
#                 labFormat = labelFormat(prefix = ""),
#                opacity = 1) %>% 
  
  # PM2.5 pcm
  addPolygons(stroke = TRUE, smoothFactor = 0.2, 
              fillOpacity = 0.5, 
              color = ~ qpal_pcm(pm25_mean_pcm),
              weight = 2,
              popup = popup_pcm,
              group = "PMC PM2.5 model") %>%
#   addLegend("bottomright", pal = pal_pcm, values = ~pm25_mean_pcm,
#             title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) PCM model : </strong>",
#             labFormat = labelFormat(prefix = ""),
#             opacity = 1) %>%
  
  # PM2.5 cmaq
  addPolygons(stroke = TRUE, smoothFactor = 0.2, 
              fillOpacity = 0.5, 
              color = ~ qpal_cmaq(pm25_mean_cmaq),
              weight = 2,
              popup = popup_cmaq,
              group = "CMAQ PM2.5 model") %>%
#   addLegend("bottomright", pal = pal_cmaq, values = ~pm25_mean_cmaq,
#             title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) CMAQ model : </strong>",
#             labFormat = labelFormat(prefix = ""),
#             opacity = 1) %>%
  
  # URBAN land cover raster
  addRasterImage(URB_Cover_nc, colors = pal_URB, opacity = 0.6,
                 group = "URBAN fraction") %>%
  # PM25_SAT raster
  addRasterImage(PM25_SAT_nc, colors = pal_PM25_SAT, opacity = 0.7,
                 group = "PM2.5 Sat. rast.") %>%
  # GWR_URB raster
  addRasterImage(GWR_PM25_URB_nc, colors = pal_GWR_URB_rast, opacity = 0.7,
                 group = "Geo.Regr.Urban rast.") %>%
  # GWR_URB_ions raster
  addRasterImage(GWR_PM25_nc, colors = pal_GWR_ions_rast, opacity = 0.7,
                 group = "Geo.Regr.Urban+Ions rast.") %>%
  # pcm_PM25_raster
  addRasterImage(pcm_PM25_nc, colors = pal_pcm_PM25_rast, opacity = 0.7,
                 group = "PCM model PM2.5 rast.") %>%
  # cmaq_PM25_raster
  addRasterImage(cmaq_PM25_nc, colors = pal_cmaq_PM25_rast, opacity = 0.7,
                 group = "CMAQ model PM2.5 rast.") %>%
  
  #addMarkers(lng=-0.973, lat=52.030,  
   #               icon = photoIcon, # function providing custom marker-icons
    #              group='Photo markers',
     #             popup = as.vector(popups_html)) %>%
 
  # Layers control
addLayersControl(
  baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
  overlayGroups = c("PM2.5 Satellite", "PM2.5 UK AIR", "GWR PM2.5 Urban",
                    "GWR PM2.5 Urban+Ions", "PMC PM2.5 model",
                    "URBAN fraction","Geo.Regr.Urban rast.", "CMAQ PM2.5 model",
                    "Geo.Regr.Urban+Ions rast.", "PM2.5 Sat. rast.",
                    "PCM model PM2.5 rast.", "CMAQ model PM2.5 rast."),
                    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("PM2.5 UK AIR") %>%
  hideGroup("GWR PM2.5 Urban") %>%
  hideGroup("GWR PM2.5 Urban+Ions") %>%
  hideGroup("PMC PM2.5 model") %>%
  hideGroup("CMAQ PM2.5 model") %>%
  hideGroup("URBAN fraction") %>%
  hideGroup("Geo.Regr.Urban rast.") %>%
  hideGroup("PM2.5 Sat. rast.") %>%
  hideGroup("PCM model PM2.5 rast.") %>%
  hideGroup("CMAQ model PM2.5 rast.") %>%
  hideGroup("Geo.Regr.Urban+Ions rast.")

map

#### to export into html file, use the button in the Wiever window: 
#### "save as Web Page"...PM25_Sat_new

 saveWidget(map,
           file="C:/SATELLITE_STUFF/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/GWR_London.html",
           selfcontained = FALSE)

