
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
 library(mapview)


###################### Merge data into Local Authorities shapefiles ############
################################################################################


dir <- "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Local Authorities_England"
### shapefile for local authorities in England
shp <- readOGR(dsn = dir, layer = "UK_lad")
#shp <- fromJSON(file = "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Local Authorities_England/UK_LAD.json")


# ----- Transform to EPSG 4326 - WGS84 (required)
shp <- spTransform(shp, CRS("+init=epsg:4326"))
names(shp)


head(shp@data)  # polygons
shp@data$name <- 1:nrow(shp)

PM25_SAT <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_1km_2009-11_interp.csv", header = TRUE)
PM25_UK_AIR <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_AIR_AVG_2010_2012_interp.csv", header = TRUE)
URB_Cover <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Land_Cover_London_interp.csv", header=TRUE)
ED_London <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/ED_London.csv", header = TRUE)
PM25_pcm <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/pcm_data/PM25_pcm_2009_11_London_interp.csv", header = TRUE)
GWR_PM25_URB <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/GWR_AOE_London_URB_new.csv", header = TRUE)
GWR_PM25 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/GWR_AOE_London.csv", header = TRUE)
pcm_PM25 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/pcm_data/PM25_pcm_2009_11_London_interp.csv", header = TRUE)
cmaq_PM25 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/PM25_2009_2011_London_interp.csv",header = TRUE)


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
                            filename="C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/PM25_SAT.nc",
                            format="CDF", overwrite=TRUE) 
PM25_SAT_nc <- raster("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/PM25_SAT.nc")




#### create a raster for URB land cover in London #######

coordinates(URB_Cover) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(URB_Cover) <- TRUE
raster_URB_Cover <- raster(URB_Cover)
projection(raster_URB_Cover) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_URB_Cover)

URB_Cover_nc <- writeRaster(raster_URB_Cover,
                            filename="C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/URB_Cover_London.nc",
                            format="CDF", overwrite=TRUE) 
URB_Cover_nc <- raster("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/URB_Cover_London.nc")


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
                               filename="C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/GWR_PM25_URB.nc",
                               format="CDF", overwrite=TRUE) 
GWR_PM25_URB_nc <- raster("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/GWR_PM25_URB.nc")



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
                           filename="C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/GWR_PM25.nc",
                           format="CDF", overwrite=TRUE) 
GWR_PM25_nc <- raster("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/GWR_PM25.nc")


#### create a raster for PM2.5 pcm #####################################

coordinates(pcm_PM25) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(pcm_PM25) <- TRUE
raster_pcm_PM25 <- raster(pcm_PM25)
projection(raster_pcm_PM25) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_pcm_PM25)

pcm_PM25_nc <- writeRaster(raster_pcm_PM25,
                           filename="C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/pcm_PM25_raster.nc",
                           format="CDF", overwrite=TRUE) 
pcm_PM25_nc <- raster("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/pcm_PM25_raster.nc")



#### create a raster for PM2.5 CMAQ #####################################

coordinates(cmaq_PM25) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(cmaq_PM25) <- TRUE
raster_cmaq_PM25 <- raster(cmaq_PM25)
projection(raster_cmaq_PM25) <- CRS("+proj=longlat +datum=WGS84")
plot(raster_cmaq_PM25)

cmaq_PM25_nc <- writeRaster(raster_cmaq_PM25,
                           filename="C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/cmaq_PM25_raster.nc",
                           format="CDF", overwrite=TRUE) 
cmaq_PM25_nc <- raster("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/cmaq_PM25_raster.nc")


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
# writeOGR(pts.poly,"C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Local Authorities_England",
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
writeOGR(shp,"C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/UK_border/Local Authorities_England",
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
dir <- "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet"
leafdat<-paste(dir, "/",  ".geojson_PM25_1km_Sat_2009_2011", sep="") 
leafdat

####  ATT !!!!! erase existing .geojson file when re-runing code ######
writeOGR(shp, leafdat, layer="", driver="GeoJSON")  ## erase existing .geojson file when re-runing code 

PM25_sat <- readOGR("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/.geojson_PM25_1km_Sat_2009_2011", "OGRGeoJSON")

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
pal_GWR_ions_rast <- colorNumeric(c("#9999FF", "#9999FF", "#9999FF","#FFFF00", "#FF0000", "#b30000"),
                                  getValues(GWR_PM25_nc),na.color = "transparent")

# "#0000FF" "#E5E5FF","#E5E5FF" "#9999FF" "#FFFF00" "#7f7fff"

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
           file="C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Leaflet/prova_PM25_sat_new_2.html",
           selfcontained = FALSE)

 
 ###################################################################################
 ###################################################################################
 
 mapview(PM25_SAT_nc) +
   viewExtent(PM25_SAT_nc, 
              opacity = 0, fillOpacity = 0)
 
 
 
 
 
 
 

####################################################################################
####################################################################################
######## Load Satellite data from Donkelaar et al. #################################

setwd <- "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe"

PM25_London_2009_2011 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_10km_London_2009-2011.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_1km_2009-11_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_1km))


############   UK AIR #############################################################

PM25_London_UK_AIR_2010_2012 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_UK_AIR_London.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_AIR_AVG_2010_2012_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_UK_AIR_AVG_1km))



############ PCM data #############################################################

PM25_pcm_London_2009_2011 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/pcm_data/PCM_2009-11_AVG_London.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/pcm_data/PM25_pcm_2009_11_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_pcm_1km))



############   UK ALTITUDE #############################################################

Altitude_London <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Altitude_London.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Altitude_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = Altitude))


############  Land Cover UK ######################################################

Land_Cover_London <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Land_Cover_London.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Land_Cover_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = Land_URB))



#####################################################################################
################# NO3 from CMAQ model ###############################################

NO3_2009_2011 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/NO3_2009_2011_London.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/NO3_2009_2011_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = NO3_AVG))



################# SO4 from CMAQ model ###############################################

SO4_2009_2011 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/SO4_2009_2011_London.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/SO4_2009_2011_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = SO4_AVG))




################# POC from CMAQ model ###############################################

POC_2009_2011 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/POC_2009_2011_London.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/POC_2009_2011_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = POC_AVG))



################# SOAA from CMAQ model ###############################################

SOAA_2009_2011 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/SOAA_2009_2011_London.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/SOAA_2009_2011_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = SOAA_AVG))


################# PM25 from CMAQ model ###############################################

PM25_2009_2011 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/PM25_2009_2011_London.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/PM25_2009_2011_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_AVG))




#####################################################################################
################# NIT from EMEP model ###############################################

EMEP_modelled <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/EMEP_data.csv",
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
write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/SO4_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = SO4_London))

##### Nitrate

idw <- idw(formula = NO3_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "NO3_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/NO3_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = NO3_London))

##### Primary Carbonaceous (PC)

idw <- idw(formula = EC_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "EC_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/EC_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = EC_London))

##### SOA (Secondary Organic Aerosols)

idw <- idw(formula = ASOA_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "ASOA_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/ASOA_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = ASOA_London))

idw <- idw(formula = BSOA_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "BSOA_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/BSOA_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = BSOA_London))

ASOA <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/ASOA_London_2013_interp.csv",
                  header = TRUE) 
BSOA <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/BSOA_London_2013_interp.csv",
                 header = TRUE) 
SOA <- cbind(ASOA$Lon, ASOA$Lon, ((ASOA$ASOA_London)+(BSOA$BSOA_London)))
colnames(SOA) <- c("Lon", "Lat", "SOA") 
write.csv(SOA, file = "C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/SOA_London_2013_interp.csv", row.names=FALSE)


##### PM2.5 from EMEP modeled

idw <- idw(formula = PM25_LONDON ~ 1, locations = EMEP_modelled, 
           newdata = grd)  # apply idw model for the data

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "PM25_London")  # give names to the modelled variables
write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/PM25_EMEP_London_2013_interp.csv", row.names=FALSE)
ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = PM25_London))



############################ kriging ################################################

#variogcloud<-variogram(LANDCOVERU ~ 1, locations = Land_Cover_UK_URB_test, data = Land_Cover_UK_URB_test, cloud=TRUE)
#plot(variogcloud)

# semivariog<-variogram(LANDCOVERU ~ 1, locations = Land_Cover_UK_URB_test, data = Land_Cover_UK_URB_test)
# plot(semivariog)

#semivariog
#model.variog<-vgm(psill=220, model="Exp", nugget=200, range=21500)
#fit.variog<-fit.variogram(semivariog, model.variog)
#plot(semivariog, fit.variog)

#model.variog<-vgm(psill=220, model="Sph", nugget=200, range=21500)
# fit.variog<-fit.variogram(semivariog, model.variog)
# plot(semivariog, fit.variog)

# model.variog<-vgm(psill=220, model="Lin", nugget=200, range=21500)
# fit.variog<-fit.variogram(semivariog, model.variog)
# plot(semivariog, fit.variog)

# model.variog<-vgm(psill=220, model="Exp", nugget=200, range=21500)

# krig <- krige(formula = LANDCOVERU ~ 1, locations = Land_Cover_UK_URB_test, 
  #            newdata = grd, model=model.variog)  # apply idw model for the data

# krig.output=as.data.frame(krig)
# names(krig.output)[1:3]<-c("long","lat","var1.pred")

# plot<-ggplot(data=krig.output,aes(x=long,y=lat))#start with the base-plot and add the Kriged data to it
#layer1<-c(geom_tile(data=krig.output,aes(fill=var1.pred)))#then create a tile layer and fill with predicted
#layer2<-c(geom_path(data=boroughoutline,aes(long, lat, group=group),colour = "grey40", size=1))#then create an outline
#plot+layer1+layer2+scale_fill_gradient(low="#FEEBE2", high="#7A0177")+coord_equal()


############  Topography UK ######################################################

Topography_London <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Topography_London.csv",
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

write.csv(idw.output, file = "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Topography_London_interp.csv", row.names=FALSE)

ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = TOPO_London))


############## Geographycally Weighted Regression ###########################
#############################################################################

library(spgwr)
library(AICcmodavg)
library("usdm")
library("nortest")

# mydata <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_1km_London_2010-12.csv",header = TRUE)

OE_PM25 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_1km_2009-11_interp.csv",header = TRUE)
In_situ_PM25 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/PM25_London_AIR_AVG_2010_2012_interp.csv",header = TRUE)
PM25_pcm <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/pcm_data/PM25_pcm_2009_11_London_interp.csv", header = TRUE)
Land_Cover <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Land_Cover_London_interp.csv",header = TRUE)
Altitude_London <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Altitude_London_interp.csv",header = TRUE)
TOPOGRAPHY <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Land_Cover_MCD12Q1_005_2007/Topography_London_interp.csv",header = TRUE)

# NIT <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/NO3_London_2013_interp.csv",header = TRUE)
# SO4 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/SO4_London_2013_interp.csv",header = TRUE)
# PC <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/EC_London_2013_interp.csv",header = TRUE)
# SOA <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/SOA_London_2013_interp.csv",header = TRUE)
# PM25 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/EMEP_Modelled/PM25_EMEP_London_2013_interp.csv",header = TRUE)

NIT <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/NO3_2009_2011_London_interp.csv",header = TRUE)
SO4 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/SO4_2009_2011_London_interp.csv",header = TRUE)
POC <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/POC_2009_2011_London_interp.csv",header = TRUE)
SOAA <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/SOAA_2009_2011_London_interp.csv",header = TRUE)
PM25 <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/cmaq/UK10_2009_2011/PM25_2009_2011_London_interp.csv",header = TRUE)


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
 NIT <- NIT$NO3_AVG
 SO4 <- SO4$SO4_AVG
 PC <- POC$POC_AVG
 SOA <- SOAA$SOAA_AVG
 PM25_CMAQ <- PM25$PM25_AVG
 
  NIT <- (NIT/PM25_CMAQ)*PM25_UK_pcm - (NIT/PM25_CMAQ)*OE
  SO4 <- (SO4/PM25_CMAQ)*PM25_UK_pcm - (SO4/PM25_CMAQ)*OE
  PC <- (PC/PM25_CMAQ)*PM25_UK_pcm - (PC/PM25_CMAQ)*OE
  SOA <- (SOA/PM25_CMAQ)*PM25_UK_pcm - (SOA/PM25_CMAQ)*OE
  
  NIT <- NIT/PM25_UK_pcm
  SO4 <- SO4/PM25_UK_pcm
  PC <- PC/PM25_UK_pcm
  SOA <- SOA/PM25_UK_pcm
  
  
#   NIT <- (NIT/PM25_CMAQ)*OE
#   SO4 <- (SO4/PM25_CMAQ)*OE
#   PC <- (PC/PM25_CMAQ)*OE
#   SOA <- (SOA*PM25_CMAQ)*OE
  
 
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

  mydata1 <- data.frame(BIAS, X, Y,URB, ED)
 # mydata1 <- data.frame(BIAS, X, Y, URB, PM25_CMAQ) 
#  mydata1 <- data.frame(BIAS, X, Y, URB, NIT)
#  mydata1 <- data.frame(BIAS, X, Y, URB, NIT, PC, SOA, SO4)
 # mydata1 <- data.frame(BIAS, X, Y, URB, NIT, PC, SOA)
 #mydata1 <- data.frame(BIAS, X, Y, URB, NIT, SO4)
 # mydata1 <- data.frame(BIAS, X, Y,URB)
 mydata1[is.na(mydata1)] <- 0
 
 
 nn <- 100/nrow(mydata1)
 memory.limit()
 # memory.limit(size=50000)
 
 gwr100 <- gwr(BIAS ~ URB + ED, data=mydata1,
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
 
 gwr100
 
 write.csv(gwr100$SDF, file = "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/GWR100_London_PM25_1km.csv")
 
 GWR_BIAS <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/GWR100_London_PM25_1km.csv",header = TRUE)
 
 GWR_BIAS$In_situ <- mydata$PM25_UK_pcm
 GWR_BIAS$OE_PM25 <- mydata$OE
 GWR_BIAS$URB_GWR <- (mydata1$URB)*(GWR_BIAS$URB)  ###urban fraction ug/m3/% 
  GWR_BIAS$ED_GWR <- (mydata1$ED)*(GWR_BIAS$ED)  ### ug/m3/m 
 # GWR_BIAS$NIT_GWR <- (mydata1$NIT)*(GWR_BIAS$NIT)
#  GWR_BIAS$SO4_GWR <- (mydata1$SO4)*(GWR_BIAS$SO4)
 # GWR_BIAS$PC_GWR <- (mydata1$PC)*(GWR_BIAS$PC)
#  GWR_BIAS$SOA_GWR <- (mydata1$SOA)*(GWR_BIAS$SOA)
 # GWR_BIAS$PM25_CMAQ_GWR <- (mydata1$PM25_CMAQ)*(GWR_BIAS$PM25_CMAQ)
 
  df <- data.frame(GWR_BIAS$OE_PM25, GWR_BIAS$URB_GWR, GWR_BIAS$ED_GWR)
 
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
 
 write.csv(GWR_AOE, file = "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/GWR_AOE_London.csv")
 
 


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

PM25_GWR <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/GWR_AOE_London_URB+NIT+SO4+PC+SOA.csv", header = TRUE)
PM25_GWR <- read.csv("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/GWR_AOE_London_URB_new.csv", header = TRUE)

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




