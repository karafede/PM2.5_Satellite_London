
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

setwd("C:/SATELLITE_STUFF/Donkelaar_1Km/UK_France_Donkelaar_2016")
dir_UK <- "C:/SATELLITE_STUFF/Donkelaar_1Km/UK_France_Donkelaar_2016/UK_shp"
dir_FR <- "C:/SATELLITE_STUFF/Donkelaar_1Km/UK_France_Donkelaar_2016/France_shp"

### shapefile for local authorities in England
shp_UK <- readOGR(dsn = dir_UK, layer = "GBR_adm0")
shp_FR <- readOGR(dsn = dir_FR, layer = "france_all_10")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UK <- spTransform(shp_UK, CRS("+init=epsg:4326"))
shp_FR <- spTransform(shp_FR, CRS("+init=epsg:4326"))
# names(shp)

shp_UK@data$name <- 1:nrow(shp_UK)
shp_FR@data$name <- 1:nrow(shp_FR)


######## NEW RESULTS from GWR of Donkellar 2016 - global runs ###########
#########################################################################

PM25_2009_nc <- raster("GlobalGWR_PM25_GL_200901_200912-RH35.nc")
PM25_2010_nc <- raster("GlobalGWR_PM25_GL_201001_201012-RH35.nc")
PM25_2011_nc <- raster("GlobalGWR_PM25_GL_201101_201112-RH35.nc")
PM25_2012_nc <- raster("GlobalGWR_PM25_GL_201201_201212-RH35.nc")
PM25_2013_nc <- raster("GlobalGWR_PM25_GL_201301_201312-RH35.nc")
PM25_2014_nc <- raster("GlobalGWR_PM25_GL_201401_201412-RH35.nc")


#plot(PM25_2009_nc)
#plot(shp, add=TRUE, lwd=1)


### crop raster over the UK shp file  ###############################

 PM25_2009_UK_nc_cropped <- crop(PM25_2009_nc, extent(shp_UK))
 PM25_2009_UK_nc_cropped <- mask(PM25_2009_UK_nc_cropped, shp_UK)
 plot(PM25_2009_UK_nc_cropped)
 
 PM25_2009_UK_nc <- writeRaster(PM25_2009_UK_nc_cropped,
                                filename="UK_PM2.5_1km_MODIS_GWR_2009.nc",
                                format="CDF", overwrite=TRUE) 
 PM25_2009_UK_nc <- raster("UK_PM2.5_1km_MODIS_GWR_2009.nc")
 
# PM25_2010_nc_cropped <- crop(PM25_2010_nc, extent(shp_UK))
# PM25_2010_nc_cropped <- mask(PM25_2010_nc_cropped, shp_UK)
# 
# PM25_2011_nc_cropped <- crop(PM25_2011_nc, extent(shp_UK))
# PM25_2011_nc_cropped <- mask(PM25_2011_nc_cropped, shp_UK)
# 
# PM25_2012_nc_cropped <- crop(PM25_2012_nc, extent(shp_UK))
# PM25_2012_nc_cropped <- mask(PM25_2012_nc_cropped, shp_UK)
# 
# 
# PM25_2013_nc_cropped <- crop(PM25_2013_nc, extent(shp_UK))
# PM25_2013_nc_cropped <- mask(PM25_2013_nc_cropped, shp_UK)

PM25_2014_UK_nc_cropped <- crop(PM25_2014_nc, extent(shp_UK))
PM25_2014_UK_nc_cropped <- mask(PM25_2014_UK_nc_cropped, shp_UK)
plot(PM25_2014_UK_nc_cropped)


# plot(PM25_2014_UK_nc_cropped)
# plot(shp, add=TRUE, lwd=2)


PM25_2014_UK_nc <- writeRaster(PM25_2014_UK_nc_cropped,
                               filename="UK_PM2.5_1km_MODIS_GWR_2014.nc",
                               format="CDF", overwrite=TRUE) 
PM25_2014_UK_nc <- raster("UK_PM2.5_1km_MODIS_GWR_2014.nc")


### crop raster over the France shp file  ###############################

PM25_2009_FR_nc_cropped <- crop(PM25_2009_nc, extent(shp_FR))
PM25_2009_FR_nc_cropped <- mask(PM25_2009_FR_nc_cropped, shp_FR)
plot(PM25_2009_FR_nc_cropped)

PM25_2009_FR_nc <- writeRaster(PM25_2009_FR_nc_cropped,
                               filename="France_PM2.5_1km_MODIS_GWR_2009.nc",
                               format="CDF", overwrite=TRUE) 
PM25_2009_FR_nc <- raster("France_PM2.5_1km_MODIS_GWR_2009.nc")



PM25_2014_FR_nc_cropped <- crop(PM25_2014_nc, extent(shp_FR))
PM25_2014_FR_nc_cropped <- mask(PM25_2014_FR_nc_cropped, shp_FR)
plot(PM25_2014_FR_nc_cropped)

PM25_2014_FR_nc <- writeRaster(PM25_2014_FR_nc_cropped,
                                 filename="France_PM2.5_1km_MODIS_GWR_2014.nc",
                                 format="CDF", overwrite=TRUE) 
PM25_2014_FR_nc <- raster("France_PM2.5_1km_MODIS_GWR_2014.nc")
plot(PM25_2014_FR_nc)

####### Map data with Leaflet #############################################

# define color palette

### colors for raster GWR_1km new data 2009_2016 Donkelaar (2016)

pal_GWR_UK_PM25_1km_2009 <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                                    getValues(PM25_2009_UK_nc),na.color = "transparent")

pal_GWR_FR_PM25_1km_2009 <- colorNumeric(c("#9999FF","#FFFF00", "#FF0000", "#ff8000"), # "#b30000"
                                    getValues(PM25_2009_FR_nc),na.color = "transparent")



#  pal_GWR_UK_PM25_1km_2009 <- colorNumeric(
#    palette = "Reds",
#    getValues(PM25_2009_UK_nc),na.color = "transparent")
 

pal_GWR_UK_PM25_1km <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                                     getValues(PM25_2014_UK_nc),na.color = "transparent")

pal_GWR_FR_PM25_1km <- colorNumeric(c("#9999FF","#FFFF00", "#FF0000", "#ff8000"), # "#b30000"
                                    getValues(PM25_2014_FR_nc),na.color = "transparent")

# Leaflet map

map <- leaflet() %>% addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%

  
  

addRasterImage(PM25_2014_UK_nc, colors = pal_GWR_UK_PM25_1km, opacity = 0.7,
               group = "UK_GWR_2014") %>%
addLegend("bottomleft",pal = pal_GWR_UK_PM25_1km, values = values(PM25_2014_UK_nc),
             title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) UK GWR (2014): </strong>",
             labFormat = labelFormat(prefix = ""),
             opacity = 0.6) %>%


addRasterImage(PM25_2014_FR_nc, colors = pal_GWR_FR_PM25_1km, opacity = 0.7,              
               group = "France_GWR_2014") %>%
addLegend("bottomleft",pal = pal_GWR_FR_PM25_1km, values = values(PM25_2014_FR_nc),
            title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) France GWR (2014): </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.6) %>%
  
  
  
  addRasterImage(PM25_2009_UK_nc, colors = pal_GWR_UK_PM25_1km_2009, opacity = 0.7,
                 group = "UK_GWR_2009") %>%
  addLegend("bottomright",pal = pal_GWR_UK_PM25_1km_2009, values = values(PM25_2009_UK_nc),
            title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) UK GWR (2009): </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.6) %>%
  
  
  addRasterImage(PM25_2009_FR_nc, colors = pal_GWR_FR_PM25_1km_2009, opacity = 0.7,              
                 group = "France_GWR_2009") %>%
  addLegend("bottomright",pal = pal_GWR_FR_PM25_1km_2009, values = values(PM25_2009_FR_nc),
            title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) France GWR (2009): </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.6) %>%  
  

# Layers control
addLayersControl(
  baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
  overlayGroups = c("UK_GWR_2014","France_GWR_2014",
                    "UK_GWR_2009", "France_GWR_2009"),
  
options = layersControlOptions(collapsed = FALSE)) %>%
# hideGroup("France_GWR_2014") 
hideGroup("France_GWR_2009") %>%
hideGroup("UK_GWR_2009") 


map



saveWidget(map,
           file="EU_France_2014_PM25_GWR.html",
           selfcontained = FALSE)
