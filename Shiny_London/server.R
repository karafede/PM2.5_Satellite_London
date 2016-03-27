library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)
# library(ncdf4)
 #library(RNetCDF)
 # library(ncdf.tools)

#setwd("C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Shiny_London")

# PM25_sat <- readOGR(".geojson_PM25_1km_Sat_2009_2011", "OGRGeoJSON")
# PM25_sat <- readOGR(dsn = "C:/RICARDO-AEA/Donkelaar_1Km/Donkelaar_10km_Europe/Shiny_London/.geojson_PM25_1km_Sat_2009_2011", 
#                                                layer = "OGRGeoJSON")
 PM25_sat <- readOGR(dsn = "Copia.geojson",layer = "OGRGeoJSON")


qpal_SAT <- colorQuantile("Blues", PM25_sat$pm25_mean, n = 7)
qpal_UK_AIR <- colorQuantile("Reds", PM25_sat$pm25_mean_UK_AIR, n = 7)
qpal_pcm <- colorQuantile("Reds", PM25_sat$pm25_mean_pcm, n = 7)

# URB_Cover_nc <- raster::raster("URB_Cover_London.nc")
 URB_Cover_nc <- raster::raster("URB_cover.tif")
 
 pal_URB <- colorNumeric(c("#FFFFCC", "#41B6C4","#0C2C84"), getValues(URB_Cover_nc),
                         na.color = "transparent")

### color palettes for legends ###############################

pal_SAT <- colorNumeric(
  palette = "Blues",
  domain = PM25_sat$pm25_mean)

pal_UK_AIR <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_UK_AIR)

pal_pcm <- colorNumeric(
  palette = "Reds",
  domain = PM25_sat$pm25_mean_pcm)

###############################################################

shinyServer(function(input, output) {
  map = leaflet() %>% addTiles() %>% setView(-2, 52.5, 6)

  finalMap <- reactive({
      PM25_Layer <- input$variable
      YEAR <- input$year
        
      if (PM25_Layer == "pm25_mean" &
          YEAR == 2011) return(map %>% addPolygons(data = PM25_sat,
                                                      stroke = FALSE, smoothFactor = 0.2, 
                                                      fillOpacity = 0.5, 
                                                      color = ~ qpal_SAT(pm25_mean),
                                                      popup = ~ id) %>%
                      addLegend("bottomright", pal = pal_SAT, values = PM25_sat$pm25_mean,
                      title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) Sat : </strong>",
                      labFormat = labelFormat(prefix = ""), opacity = 1))
      
      if (PM25_Layer == "pm25_mean_UK_AIR" &
          YEAR == 2011) return(map %>% addPolygons(data = PM25_sat,
                                                       stroke = FALSE, smoothFactor = 0.2, 
                                                       fillOpacity = 0.5, 
                                                       color = ~ qpal_UK_AIR(pm25_mean_UK_AIR),
                                                        popup = ~id) %>%
                      addLegend("bottomright", pal = pal_UK_AIR , values = PM25_sat$pm25_mean_UK_AIR,
                      title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) UK-AIR : </strong>",
                      labFormat = labelFormat(prefix = ""), opacity = 1))
      
      if (PM25_Layer == "pm25_mean_pcm" &
          YEAR == 2012) return(map %>% addPolygons(data = PM25_sat,
                                                        stroke = FALSE, smoothFactor = 0.2, 
                                                        fillOpacity = 0.5, 
                                                        color = ~ qpal_pcm(pm25_mean_pcm),
                                                        popup = ~id) %>%
            addLegend("bottomright", pal = pal_pcm , values = PM25_sat$pm25_mean_pcm,
             title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) PMC : </strong>",
                labFormat = labelFormat(prefix = ""), opacity = 1))
      
   if (PM25_Layer == "URB_Cover_nc" &
       YEAR == 2012) return(map %>% addRasterImage(URB_Cover_nc, 
                                                       colors = pal_URB, 
                                                       opacity = 0.6) %>%
             addLegend("bottomright",pal = pal_URB, values = getValues(URB_Cover_nc),
             title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) URB Cover: </strong>",
             labFormat = labelFormat(prefix = ""),
             opacity = 0.6))
       
   else return (map) 
  #    else return(NULL)    
  })
  
    
  output$myMap = renderLeaflet(finalMap())
  
})