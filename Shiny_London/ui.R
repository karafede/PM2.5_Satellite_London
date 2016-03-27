library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)
# library(ncdf4)
# library(RNetCDF)
# library(ncdf.tools)

# Choices for drop-downs

years <- c(
  "2011" = "2011",
  "2012" ="2012"
)

vars <- c(
  "PM25_SAT" = "pm25_mean",
  "PM25_UK_AIR" = "pm25_mean_UK_AIR",
  "PM25_PCM" = "pm25_mean_pcm",
  "URB_COVER" = "URB_Cover_nc"
)


 ############################################################
 
 shinyUI(fluidPage(
   
   # Application title
   titlePanel("Greater London Area, Satellite and UK AIR data"),
   
   # Sidebar with a slider input for number of bins
   sidebarLayout(
     sidebarPanel(
       selectInput("variable", "Choose a Layer",vars, selected = "pm25_mean"),
       selectInput("year", "Choose a year",years, selected = "2011")
     ),
     
     # Show leaflet map with a text div reporting the selected date and extents 
     mainPanel(
       
       leafletOutput('myMap', height = 500, width = 800)
     )
   )
 ))

 