# Load library
library(shiny)
library(dplyr)
library(leaflet)
library(readr)
library(KernSmooth)
library(shinyWidgets)
library(plotly)
library(sp)
library(mapview)
library(leafem)
library(rgdal)
library(maptools)
library(raster)
library(DT)
library(htmlwidgets)

load("data/save.RData")
# Load in training data