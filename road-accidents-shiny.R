# queensland road accident data

# libraries
library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(magrittr)


# load data
accidents <- read_csv("./road-accidents/locations.csv")

# sample for devlopment
accidents %<>% filter(Loc_Suburb == "Brisbane City")

# basic leaflet
m <- leaflet(accidents) %>% 
  addTiles() %>% 
  # addCircles(lng = ~Crash_Longitude_GDA94, lat = ~Crash_Latitude_GDA94) %>% 
  addHeatmap(
    lng = ~Crash_Longitude_GDA94, 
    lat = ~Crash_Latitude_GDA94,
    radius = 7
    )
