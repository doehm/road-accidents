# queensland road accident data

library(shiny)
library(tidyverse)
library(magrittr)
library(data.table)

# load data
load("./data/road-accident-data.Rdata")

# set filter vars
year <- accidents_raw$Crash_Year %>% unique %>% sort
day_of_week <- c("...", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
month <- c("...", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
weather_conditions <- c("...", accidents_raw$Crash_Atmospheric_Condition %>% unique %>% sort)
driving_conditions <- c("...", accidents_raw$Crash_Lighting_Condition %>% unique %>% sort)
road_condition <- c("...", accidents_raw$Crash_Road_Surface_Condition %>% unique %>% sort)
speed_limit <- c("...", accidents_raw$Crash_Speed_Limit %>% unique %>% sort)
road_feature <- c("...", accidents_raw$Crash_Roadway_Feature %>% unique %>% sort)
crash_type <- c("...", accidents_raw$Crash_Nature %>% unique %>% sort)
crash_severity <- c("...", accidents_raw$Crash_Severity %>% unique %>% sort)
loc_type <- colnames(accidents_raw)[str_detect(colnames(accidents_raw), "Loc_ABS|Loc_Local")]
loc_list <- sapply(loc_type, function(x) accidents_raw[[x]] %>% unique %>% sort)

# js code for making the map fit the screen height
jscode <- '
  $(document).on("shiny:connected", function(e) {
  var jsHeight = window.innerHeight;
  Shiny.onInputChange("GetScreenHeight",jsHeight);
  });
  '

# ui function
navbarPage("Road accidents in Queensland", id="road",
                 tabPanel("Heat map",
                          div(class="outer",
                              p(),
                              tags$script(jscode),
                              uiOutput("leafl"),
                              absolutePanel(
                                id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 90, left = "auto", right = 180, bottom = "auto",
                                width = 250, height = "auto",
                                HTML('<button data-toggle="collapse" data-target="#demo">Show filters</button>'),
                                tags$div(id = 'demo',  class="collapse",
                                         
                                         # select statistical area
                                         selectInput("loc", "Location category", loc_type, selected = "Loc_ABS_Statistical_Area_3"),
                                         
                                         # if sa2 select area with sa2
                                         conditionalPanel(
                                           condition = "input.loc == 'Loc_ABS_Statistical_Area_2'",
                                           selectInput("sa2", "Location", loc_list[["Loc_ABS_Statistical_Area_2"]], selected = "Brisbane City")
                                         ),
                                         
                                         # if sa3 select area with sa3
                                         conditionalPanel(
                                           condition = "input.loc == 'Loc_ABS_Statistical_Area_3'",
                                           selectInput("sa3", "Location", loc_list[["Loc_ABS_Statistical_Area_3"]], selected = "Brisbane Inner")
                                         ),
                                         
                                         # if sa4 select area with sa4
                                         conditionalPanel(
                                           condition = "input.loc == 'Loc_ABS_Statistical_Area_4'",
                                           selectInput("sa4", "Location", loc_list[["Loc_ABS_Statistical_Area_4"]], selected = "Brisbane Inner City")
                                         ),
                                         
                                         # if lga select area with lga
                                         conditionalPanel(
                                           condition = "input.loc == 'Loc_Local_Government_Area'",
                                           selectInput("lga", "Location", loc_list[["Loc_Local_Government_Area"]], selected = "Brisbane City")
                                         ),
                                         
                                         # if remoteness select area with remoteness
                                         conditionalPanel(
                                           condition = "input.loc == 'Loc_ABS_Remoteness'",
                                           selectInput("remote", "Location", loc_list[["Loc_ABS_Remoteness"]], selected = "Major Cities")
                                         ),
                                         
                                         #TODO: make all these conditional
                                         sliderInput("year", label = "Year", min = min(year), max = max(year), value = c(2013, 2018), round = TRUE, step = 1),
                                         selectInput("day_of_week", "Day of the week", day_of_week, selected = "..."),
                                         selectInput("month", "Month", month, selected = "..."),
                                         selectInput("weather", "Weather condition", weather_conditions, selected = "..."),
                                         selectInput("driving_conditions", "Driving condition", driving_conditions, selected = "..."),
                                         selectInput("road_condition", "Road condition", road_condition, selected = "..."),
                                         selectInput("speed_limit", "Speed limit", speed_limit, selected = "..."),
                                         selectInput("road_feature", "Road feature", road_feature, selected = "..."),
                                         selectInput("crash_type", "Crash type", crash_type, selected = "..."),
                                         selectInput("crash_severity", "Crash severity", crash_severity, selected = "...")
                                )
                              ),
                              
                              absolutePanel(
                                id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 180, left = 20, right = "auto", bottom = "auto",
                                width = 400, height = "auto",
                                plotOutput("casualty", height = 200),
                                plotOutput("unit", height = 200),
                                plotOutput("time_series", height = 200),
                                plotOutput("fatality_rate", height = 200),
                                p(HTML('Created by <a target="_blank" href="https://twitter.com/danoehm">@danoehm</a> / <a target="_blank" href="http://gradientdescending.com/">gradientdescending.com</a>')),
                                p(HTML('Source: Dept. of Transport and Main Roads Qld'))
                              )
                          )
                 ),
                 tabPanel(
                   "Data table",
                   div(
                     class = "outer",
                     p(),
                     tags$script(jscode),
                     dataTableOutput("crash_data")
                   )
                 )
)
