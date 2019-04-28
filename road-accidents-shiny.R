# queensland road accident data

# libraries
library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(htmltools)
library(htmlwidgets)
library(showtext)
library(data.table)


# font
font_add_google(name = "Montserrat", family = "mont")
showtext_auto()


# load data
load_data <- function(){
  if(!dir.exists("./road-accidents")){
    print("Create a directory './road-accidents/' to continue.")
  }else{
    if(!file.exists("./road-accidents/locations.csv")){
      cat('\n Download may take a few minutes...\n')
      url <- "http://www.tmr.qld.gov.au/~/media/aboutus/corpinfo/Open%20data/crash/locations.csv"
      download.file(url, destfile = "./road-accidents/locations.csv", method="libcurl")
    }
    accidents_raw <- read_csv("./road-accidents/locations.csv")
  }
  return(accidents_raw)
}
accidents_raw <- load_data()


# set my theme and colours
my_theme <- function(){
  theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(family = "mont", size = 12),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.title = element_blank(),
      plot.title = element_text(family = "mont", hjust = 0.5)
    )
}

my_cols <- function(n = 16) colorRampPalette(c("darkmagenta", "turquoise"))(n)


# set filter vars
year <- accidents_raw$Crash_Year %>% unique %>% sort
sa3_list <- accidents_raw$Loc_ABS_Statistical_Area_3 %>% unique %>% sort
day_of_week <- c("...", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
month <- c("...", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
weather_conditions <- c("...", accidents_raw$Crash_Atmospheric_Condition %>% unique %>% sort)
driving_conditions <- c("...", accidents_raw$Crash_Lighting_Condition %>% unique %>% sort)
road_condition <- c("...", accidents_raw$Crash_Road_Surface_Condition %>% unique %>% sort)
speed_limit <- c("...", accidents_raw$Crash_Speed_Limit %>% unique %>% sort)
road_feature <- c("...", accidents_raw$Crash_Roadway_Feature %>% unique %>% sort)
crash_type <- c("...", accidents_raw$Crash_Nature %>% unique %>% sort)
crash_severity <- c("...", accidents_raw$Crash_Severity %>% unique %>% sort)
loc_list <- colnames(accidents_raw)[str_detect(colnames(accidents_raw), "Loc")]


# js code for collapsible panel
jscode <- '
$(document).on("shiny:connected", function(e) {
var jsHeight = window.innerHeight;
Shiny.onInputChange("GetScreenHeight",jsHeight);
});
'


# ui function
ui <- navbarPage("Road accidents in Queensland", id="road",
                 tabPanel("Heat map",
                          div(class="outer",
                            p(),
                            tags$script(jscode),
                            uiOutput("leafl"),
                            absolutePanel(
                              id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 200, left = "auto", right = 20, bottom = "auto",
                              width = 250, height = "auto",
                              HTML('<button data-toggle="collapse" data-target="#demo">Show filters</button>'),
                              tags$div(id = 'demo',  class="collapse",
                              
                                p("Select filters to view accident data over time and road, weather and vehicle type features"),
                                
                                sliderInput("year", label = "Year", min = min(year), max = max(year), value = c(2013, 2018), round = TRUE, step = 1),
                                selectInput("sa3", "SA3", sa3_list, selected = "Brisbane Inner"),
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
                              p(HTML('Created by <a target="_blank" href="https://twitter.com/danoehm">@danoehm</a> / <a target="_blank" href="http://gradientdescending.com/">gradientdescending.com</a>')),
                              plotOutput("casualty", height = 250),
                              plotOutput("unit", height = 250),
                              plotOutput("time_series", height = 250)
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



# server function
server <- function(input, output, session) {
  
  selectData <- reactive({
    accidents_raw %>% 
      filter(Loc_ABS_Statistical_Area_3 == input$sa3) %>% 
      filter(Crash_Year >= input$year[1] & Crash_Year <= input$year[2]) %>% 
      {if(input$day_of_week != "...") filter(., Crash_Day_Of_Week == input$day_of_week) else .} %>% 
      {if(input$month != "...") filter(., Crash_Month == input$month) else .} %>% 
      {if(input$weather != "...") filter(., Crash_Atmospheric_Condition == input$weather) else .} %>% 
      {if(input$driving_conditions != "...") filter(., Crash_Lighting_Condition == input$driving_conditions) else .} %>% 
      {if(input$road_condition != "...") filter(., Crash_Road_Surface_Condition == input$road_condition) else .} %>% 
      {if(input$speed_limit != "...") filter(., Crash_Speed_Limit == input$speed_limit) else .} %>% 
      {if(input$road_feature != "...") filter(., Crash_Roadway_Feature == input$road_feature) else .} %>% 
      {if(input$crash_type != "...") filter(., Crash_Nature == input$crash_type) else .} %>% 
      {if(input$crash_severity != "...") filter(., Crash_Severity == input$crash_severity) else .} %>% 
      mutate(fatality = Count_Casualty_Fatality > 0)
  })
  
  # count of casualty type
  output$casualty <- renderPlot({
    selectData() %>% 
      summarise(
        Fatality = sum(Count_Casualty_Fatality),
        Hospitalised = sum(Count_Casualty_Hospitalised),
        `Medically treated` = sum(Count_Casualty_MedicallyTreated),
        `Minor Injury` = sum(Count_Casualty_MinorInjury)
      ) %>% 
      gather(casualty_type, count) %>% 
      ggplot(aes(x = casualty_type, y = count, fill = count)) +
      geom_bar(stat = "identity", alpha = 0.75) +
      labs(title = "Severity of crash") +
      scale_fill_gradientn(colors = my_cols()) +
      my_theme() +
      geom_text(aes(x = casualty_type, y = max(count)/4, label = count), family = "mont")
  })
  
  # number of units involved with the crash
  output$unit <-renderPlot({
    selectData() %>% 
      summarise(
        Car = sum(Count_Unit_Car),
        Motorcycle = sum(Count_Unit_Motorcycle_Moped),
        Truck = sum(Count_Unit_Truck),
        Bus = sum(Count_Unit_Bus),
        Bike = sum(Count_Unit_Bicycle),
        Pedestrian = sum(Count_Unit_Pedestrian),
        Other = sum(Count_Unit_Other)
      ) %>% 
      gather(unit_type, count) %>% 
      ggplot(aes(x = unit_type, y = count, fill = count)) +
      geom_bar(stat = "identity", alpha = 0.75) +
      labs(title = "Types of vehicles involved in accident") +
      scale_fill_gradientn(colors = my_cols()) +
      my_theme() +
      geom_text(aes(x = unit_type, y = max(count)/4, label = count), family = "mont")
  }) 
  
  # crashes over time
  output$time_series <- renderPlot({
    accidents_raw %>% 
      filter(Loc_ABS_Statistical_Area_3 == input$sa3 & Crash_Year != 2018) %>% 
      group_by(Crash_Year) %>% 
      summarise(count = n()) %>% 
      ggplot(aes(x = Crash_Year, y = count)) +
      geom_area(fill = "darkmagenta", alpha = 0.7) + 
      my_theme() +
      labs(title = "Number of accidents from 2001-2017")
  })
  
  output$accident_map <- renderLeaflet({
    leaflet(selectData()) %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Black and white") %>% 
      addTiles(options = providerTileOptions(noWrap = TRUE), group="Colour") %>% 
      addMarkers(
        lng = ~Crash_Longitude_GDA94, 
        lat = ~Crash_Latitude_GDA94,
        clusterOptions = markerClusterOptions(),
        label = ~htmlEscape(paste0(Crash_Ref_Number, ": ", Crash_DCA_Description))
      ) %>% 
      addCircleMarkers(
        lng = ~Crash_Longitude_GDA94[selectData()$fatality], 
        lat = ~Crash_Latitude_GDA94[selectData()$fatality],
        color = "#8B0000",
        stroke = FALSE,
        fillOpacity = 0.8,
        group = "Fatalities"
      ) %>% 
      addHeatmap(
        lng = ~Crash_Longitude_GDA94, 
        lat = ~Crash_Latitude_GDA94,
        radius = 17,
        blur = 25,
        cellSize = 25
      ) %>% 
      
      # adding in some controls directly into leaflet rather than through shiny
      # this way we won't have to draw a new map every time
      addLayersControl(
        overlayGroups = c("Fatalities"),
        baseGroups = c("Black and white","Colour"), 
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$leafl <- renderUI({
    if(!is.null(input$GetScreenHeight)){
      width  <- session$clientData$output_image1_width
      print(session$clientData)
      height <- session$clientData$output_image1_height
      leafletOutput("accident_map", width = "100%", height = input$GetScreenHeight)
    }
  })
  
  output$crash_data <- renderDataTable({
    selectData()
  }, options = list(pageLength = 15))
}


# run app
shinyApp(ui, server)

