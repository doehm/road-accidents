
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
library(lazyeval)

# load data
load("./data/road-accident-data.Rdata")

# font
try({
  font_add_google(name = "Montserrat", family = "mont")
  showtext_auto()
}, TRUE)

# set my theme and colours
my_theme <- function(y_on = element_blank()){
  theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(family = "mont", size = 12),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text.y = y_on,
      axis.title = element_blank(),
      plot.title = element_text(family = "mont", hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(family = "mont", hjust = 0.5, face = "bold")
    )
}

my_cols <- function(n = 16) colorRampPalette(c("darkmagenta", "turquoise"))(n)

theme_forest <- function(scale = 1){
  theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(family = "mont", size = 16*scale),
      axis.text.x = element_text(vjust = 0.5),
      axis.title.y = element_blank(),
      axis.title.x = element_text(family = "mont", size = 16*scale),
      plot.title = element_text(family = "mont", hjust = 0.5, size = 26*scale, face = "bold"),
      plot.subtitle = element_text(family = "mont", hjust = 0.5, size = 18*scale),
      plot.caption = element_text(size = 12*scale)
    )
}

posterior_f <- function(df, y, n, a = 1.30, b = 77, inflator = 100) {
  out <- data.frame()
  qs <- c(0.025, 0.1, 0.5, 0.9, 0.975)
  for(k in 1:nrow(df)){
    out <- rbind(out, inflator*rbeta(1e4, shape1 = a+y[k], shape2 = b+n[k]-y[k]) %>% quantile(qs))
  }
  colnames(out) <- paste0("q", 100*qs)
  return(out)
}




set.seed(190513)
# fatalities
areas <- grep("Area", colnames(accidents_raw), value = TRUE)[3:4]
names(areas) <- c("sa3", "sa4")
fatality_fn <- function(area){
  accidents_raw %>% 
    group_by_(area) %>% 
    filter(
      Crash_Year == 2017,
    ) %>% 
    summarise(
      count = length(Count_Casualty_Total),
      n_fatalities = sum(Count_Casualty_Fatality)
    ) %>% 
    bind_cols(posterior_f(df = ., y = .$n_fatalities, n = .$count)) %>% 
    arrange(q50) %>% 
    mutate_(area = interp(~factor(v, level = v), v = as.name(area))) %>% 
    ggplot() +
    geom_segment(mapping = aes(x = q2.5, xend = q97.5, y = area, yend = area)) +
    geom_segment(mapping = aes(x = q10, xend = q90, y = area, yend = area, col = q50), size = 2) +
    geom_point(mapping = aes(x = q50, y = area), pch = 3) +
    theme_forest() +
    scale_colour_gradientn(colors = my_cols()) +
    labs(
      title = "Fatality rate given observed road accidents",
      subtitle = paste("Bayesian estimate of the fatality rate for", toupper(names(area)), "areas in 2017"),
      x = "Fatality rate (%)")
}
fatality_plots <- list(sa3 = fatality_fn(areas[1]), sa4 = fatality_fn(areas[2]))



# cyclists
cyclist_fn <- function(area){
  accidents_raw %>% 
    group_by_(area) %>% 
    filter(Crash_Year == 2017) %>% 
    summarise(
      count = n(),
      n_bicycles = sum(Count_Unit_Bicycle > 0)
    ) %>% 
    bind_cols(posterior_f(df = ., y = .$n_bicycles, n = .$count, a = 1.55, b = 25)) %>% 
    arrange(q50) %>% 
    mutate_(area = interp(~factor(v, level = v), v = as.name(area))) %>% 
    ggplot() +
    geom_segment(mapping = aes(x = q2.5, xend = q97.5, y = area, yend = area)) +
    geom_segment(mapping = aes(x = q10, xend = q90, y = area, yend = area, col = q50), size = 2) +
    geom_point(mapping = aes(x = q50, y = area), pch = 3) +
    theme_forest() +
    scale_colour_gradientn(colors = my_cols()) +
    labs(
      title = "Rate of cyclists involved in road accidents",
      subtitle = paste("Bayesian estimate of the rate of accidents involving cyclists for", toupper(names(area)), "areas in 2017"),
      x = "Accidents involving cyclists (%)"
    )
}
cyclist_plots <- list(sa3 = cyclist_fn(areas[1]), sa4 = cyclist_fn(areas[2]))


# pedestrians
pedestrian_fn <- function(area){
  accidents_raw %>% 
    group_by_(area) %>% 
    filter(Crash_Year == 2017) %>% 
    summarise(
      count = n(),
      n_bicycles = sum(Count_Unit_Pedestrian > 0)
    ) %>% 
    bind_cols(posterior_f(df = ., y = .$n_bicycles, n = .$count, a = 3, b = 60)) %>% 
    arrange(q50) %>% 
    mutate_(area = interp(~factor(v, level = v), v = as.name(area))) %>% 
    ggplot() +
    geom_segment(mapping = aes(x = q2.5, xend = q97.5, y = area, yend = area)) +
    geom_segment(mapping = aes(x = q10, xend = q90, y = area, yend = area, col = q50), size = 2) +
    geom_point(mapping = aes(x = q50, y = area), pch = 3) +
    theme_forest() +
    scale_colour_gradientn(colors = my_cols()) +
    labs(
      title = "Rate of pedestrians involved in road accidents",
      subtitle = paste("Bayesian estimate of the rate of accidents involving pedestrians for", toupper(names(area)), "areas in 2017"),
      x = "Pedestrians injured from the accident (%)"
    )
}
pedestrian_plots <- list(sa3 = pedestrian_fn(areas[1]), sa4 = pedestrian_fn(areas[2]))







function(input, output, session) {
  
  mainFilter <- reactive({
    accidents_raw %>% 
      {if(input$loc == "Loc_ABS_Statistical_Area_2") filter(., Loc_ABS_Statistical_Area_2 == input$sa2) else .} %>% 
      {if(input$loc == "Loc_ABS_Statistical_Area_3") filter(., Loc_ABS_Statistical_Area_3 == input$sa3) else .} %>% 
      {if(input$loc == "Loc_ABS_Statistical_Area_4") filter(., Loc_ABS_Statistical_Area_4 == input$sa4) else .} %>% 
      {if(input$loc == "Loc_Local_Government_Area") filter(., Loc_Local_Government_Area == input$lga) else .} %>% 
      {if(input$loc == "Loc_ABS_Remoteness") filter(., Loc_ABS_Remoteness == input$remote) else .} %>% 
      {if(input$unit == "Bicycle") filter(., Count_Unit_Bicycle > 0) else .} %>% 
      {if(input$unit == "Car") filter(., Count_Unit_Car > 0) else .} %>% 
      {if(input$unit == "Motocycle") filter(., Count_Unit_Motorcycle_Moped > 0) else .} %>% 
      {if(input$unit == "Truck") filter(., Count_Unit_Truck > 0) else .} %>% 
      {if(input$unit == "Pedestrian") filter(., Count_Unit_Pedestrian > 0) else .} %>% 
      {if(input$unit == "Bus") filter(., Count_Unit_Bus > 0) else .} %>% 
      {if(input$day_of_week != "..." & !is.null(input$day_of_the_week)) filter(., Crash_Day_Of_Week == input$day_of_week) else .} %>% 
      {if(input$month != "..." & !is.null(input$month)) filter(., Crash_Month == input$month) else .} %>% 
      {if(input$weather != "..." & !is.null(input$weather)) filter(., Crash_Atmospheric_Condition == input$weather) else .} %>% 
      {if(input$driving_conditions != "..." & !is.null(input$driving_conditions)) filter(., Crash_Lighting_Condition == input$driving_conditions) else .} %>% 
      {if(input$road_condition != "..." & !is.null(input$road_condition)) filter(., Crash_Road_Surface_Condition == input$road_condition) else .} %>% 
      {if(input$speed_limit != "..." & !is.null(input$speed_limit)) filter(., Crash_Speed_Limit == input$speed_limit) else .} %>% 
      {if(input$road_feature != "..." & !is.null(input$road_feature)) filter(., Crash_Roadway_Feature == input$road_feature) else .} %>% 
      {if(input$crash_type != "..." & !is.null(input$crash_type)) filter(., Crash_Nature == input$crash_type) else .} %>% 
      {if(input$crash_severity != "..." & !is.null(input$crash_severity)) filter(., Crash_Severity == input$crash_severity) else .} %>% 
      mutate(fatality = Count_Casualty_Fatality > 0)
  })
  
  selectData <- reactive({
    mainFilter() %>% 
      filter(Crash_Year >= input$year[1] & Crash_Year <= input$year[2])
  })
  
  # data frame for fatality rate simulation
  fatalityRate <- reactive({
    
    accidents_raw %>% 
      {if(input$loc == "Loc_ABS_Statistical_Area_2") filter(., Loc_ABS_Statistical_Area_2 == input$sa2) else .} %>% 
      {if(input$loc == "Loc_ABS_Statistical_Area_3") filter(., Loc_ABS_Statistical_Area_3 == input$sa3) else .} %>% 
      {if(input$loc == "Loc_ABS_Statistical_Area_4") filter(., Loc_ABS_Statistical_Area_4 == input$sa4) else .} %>% 
      {if(input$loc == "Loc_Local_Government_Area") filter(., Loc_Local_Government_Area == input$lga) else .} %>% 
      {if(input$loc == "Loc_ABS_Remoteness") filter(., Loc_ABS_Remoteness == input$remote) else .} %>% 
      filter(Crash_Year == 2017) %>% 
      summarise(
        count = length(Crash_Ref_Number),
        n_fatalities = sum(Count_Casualty_Fatality)
      )
    
  })
  
  # draws from the posterior
  fatalityRateSim <- reactive({
    rbeta(1e4, shape1 = 1.3 + fatalityRate()$n_fatalities, shape2 = 77 + fatalityRate()$count - fatalityRate()$n_fatalities)
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
    df <- mainFilter() %>% 
      filter(Crash_Year != 2018) %>% 
      group_by(Crash_Year) %>% 
      summarise(count = length(Crash_Ref_Number))
    
    ggplot(df, aes(x = Crash_Year, y = count)) +
      geom_line(col = "darkmagenta") +
      geom_point(col = "darkmagenta") +
      coord_cartesian(ylim = c(0, max(df$count))) +
      my_theme(y_on = element_text(family = "mont")) +
      labs(title = "Number of accidents from 2001-2017")
  })
  
  # rate dist
  output$fatality_rate <- renderPlot({
    data.frame(x = 100*fatalityRateSim()) %>% 
      ggplot(aes(x = x)) +
      geom_histogram(fill = "turquoise") +
      my_theme() +
      labs(title = paste0("Rate of road accident fatalities in 2017 (", round(100*median(fatalityRateSim()), 1), ")"))
  })
  
  # leaflet map
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
      height <- session$clientData$output_image1_height
      leafletOutput("accident_map", width = "100%", height = input$GetScreenHeight)
    }
  })
  
  output$crash_data <- renderDataTable({
    selectData()
  }, options = list(pageLength = 15))
  
  # TODO: fix the height of this output - make it dynamic
  # also just make this better - for now, path of least resistance
  selectForestPlot <- reactive({
    if(input$floc == "Loc_ABS_Statistical_Area_3"){
      k <- 1
    }else if(input$floc == "Loc_ABS_Statistical_Area_4"){
      k <- 2
    }
    if(input$measure == "Fatality"){
      fatality_plots[[k]]
    }else if(input$measure == "Cyclists"){
      cyclist_plots[[k]]
    }else if(input$measure == "Pedestrians"){
      pedestrian_plots[[k]]
    }
  })
  
  output$forest_plot <- renderPlot({
    selectForestPlot()
  }, width = 900, height = 1200)

}
