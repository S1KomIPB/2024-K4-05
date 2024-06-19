library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(jsonlite)

server <- function(input, output) {
  
#------------------------ Data Filter ------------------------#
  parsed_data <- reactive({
    api_url <- 'https://api-data.telkomiot.id/api/v2.0/APP63c0d771ca53f75491/DEV651acb273543c59748/history?starttime=2024-06-01&endtime=2024-06-18'
    
    json_data <- fromJSON(api_url)
    
    # Extract relevant data from the nested structure
    relevant_data <- lapply(json_data$data, function(x) {
      # Check if the data is character type
      if (is.character(x)) {
        # Convert character data to JSON and return
        data <- fromJSON(x)
        return(data)
      } else {
        return(NULL)
      }
    })
    
    # Extract timestamps from the JSON
    timestamps <- as.POSIXct(json_data$time, format = "%Y-%m-%dT%H:%M:%OSZ")
    
    # Create a function to extract values or return NA if missing
    extract_values <- function(x, field) {
      if (!is.null(x) && field %in% names(x)) {
        return(x[[field]])
      } else {
        return(NA)
      }
    }
    
    # Combine timestamps and relevant data into a data frame
    # For data frame 1 (tem, hum, adcSM, SM)
    df_1 <- data.frame(timestamps = timestamps,
                       Tem = sapply(relevant_data, function(x) extract_values(x, "Tem")),
                       Hum = sapply(relevant_data, function(x) extract_values(x, "Hum")),
                       adcSM = sapply(relevant_data, function(x) extract_values(x, "adcSM")),
                       SM = sapply(relevant_data, function(x) extract_values(x, "SM")))
    
    # For data frame 2 (WD, WS, GWL)
    df_2 <- data.frame(timestamps = timestamps,
                       WD = sapply(relevant_data, function(x) extract_values(x, "WD")),
                       WS = sapply(relevant_data, function(x) extract_values(x, "WS")),
                       GWL = sapply(relevant_data, function(x) extract_values(x, "GWL")))
    
    # For data frame 3 (RF, adcL, Lux)
    df_3 <- data.frame(timestamps = timestamps,
                       RF = sapply(relevant_data, function(x) extract_values(x, "RF")),
                       adcL = sapply(relevant_data, function(x) extract_values(x, "adcL")),
                       Lux = sapply(relevant_data, function(x) extract_values(x, "Lux")))
    
    # Keep only rows without NA values
    df_1 <- df_1[complete.cases(df_1), ]
    df_2 <- df_2[complete.cases(df_2), ]
    df_3 <- df_3[complete.cases(df_3), ]
    
    return(list(df_1 = df_1, df_2 = df_2, df_3 = df_3))
  })
  
  convert_wind_direction <- function(degrees) {
    if (degrees == 0) {
      return("N")
    } else if (degrees > 0 && degrees < 90) {
      return("NE")
    } else if (degrees == 90) {
      return("E")
    } else if (degrees > 90 && degrees < 180) {
      return("SE")
    } else if (degrees == 180) {
      return("S")
    } else if (degrees > 180 && degrees < 270) {
      return("SW")
    } else if (degrees == 270) {
      return("W")
    } else {
      return("NW")
    }
  }
  
  output$timestampBox <- renderValueBox({
    data <- parsed_data()
    last_index <- nrow(data$df_1)
    valueBox(
      format(data$df_1$timestamps[last_index], "%Y-%m-%d %H:%M:%S"),  
      "Last fetched data @Danau SDGs, IPB University", 
      icon = icon("clock"), 
      color = "black"
    )
  })
  
  output$humidityBox <- renderValueBox({
    data <- parsed_data()
    last_index <- nrow(data$df_1)
    valueBox(paste0(data$df_1$Hum[last_index], " %"), "Humidity", icon = icon("tint"), color = "purple")
  })
  
  output$soilMoistureBox <- renderValueBox({
    data <- parsed_data()
    last_index <- nrow(data$df_1)
    valueBox(paste0(data$df_1$SM[last_index], " mm"), "Soil Moisture", icon = icon("tint"), color = "orange")
  })
  
  output$temperatureBox <- renderValueBox({
    data <- parsed_data()
    last_index <- nrow(data$df_1)
    valueBox(paste0(data$df_1$Tem[last_index], " °C"), "Temperature", icon = icon("thermometer-half"), color = "red")
  })
  
  output$windSpeedBox <- renderValueBox({
    data <- parsed_data()
    last_index <- nrow(data$df_2)
    valueBox(paste0(data$df_2$WS[last_index], " Km/h"), "Wind Speed", icon = icon("wind"), color = "green")
  })
  
  output$windDirectionBox <- renderValueBox({
    data <- parsed_data()
    last_index <- nrow(data$df_2)
    direction <- sapply(data$df_2$WD[last_index], convert_wind_direction)
    valueBox(paste0(data$df_2$WD[last_index], "° (", direction, ")"), "Wind Direction", icon = icon("compass"), color = "teal")
  })
  
  output$groundWaterLevelBox <- renderValueBox({
    data <- parsed_data()
    last_index <- nrow(data$df_2)
    valueBox(paste0(data$df_2$GWL[last_index], " cm"), "Ground Water Level", icon = icon("water"), color = "blue")
  })
  
  parameterPlot <- function(data, parameter, y_label) {
    renderPlotly({
      plot_ly(data, x = ~timestamps, y = as.formula(paste0("~", parameter)), type = 'scatter', mode = 'lines+markers',
              line = list(color = sample(colors(), 1))) %>%
        layout(title = y_label,
               titlefont = list(size = 24), 
               margin = list(t = 100),
               xaxis = list(title = "Date"),
               yaxis = list(title = y_label))
    })
  }
 
  output$humidityPlot <- parameterPlot(parsed_data()$df_1, "Hum", "Humidity (%)")
  output$soilMoisturePlot <- parameterPlot(parsed_data()$df_1, "SM", "Soil Moisture (mm)")
  output$temperaturePlot <- parameterPlot(parsed_data()$df_1, "Tem", "Temperature (°C)")
  output$groundWaterLevelPlot <- parameterPlot(parsed_data()$df_2, "GWL", "Ground Water Level (cm)")
  
  output$windRosePlot <- renderPlotly({
    
    data <- parsed_data()$df_2
    
    freq_data <- data %>%
      group_by(WD) %>%
      summarise(freq = n()) %>%
      ungroup()
    plot_ly(freq_data, r = ~freq, theta = ~WD, type = 'barpolar',
            marker = list(
              color = ~freq,
              cmin = 0,
              cmax = 40,
              colorbar = list(
                title = "Speed",
                titleside = "right",
                tickmode = "array",
                tickvals = c(0, 4, 6, 10, 15, 20, 25),
                ticktext = c("0-4", "4-6", "6-10", "10-15", "15-20", "20-25", "40")
              ),
              colorscale = list(
                c(0.00, 'rgb(102, 153, 255)'),  
                c(0.10, 'rgb(102, 153, 255)'),
                c(0.10, 'rgb(51, 204, 51)'),    
                c(0.15, 'rgb(51, 204, 51)'),
                c(0.15, 'rgb(255, 255, 51)'),   
                c(0.25, 'rgb(255, 255, 51)'),
                c(0.25, 'rgb(255, 153, 0)'),    
                c(0.375, 'rgb(255, 153, 0)'),
                c(0.375, 'rgb(255, 51, 51)'),   
                c(0.50, 'rgb(255, 51, 51)'),
                c(0.50, 'rgb(153, 51, 255)'),   
                c(0.625, 'rgb(153, 51, 255)'),
                c(0.625, 'rgb(102, 0, 204)'),   
                c(1.00, 'rgb(102, 0, 204)')
              )
            )) %>%
      layout(title = "Arah dan Kecepatan Angin",
             titlefont = list(size = 24),
             margin = list(t = 100),
             polar = list(
               radialaxis = list(ticksuffix = " unit"),
               angularaxis = list(direction = "clockwise", type = "categories")
             ))
  })
  
  #------------------------ Data Filter ------------------------#
  
  
  
  #------------------------ Dashboard ------------------------#
  parsed_data_realtime <- reactive({
    api_url_realtime <- 'https://api-data.telkomiot.id/api/v2.0/APP63c0d771ca53f75491/DEV651acb273543c59748/lasthistory'
    
    json_data_realtime <- fromJSON(api_url_realtime)
    
    relevant_data_realtime <- lapply(json_data_realtime$data, function(x) {
      if (is.character(x)) {
        # Convert character data to JSON and return
        data_realtime <- fromJSON(x)
        return(data_realtime)
      } else {
        return(NULL)
      }
    })
    
    # Extract timestamps from the JSON
    timestamps_realtime <- as.POSIXct(json_data_realtime$time, format = "%Y-%m-%dT%H:%M:%OSZ")
    
    # Create a function to extract values or return NA if missing
    extract_values_realtime <- function(x, field) {
      if (!is.null(x) && field %in% names(x)) {
        return(x[[field]])
      } else {
        return(NA)
      }
    }
    
    # Combine timestamps and relevant data into a data frame
    # For data frame 1 (tem, hum, adcSM, SM)
    df_1_realtime <- data.frame(timestamps = timestamps_realtime,
                                Tem = sapply(relevant_data_realtime, function(x) extract_values_realtime(x, "Tem")),
                                Hum = sapply(relevant_data_realtime, function(x) extract_values_realtime(x, "Hum")),
                                adcSM = sapply(relevant_data_realtime, function(x) extract_values_realtime(x, "adcSM")),
                                SM = sapply(relevant_data_realtime, function(x) extract_values_realtime(x, "SM")))
    
    # For data frame 2 (WD, WS, GWL)
    df_2_realtime <- data.frame(timestamps = timestamps_realtime,
                                WD = sapply(relevant_data_realtime, function(x) extract_values_realtime(x, "WD")),
                                WS = sapply(relevant_data_realtime, function(x) extract_values_realtime(x, "WS")),
                                GWL = sapply(relevant_data_realtime, function(x) extract_values_realtime(x, "GWL")))
    
    # Keep only rows without NA values
    df_1_realtime <- df_1_realtime[complete.cases(df_1_realtime), ]
    df_2_realtime <- df_2_realtime[complete.cases(df_2_realtime), ]
    
    return(list(df_1_realtime = df_1_realtime, df_2_realtime = df_2_realtime))
  })
  
  convert_wind_direction_realtime <- function(degrees) {
    if (degrees == 0) {
      return("N")
    } else if (degrees > 0 && degrees < 90) {
      return("NE")
    } else if (degrees == 90) {
      return("E")
    } else if (degrees > 90 && degrees < 180) {
      return("SE")
    } else if (degrees == 180) {
      return("S")
    } else if (degrees > 180 && degrees < 270) {
      return("SW")
    } else if (degrees == 270) {
      return("W")
    } else {
      return("NW")
    }
  }
  
  output$timestampBoxDB <- renderValueBox({
    data_realtime <- parsed_data_realtime()
    last_index_realtime <- nrow(data_realtime$df_1_realtime)
    valueBox(
      format(data_realtime$df_1_realtime$timestamps[last_index_realtime], "%Y-%m-%d %H:%M:%S"),  
      "Last fetched data @Danau SDGs, IPB University", 
      icon = icon("clock"), 
      color = "black"
    )
  })
  
  output$humidityBoxDB <- renderValueBox({
    data_realtime <- parsed_data_realtime()
    last_index_realtime <- nrow(data_realtime$df_1_realtime)
    valueBox(paste0(data_realtime$df_1_realtime$Hum[last_index_realtime], " %"), "Humidity", icon = icon("tint"), color = "purple")
  })
  
  output$soilMoistureBoxDB <- renderValueBox({
    data_realtime <- parsed_data_realtime()
    last_index_realtime <- nrow(data_realtime$df_1_realtime)
    valueBox(paste0(data_realtime$df_1_realtime$SM[last_index_realtime], " mm"), "Soil Moisture", icon = icon("tint"), color = "orange")
  })
  
  output$temperatureBoxDB <- renderValueBox({
    data_realtime <- parsed_data_realtime()
    last_index_realtime <- nrow(data_realtime$df_1_realtime)
    valueBox(paste0(data_realtime$df_1_realtime$Tem[last_index_realtime], " °C"), "Temperature", icon = icon("thermometer-half"), color = "red")
  })
  
  output$windSpeedBoxDB <- renderValueBox({
    data_realtime <- parsed_data_realtime()
    last_index_realtime <- nrow(data_realtime$df_2_realtime)
    valueBox(paste0(data_realtime$df_2_realtime$WS[last_index_realtime], " Km/h"), "Wind Speed", icon = icon("wind"), color = "green")
  })
  
  output$windDirectionBoxDB <- renderValueBox({
    data_realtime <- parsed_data_realtime()
    last_index_realtime <- nrow(data_realtime$df_2_realtime)
    direction <- sapply(data_realtime$df_2_realtime$WD[last_index_realtime], convert_wind_direction_realtime)
    valueBox(paste0(data_realtime$df_2_realtime$WD[last_index_realtime], "° (", direction, ")"), "Wind Direction", icon = icon("compass"), color = "teal")
  })
  
  output$groundWaterLevelBoxDB <- renderValueBox({
    data_realtime <- parsed_data_realtime()
    last_index_realtime <- nrow(data_realtime$df_2_realtime)
    valueBox(paste0(data_realtime$df_2_realtime$GWL[last_index_realtime], " cm"), "Ground Water Level", icon = icon("water"), color = "blue")
  })
  
  parameterPlot_realtime <- function(data, parameter, y_label) {
    renderPlotly({
      plot_ly(data, x = ~timestamps, y = as.formula(paste0("~", parameter)), type = 'scatter', mode = 'lines+markers',
              line = list(color = sample(colors(), 1))) %>%
        layout(title = y_label,
               titlefont = list(size = 24), 
               margin = list(t = 100),
               xaxis = list(title = "Date"),
               yaxis = list(title = y_label))
    })
  }
  
  output$humidityPlotDB <- parameterPlot_realtime(parsed_data_realtime()$df_1_realtime, "Hum", "Humidity (%)")
  output$soilMoisturePlotDB <- parameterPlot_realtime(parsed_data_realtime()$df_1_realtime, "SM", "Soil Moisture (%)")
  output$temperaturePlotDB <- parameterPlot_realtime(parsed_data_realtime()$df_1_realtime, "Tem", "Temperature (°C)")
  output$groundWaterLevelPlotDB <- parameterPlot_realtime(parsed_data_realtime()$df_2_realtime, "GWL", "Ground Water Level (m)")
  
  output$windRosePlotDB <- renderPlotly({
    data_realtime <- parsed_data_realtime()$df_2_realtime
    freq_data <- data_realtime %>%
      group_by(WD) %>%
      summarise(freq = n()) %>%
      ungroup()
    
    plot_ly(freq_data, r = ~freq, theta = ~WD, type = 'barpolar',
            marker = list(color = ~freq, cmin = 0, cmax = 20, colorscale = 'Blues'),
            text = ~paste('Frequency:', freq), hoverinfo = 'text+theta') %>%
      layout(title = "Wind Direction Rose Plot",
             titlefont = list(size = 24),
             margin = list(t = 100),
             polar = list(
               radialaxis = list(ticksuffix = ""),
               angularaxis = list(direction = "clockwise",
                                  tickvals = c(0, 45, 90, 135, 180, 225, 270, 315),
                                  ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
                                  type = "categories")
             )
      )
  })
  #------------------------ Dashboard ------------------------#
  
}

