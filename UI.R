library(shiny)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Environmental Monitoring Dashboard"),
  dashboardSidebar(
    tags$head(
      tags$style(HTML('
        .sidebar .sidebar-menu .treeview-menu > li > a {
          color: #f9f9f9;
          font-size: 14px;
          padding: 10px 15px;
        }
        .sidebar .sidebar-menu .treeview-menu > li > a:hover {
          background-color: #050C9C;
        }
        .sidebar {
          background-color: #374850;
        }
        .skin-blue .sidebar a {
          color: #b8c7ce;
        }
        .skin-blue .sidebar a:hover {
          text-decoration: none;
        }
      '))
    ),
    sidebarMenu(
      menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
      menuItem("Data Filter", icon = icon("filter"), tabName = "dataFilter"),
      menuItem("About Us", icon = icon("question-circle"), tabName = "about-me")
    ),
    uiOutput("dateSelector")  # Tempat untuk input tanggal
  ),
  dashboardBody(
    tabItems(
tabItem(tabName = "dataFilter",
  fluidRow(
    valueBoxOutput("humidityBox", width = 3),
    valueBoxOutput("soilMoistureBox", width = 3),
    valueBoxOutput("temperatureBox", width = 3),
    valueBoxOutput("groundWaterLevelBox", width = 3),
    valueBoxOutput("windSpeedBox", width = 3),
    valueBoxOutput("timestampBox", width = 6),
    valueBoxOutput("windDirectionBox", width = 3)
  ),
  fluidRow(
    box(status = "primary", solidHeader = TRUE, width = 12,
        tabsetPanel(
          id = "tabs",
          tabPanel("Humidity", plotlyOutput("humidityPlot")),
          tabPanel("Soil Moisture", plotlyOutput("soilMoisturePlot")),
          tabPanel("Temperature", plotlyOutput("temperaturePlot")),
          tabPanel("Wind direction", plotlyOutput("windRosePlot")),
          tabPanel("Ground Water Level", plotlyOutput("groundWaterLevelPlot"))
        )
    )
  )
),
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("humidityBoxDB", width = 3),
                valueBoxOutput("soilMoistureBoxDB", width = 3),
                valueBoxOutput("temperatureBoxDB", width = 3),
                valueBoxOutput("groundWaterLevelBoxDB", width = 3),
                valueBoxOutput("windSpeedBoxDB", width = 3),
                valueBoxOutput("timestampBoxDB", width = 6),
                valueBoxOutput("windDirectionBoxDB", width = 3)
              ),
              fluidRow(
                box(status = "primary", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      id = "tabs",
                      tabPanel("Humidity", plotlyOutput("humidityPlotDB")),
                      tabPanel("Soil Moisture", plotlyOutput("soilMoisturePlotDB")),
                      tabPanel("Temperature", plotlyOutput("temperaturePlotDB")),
                      tabPanel("Wind direction", plotlyOutput("windRosePlotDB")),
                      tabPanel("Ground Water Level", plotlyOutput("groundWaterLevelPlotDB"))
                    )
                )
              )
      ),
      tabItem(tabName = "about-me",
              HTML("Kelompok capstone:<br>
                   -Salma Nadhira Danuningrat       - G6401211065</br> 
                   -Priscilla Nur Elia Putri Gulo   - G6401211045</br>
                   -Lutfiah Nursabiliyanti          - G6401211041</br>
                   -Muhamad Yasir Buana Dyva        - G6401211032</br>
                   -Muhammad Irfan Satriya Dewanto  - G6401211118</br>
                   -Tioninta Mandaika Maha          - G6401211014</br>")
      )
    )
  )
)
