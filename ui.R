# User interface code for the TEMPEST data dashboard
# June 2023

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(shinyWidgets)
library(shinybusy)
library(shinyalert)
library(gmailr)

ui <- dashboardPage(

    skin = if_else(TESTING, "red-light", "black-light"),
    dashboardHeader(
        title = "TEMPEST Dashboard"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("compass")),
            menuItem("Sapflow", tabName = "sapflow", icon = icon("tree")),
            menuItem("TEROS", tabName = "teros", icon = icon("temperature-high")),
            menuItem("AquaTroll", tabName = "troll", icon = icon("water")),
            menuItem("Redox", tabName = "redox", icon = icon("face-smile")),
            menuItem("Battery", tabName = "battery", icon = icon("car-battery")),
            menuItem("Maps", tabName = "maps", icon = icon("map-location-dot")),
            menuItem("Alerts", tabName = "alerts", icon = icon("comment-dots"))
        )
    ),
    dashboardBody(
        tags$head(tags$style(".shiny-notification {position: fixed; top: 30% ;left: 50%; width: 300px")),
        tabItems(
            tabItem(
                tabName = "dashboard",
                fluidRow(
                    # Clearly display the time of the dashboard
                    # If in testing mode, this will be set to the latest timestamp
                    # of the offline data
                    textOutput("DDT"),

                    # Front page badges; their attributes are computed by the server
                    valueBoxOutput("sapflow_bdg", width = 3),
                    valueBoxOutput("teros_bdg", width = 3),
                    valueBoxOutput("aquatroll_bdg", width = 3),
                    valueBoxOutput("battery_bdg", width = 3)
                ),
                fluidRow(
                    # Gear UI is defined in R/gear_module.R
                    column(1, gearUI("gear")),
                    column(5,
                           progress_circle(value = 0, shiny_id = "circle",
                                           color = "#00B0CA", stroke_width = 15,
                                           trail_color = "#BBE7E6"),
                           tags$h3("Flood Progress", align = "center")
                    ),
                    column(width = 6,
                           tabBox(width = 12,
                                  tabPanel(
                                      title = "Sapflow",
                                      dataTableOutput("sapflow_bad_sensors")
                                  ),
                                  tabPanel(
                                      title = "TEROS",
                                      dataTableOutput("teros_bad_sensors")
                                  ),
                                  tabPanel(
                                      title = "AquaTroll",
                                      dataTableOutput("troll_bad_sensors")

                                  ),
                                  tabPanel(
                                      title = "Battery",
                                      dataTableOutput("batt_bad_sensors")
                                  )
                           )

                    )
                ),
                fluidRow(
                    tabBox(width = 12,
                           tabPanel(
                               title = "Sapflow",
                               plotlyOutput("sapflow_plot", height = "400px")
                           ),
                           tabPanel(
                               title = "TEROS",
                               plotlyOutput("teros_plot", height = "400px")
                           ),
                           tabPanel(
                               title = "AquaTroll",
                               plotlyOutput("aquatroll_plot", height = "400px")
                           ),
                           tabPanel(
                               title = "Redox",
                               plotlyOutput("redox_plot", height = "400px")
                           ),
                           tabPanel(
                               title = "Battery",
                               plotlyOutput("battery_plot", height = "400px")
                           )
                    )
                )

            ),
            tabItem(
                tabName = "sapflow",
                DT::dataTableOutput("sapflow_table"),
                plotlyOutput("sapflow_detail_graph")
            ),
            tabItem(
                tabName = "teros",
                DT::dataTableOutput("teros_table"),
                plotlyOutput("teros_detail_graph")
            ),
            tabItem(
                tabName = "troll",
                DT::dataTableOutput("troll_table"),
                plotlyOutput("troll_detail_graph")
            ),
            tabItem(
                tabName = "redox",
                DT::dataTableOutput("redox_table"),
                plotlyOutput("redox_detail_graph")
            ),
            tabItem(
                tabName = "battery",
                dataTableOutput("btable")
            ),
            # Maps tab UI is defined in R/maps_module.R
            mapsUI("mapsTab"),
            # Alerts tab UI is defined in R/alerts_module.R
            alertsUI("alertsTab")
        )
    )
)
