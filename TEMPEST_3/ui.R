#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(lubridate)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(dplyr)

theme_set(theme_minimal())


# Define UI for application that draws a histogram
page_sidebar(
    tags$head(
        includeCSS("www/styles.css"),
        tags$style(HTML(".shiny-spinner-output-container {height: 100%}"))
    ),
        # Include our custom CSS

    # Application title
    title = div(class = "header",
        img(
            src = "COMPASS_logo_FME.png",
            width = 250,
        ),
        "TEMPEST 3 Data Explorer"),

    em(paste("Our resistence shatters, yet we stay resilient. The path to the status-quo we now forge. Never to forget, too soon, it was TEMPEST 3! - Kennedy Doro")),
    sidebar = sidebar(
            h5("Select Data to View"),
            selectInput("type",
                        "Dataset:",
                        c("Soil Properties" = "teros",
                          "Sapflow" = "sapflow",
                          "Groundwater" = "aquatroll",
                          "Redox" = "redox")),
            conditionalPanel(
                condition = "input.type == 'teros'",
                selectInput("var_teros",
                            "Soil Variable:",
                            c("Water Content" = "VWC",
                              "Electrical Conductivity" = "EC",
                              "Temperature" = "TSOIL"))
            ),
            conditionalPanel(
                condition = "input.type == 'aquatroll'",
                selectInput("var_troll",
                            "Groundwater Variable:",
                            c("Temperature" = "Temp",
                              "Pressure" = "Pressure_psi",
                              "Salinity" = "Salinity",
                              "Dissolved Oxygen" = "DO_mgl"))
            )
        ),
    layout_columns(
        fill = FALSE,
        value_box(
            title = "Gallons distributed",
            value = '240,000',
            showcase = bsicons::bs_icon("droplet"),
            theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E")
            ),
        value_box(
            title = "Hours of inundation",
            value = '30',
            showcase = bsicons::bs_icon("clock"),
            theme = "text-success")
        ),
    layout_columns(
        col_widths = c(12, 12),
        row_heights = c(3, 4),
        card(
            full_screen = TRUE,
            card_header("Plots"),
            plotlyOutput("plot", height = "100%") %>% withSpinner(type = 5)
        )#,
        # card(
        #     full_screen = TRUE,
        #     card_header("Maps"),
        #     plotOutput("body_mass")
        # )
    )


)
