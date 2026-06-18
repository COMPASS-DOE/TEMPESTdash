#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

source("tempest_3_rects.R", local = TRUE)

# Define server logic required to draw a histogram
function(input, output, session) {

    readRDS("tempest_3.rds") -> t3_data

    output$plot <- renderPlotly({

        if (input$type == "teros") {

            if (input$var_teros == "EC") {
                y_label <- "μS/cm"
            } else if (input$var_teros == "TSOIL") {
                y_label <- "degC"
            } else if(input$var_teros == "VWC") {
                y_label <- "m3/m3"
            }

            t3_data[["teros"]] %>%
                filter(variable == input$var_teros) %>%
                ggplot() +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2024-06-11 05:10:00", tz = "EST"),
                              xmax = as_datetime("2024-06-11 16:26:00", tz = "EST"),
                              ymin = min(Value, na.rm = TRUE), ymax = max(Value, na.rm = TRUE))) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2024-06-12 05:30:00", tz = "EST"),
                              xmax = as_datetime("2024-06-12 16:10:00", tz = "EST"),
                              ymin = min(Value, na.rm = TRUE), ymax = max(Value, na.rm = TRUE))) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2024-06-13 05:10:00", tz = "EST"),
                              xmax = as_datetime("2024-06-13 16:10:00", tz = "EST"),
                              ymin = min(Value, na.rm = TRUE), ymax = max(Value, na.rm = TRUE))) +
                facet_wrap(~Plot, scales = "free", ncol = 1) +
                geom_line(aes(Timestamp, Value, color = Depth)) +
                labs(y = y_label,color = "Depth (cm)")

        } else if (input$type == "sapflow") {

            t3_data[["sapflow"]] %>%
                mutate(Species = case_match(Species, "ACRU" ~ "Red Maple", "FAGR" ~ "American Beech", "LITU" ~ "Tulip Poplar", .default = Species)) %>%
                ggplot(aes(Timestamp_rounded, Value, color = Species)) +
                annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                        xmin = as_datetime("2024-06-11 05:10:00", tz = "EST"),
                              xmax = as_datetime("2024-06-11 16:26:00", tz = "EST"),
                          ymin = 0, ymax = 1) +
                annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                          xmin = as_datetime("2024-06-12 05:10:00", tz = "EST"),
                              xmax = as_datetime("2024-06-12 16:10:00", tz = "EST"),
                          ymin = 0, ymax = 1) +
                annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                          xmin = as_datetime("2024-06-13 05:10:00", tz = "EST"),
                              xmax = as_datetime("2024-06-13 16:10:00", tz = "EST"),
                          ymin = 0, ymax = 1) +
                facet_wrap(~Plot, scales = "free", ncol = 1) +
                geom_line() + labs(y = "mV, raw", x = "Timestamp")

        } else if (input$type == "aquatroll") {

            if (input$var_troll == "Pressure_psi") {
                y_label <- "PSI"
            } else if (input$var_troll == "Salinity") {
                y_label <- "PSU"
            } else if(input$var_troll == "DO_mgl") {
                y_label <- "mg/L"
            } else if(input$var_troll == "Temp") {
                y_label <- "degC"
            }

            t3_data[["aquatroll"]] %>%
                filter(variable == input$var_troll) %>%
                ggplot(aes(Timestamp, value, color = Plot, group = Well_Name)) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2024-06-11 05:10:00", tz = "EST"),
                              xmax = as_datetime("2024-06-11 16:26:00", tz = "EST"),
                              ymin = min(value, na.rm = TRUE), ymax = max(value, na.rm = TRUE))) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2024-06-12 05:30:00", tz = "EST"),
                              xmax = as_datetime("2024-06-12 16:10:00", tz = "EST"),
                              ymin = min(value, na.rm = TRUE), ymax = max(value, na.rm = TRUE))) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2024-06-13 05:10:00", tz = "EST"),
                              xmax = as_datetime("2024-06-13 16:10:00", tz = "EST"),
                              ymin = min(value, na.rm = TRUE), ymax = max(value, na.rm = TRUE))) +
                geom_line() + labs(y = y_label)

        } else if (input$type == "redox") {

            t3_data[["redox"]] %>%
                ungroup() %>%
                ggplot(aes(Timestamp, Redox, color = as.factor(Depth_cm), linetype = Ref)) +
                annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                          xmin = as_datetime("2024-06-11 05:10:00", tz = "EST"),
                          xmax = as_datetime("2024-06-11 16:26:00", tz = "EST"),
                         ymin = -110, ymax = 800) +
                annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                          xmin = as_datetime("2024-06-12 05:30:00", tz = "EST"),
                          xmax = as_datetime("2024-06-12 16:10:00", tz = "EST"),
                          ymin = -110, ymax = 800) +
                annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                         xmin = as_datetime("2024-06-13 05:10:00", tz = "EST"),
                         xmax = as_datetime("2024-06-13 16:10:00", tz = "EST"),
                         ymin = -110, ymax = 800) +
                facet_wrap(~Plot, scales = "free", ncol = 1) +
                geom_line() + labs(y = "mV, raw", color = "Depth (cm)")

        }
    })
}
