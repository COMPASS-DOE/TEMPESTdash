#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

source("tempest_4_rects.R", local = TRUE)

# Define server logic required to draw a histogram
function(input, output, session) {

    readRDS("tempest4_data.rds") -> t4_data

    output$plot <- renderPlotly({

        if (input$type == "teros") {

            if (input$var_teros == "EC") {
                y_label <- "μS/cm"
            } else if (input$var_teros == "TSOIL") {
                y_label <- "degC"
            } else if(input$var_teros == "VWC") {
                y_label <- "m3/m3"
            } else if (input$var_teros == "MP") {
                y_label <- "kPa"
            }

            t4_data[["teros"]] %>%
                filter(variable == input$var_teros) -> t

            t %>%
                ggplot() +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-08 05:00:00", tz = "EST"),
                          xmax = as_datetime("2026-06-08 15:00:00", tz = "EST",),
                          ymin = min(t$value, na.rm = TRUE) - 2, ymax = max(t$value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-09 05:00:00", tz = "EST"),
                          xmax = as_datetime("2026-06-09 15:00:00", tz = "EST"),
                          ymin = min(t$value, na.rm = TRUE) - 2, ymax = max(t$value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-10 05:00:00", tz = "EST"),
                          xmax = as_datetime("2026-06-10 15:00:00", tz = "EST"),
                          ymin = min(t$value, na.rm = TRUE) - 2, ymax = max(t$value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-11 05:00:00", tz = "EST"),
                          xmax = as_datetime("2026-06-11 15:00:00", tz = "EST"),
                          ymin = min(t$value, na.rm = TRUE) - 2, ymax = max(t$value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                facet_wrap(~Plot, scales = "free_y", ncol = 1) +
                geom_line(aes(Timestamp, value, color = Depth)) +
                labs(y = y_label, color = "Depth (cm)")

        } else if (input$type == "sapflow") {

            t4_data[["sapflow"]] %>%
                mutate(Species = case_match(Species_code, "ACRU" ~ "Red Maple", "FAGR" ~ "American Beech", "LITU" ~ "Tulip Poplar", .default = Species_code)) -> t

            t %>%
                ggplot() +
                annotate("rect", color = NA, fill = "#BBE7E6", alpha = 0.7,
                         xmin = as_datetime("2026-06-08 05:00:00", tz = "EST"),
                         xmax = as_datetime("2026-06-08 15:00:00", tz = "EST",),
                         ymin = 0, ymax = 1, inherit.aes = FALSE) +
                annotate("rect", color = NA, fill = "#BBE7E6", alpha = 0.7,
                         xmin = as_datetime("2026-06-09 05:00:00", tz = "EST"),
                         xmax = as_datetime("2026-06-09 15:00:00", tz = "EST"),
                         ymin = 0, ymax = 1, inherit.aes = FALSE) +
                annotate("rect", color = NA, fill = "#BBE7E6", alpha = 0.7,
                         xmin = as_datetime("2026-06-10 05:00:00", tz = "EST"),
                         xmax = as_datetime("2026-06-10 15:00:00", tz = "EST"),
                         ymin = 0, ymax = 1, inherit.aes = FALSE) +
                annotate("rect", color = NA, fill = "#BBE7E6", alpha = 0.7,
                         xmin = as_datetime("2026-06-11 05:00:00", tz = "EST"),
                         xmax = as_datetime("2026-06-11 15:00:00", tz = "EST"),
                         ymin = 0, ymax = 1, inherit.aes = FALSE) +
                facet_wrap(~Plot, scales = "free", ncol = 1) +
                geom_line(aes(Timestamp, Value, color = Species)) +
                labs(y = "mV, raw", x = "Timestamp") +
                ylim(0, 1)

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

            t4_data[["aquatroll"]] %>%
                filter(variable == input$var_troll) -> t

            t %>%
                ggplot() +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-08 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-08 15:00:00", tz = "EST",),
                              ymin = min(t$value, na.rm = TRUE) - 2, ymax = max(t$value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-09 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-09 15:00:00", tz = "EST"),
                              ymin = min(t$value, na.rm = TRUE) - 2, ymax = max(t$value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-10 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-10 15:00:00", tz = "EST"),
                              ymin = min(t$value, na.rm = TRUE) - 2, ymax = max(t$value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-11 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-11 15:00:00", tz = "EST"),
                              ymin = min(t$value, na.rm = TRUE) - 2, ymax = max(t$value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_line(aes(Timestamp, value, color = Plot)) +
                labs(y = y_label)

        } else if (input$type == "redox") {

            t4_data[["redox"]] %>%
                ungroup() -> t

            t %>%
                ggplot() +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-08 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-08 15:00:00", tz = "EST",),
                              ymin = min(t$Redox, na.rm = TRUE) - 2, ymax = max(t$Redox, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-09 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-09 15:00:00", tz = "EST"),
                              ymin = min(t$Redox, na.rm = TRUE) - 2, ymax = max(t$Redox, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-10 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-10 15:00:00", tz = "EST"),
                              ymin = min(t$Redox, na.rm = TRUE) - 2, ymax = max(t$Redox, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-11 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-11 15:00:00", tz = "EST"),
                              ymin = min(t$Redox, na.rm = TRUE) - 2, ymax = max(t$Redox, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                facet_wrap(~Plot, scales = "free", ncol = 1) +
                geom_line(aes(Timestamp, Redox, color = as.factor(Depth_cm))) +
                labs(y = "mV, raw", color = "Depth (cm)")

        } else if (input$type == "do") {

            y_label <- "Percent (%)"


            t4_data[["do"]] %>%
                ungroup() -> t

            t %>%
                ggplot() +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-08 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-08 15:00:00", tz = "EST",),
                              ymin = min(t$Value, na.rm = TRUE) - 2, ymax = max(t$Value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-09 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-09 15:00:00", tz = "EST"),
                              ymin = min(t$Value, na.rm = TRUE) - 2, ymax = max(t$Value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-10 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-10 15:00:00", tz = "EST"),
                              ymin = min(t$Value, na.rm = TRUE) - 2, ymax = max(t$Value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                          aes(xmin = as_datetime("2026-06-11 05:00:00", tz = "EST"),
                              xmax = as_datetime("2026-06-11 15:00:00", tz = "EST"),
                              ymin = min(t$Value, na.rm = TRUE) - 2, ymax = max(t$Value, na.rm = TRUE) + 2),
                          inherit.aes = FALSE) +
                facet_wrap(~Plot, scales = "free", ncol = 1) +
                geom_line(aes(Timestamp, Value, color = Depth_cm)) +
                labs(y = y_label)

        }
    })
}
