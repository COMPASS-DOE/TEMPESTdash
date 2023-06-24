# This is the server of the TEMPEST data dashboard
# June 2023

source("global.R")

server <- function(input, output) {

    autoInvalidate <- reactiveTimer(15 * 60 * 1000)
    alertInvalidate <- reactiveTimer(60 * 60 * 1000)

    # ------------------ Read in sensor data -----------------------------

    dropbox_data <- reactive({

        # Invalidate and re-execute this reactive expression every time the
        # timer fires
        autoInvalidate()

        if(TESTING) {
            sapflow <- readRDS("offline-data/sapflow")
            teros <- readRDS("offline-data/teros")
            aquatroll <- readRDS("offline-data/aquatroll")
            battery <- readRDS("offline-data/battery")
        } else {
            sapflow <- withProgress(process_sapflow(token, datadir), message = "Updating sapflow...")
            teros <- withProgress(process_teros(token, datadir), message = "Updating TEROS...")
            atroll <- withProgress(process_aquatroll(token, datadir), message = "Updating AquaTroll...")
            aquatroll <- list(
                aquatroll_600 = filter(atroll, Instrument == "TROLL600"),
                aquatroll_200 = filter(atroll, Instrument == "TROLL200")
            )
            sapflow %>%
                select(Timestamp, BattV_Avg, Plot, Logger) %>%
                group_by(Plot, Logger, Timestamp) %>%
                summarise(BattV_Avg = mean(BattV_Avg), .groups = "drop") ->
                battery
        }

        # Do limits testing and compute data needed for badges
        # compute_sapflow() etc. are defined in R/data_processing.R
        latest_ts <- with_tz(Sys.time(), tzone = "EST")
        sapflow_list <- compute_sapflow(sapflow, latest_ts)
        teros_list <- compute_teros(teros, latest_ts)
        aquatroll_list <- compute_aquatroll(aquatroll, latest_ts)
        battery_list <- compute_battery(battery, latest_ts)

        # Return data and badge information
        c(sapflow_list, teros_list, aquatroll_list, battery_list)
    })

    # ------------------ Gear and progress circle --------------------------

    # gearServer is defined in R/gear_module.R
    progress <- gearServer("gear")

    observeEvent({
        input$prog_button
        autoInvalidate() # for actual app, we can have multiple triggers
    }, {
        elapsed <- difftime(with_tz(Sys.time(), tzone = "EST"),
                            progress()$EVENT_START,
                            units = "hours")
        circleval <- round(as.numeric(elapsed) / progress()$EVENT_HOURS, 2)

        # Don't show a flood progress indicator if too far beyond the end
        if(circleval < 0.0 || circleval > 1.05) circleval <- NA

        update_progress("circle", circleval)
    })

    # ------------------ Main dashboard bad sensor tables --------------------

    output$sapflow_bad_sensors <- DT::renderDataTable({

        dropbox_data()$sapflow %>%
            filter_recent_timestamps(FLAG_TIME_WINDOW) ->
            sapflow

        vals <- bad_sensors(sapflow, sapflow$Value, "Tree_Code", limits = SAPFLOW_RANGE)

        datatable(vals, options = list(searching = FALSE, pageLength = 5))
    })

    output$teros_bad_sensors <- DT::renderDataTable({
        dropbox_data()$teros_bad_sensors %>%
            datatable(options = list(searching = FALSE, pageLength = 5))
    })

    output$troll_bad_sensors <- DT::renderDataTable({
        dropbox_data()$aquatroll_bad_sensors %>%
            datatable(options = list(searching = FALSE, pageLength = 5))
    })

    output$batt_bad_sensors <- DT::renderDataTable({
        dropbox_data()$battery %>%
            filter_recent_timestamps(FLAG_TIME_WINDOW) ->
            battery

        battery[!between(battery$BattV_Avg, min(VOLTAGE_RANGE), max(VOLTAGE_RANGE)), ] %>%
            select(Logger) ->
            bounds

        nas <- battery[is.na(battery$BattV_Avg), ] %>% select(Logger)
        vals <- unique(bind_rows(nas, bounds))

        datatable(vals, options = list(searching = FALSE, pageLength = 5))
    })


    # ------------------ Main dashboard graphs ---------------------------

    output$sapflow_plot <- renderPlotly({
        # Average sapflow data by plot and 15 minute interval
        # This graph is shown when users click the "Sapflow" tab on the dashboard

        sapflow <- dropbox_data()$sapflow

        if(nrow(sapflow)) {
            latest_ts <- with_tz(Sys.time(), tzone = "EST")

            sapflow %>%
                filter_recent_timestamps(GRAPH_TIME_WINDOW) %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Plot, Logger, Timestamp_rounded) %>%
                summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, Value, color = Plot, group = Logger)) +
                geom_rect(aes(xmin = progress()$EVENT_START, xmax = progress()$EVENT_STOP,
                              ymin = min(SAPFLOW_RANGE), ymax = max(SAPFLOW_RANGE)),
                          fill = "#BBE7E6",
                          alpha = 0.7,
                          col = "#BBE7E6") +
                geom_line() +
                xlab("") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                geom_hline(yintercept = SAPFLOW_RANGE, color = "grey", linetype = 2)  ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })

    output$teros_plot <- renderPlotly({
        # Average TEROS data by plot and 15 minute interval,
        # one facet per sensor (temperature, moisture, conductivity)
        # This graph is shown when users click the "TEROS" tab on the dashboard

        teros <- dropbox_data()$teros

        if(nrow(teros) > 1) {
            latest_ts <- with_tz(Sys.time(), tzone = "EST")
            teros %>%
                left_join(TEROS_RANGE, by = "variable") %>%
                # Certain versions of plotly seem to have a bug and produce
                # a tidyr::pivot error when there's a 'variable' column; rename
                rename(var = variable) %>%
                filter_recent_timestamps(GRAPH_TIME_WINDOW) %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Plot, var, Logger, Timestamp_rounded) %>%
                summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
                left_join(TEROS_RANGE, by = c("var" = "variable")) -> tdat

            ggplot(tdat) +
                geom_rect(group = 1,
                          aes(xmin = progress()$EVENT_START, xmax = progress()$EVENT_STOP,
                              ymin = low, ymax = high), fill = "#BBE7E6", alpha = 0.7, col = "#BBE7E6") +
                facet_wrap(~var, scales = "free", ncol = 2) +
                geom_line(aes(Timestamp_rounded, value, color = Plot, group = Logger)) +
                xlab("") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                geom_hline(aes(yintercept = low), color = "grey", linetype = 2) +
                geom_hline(aes(yintercept = high), color = "grey", linetype = 2) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }

        plotly::ggplotly(b)
    })

    output$aquatroll_plot <- renderPlotly({
        # AquaTroll data plot
        # TODO: this currently just shows temperature; we may want to have multiple
        # variables, in which case the badge status computation would be like
        # that of TEROS
        # This graph is shown when users click the "Battery" tab on the dashboard

        full_trolls_long <- bind_rows(dropbox_data()$aquatroll_200_long,
                                      dropbox_data()$aquatroll_600_long)

        if(nrow(full_trolls_long) > 1) {
            latest_ts <- with_tz(Sys.time(), tzone = "EST")

            full_trolls_long %>%
                filter_recent_timestamps(GRAPH_TIME_WINDOW) %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Logger_ID, Well_Name, Timestamp_rounded, variable) %>%
                summarise(Well_Name = Well_Name,
                          value = mean(value, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, value, color = Well_Name)) +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                         xmin = progress()$EVENT_START, xmax = progress()$EVENT_STOP,
                         ymin = min(AQUATROLL_TEMP_RANGE), ymax = max(AQUATROLL_TEMP_RANGE)) +
                geom_line() + facet_wrap(~variable, scales = "free") +
                xlab("") ->
                b

        } else {
            b <- NO_DATA_GRAPH
        }

        plotly::ggplotly(b)
    })

    output$battery_plot <- renderPlotly({
        # Battery voltages, from the sapflow data
        # This graph is shown when users click the "Battery" tab on the dashboard
        battery <- dropbox_data()$battery

        if(nrow(battery)) {
            latest_ts <- with_tz(Sys.time(), tzone = "EST")
            battery %>%
                filter_recent_timestamps(GRAPH_TIME_WINDOW) %>%
                ggplot(aes(Timestamp, BattV_Avg, color = as.factor(Logger))) +
                annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                         xmin = progress()$EVENT_START, xmax = progress()$EVENT_STOP,
                         ymin = min(VOLTAGE_RANGE), ymax = max(VOLTAGE_RANGE)) +
                geom_line() +
                labs(x = "", y = "Battery (V)", color = "Logger") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                geom_hline(yintercept = VOLTAGE_RANGE, color = "grey", linetype = 2) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }

        plotly::ggplotly(b)
    })


    # ------------------ Sapflow tab table and graph -----------------------------

    output$sapflow_table <- DT::renderDataTable(datatable({
        autoInvalidate()

        dropbox_data()$sapflow_table_data
    }))

    output$sapflow_detail_graph <- renderPlotly({

        if(length(input$sapflow_table_rows_selected)) {
            latest_ts <- with_tz(Sys.time(), tzone = "EST")

            dropbox_data()$sapflow_table_data %>%
                slice(input$sapflow_table_rows_selected) %>%
                pull(Tree_Code) ->
                trees_selected

            dropbox_data()$sapflow %>%
                filter(Tree_Code %in% trees_selected) %>%
                ggplot(aes(Timestamp, Value, group = Tree_Code, color = Plot)) +
                geom_rect(aes(xmin = progress()$EVENT_START, xmax = progress()$EVENT_STOP,
                              ymin = min(SAPFLOW_RANGE), ymax = max(SAPFLOW_RANGE)),
                          fill = "#BBE7E6",
                          alpha = 0.7,
                          col = "#BBE7E6") +
                geom_line() +
                xlab("") +
                xlim(c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                geom_hline(yintercept = SAPFLOW_RANGE, color = "grey", linetype = 2) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })


    # ------------------ TEROS tab table and graph -----------------------------

    output$teros_table <- renderDataTable({
        autoInvalidate()

        dropbox_data()$teros %>%
            group_by(ID, variable) %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            select(Timestamp, ID, Plot, variable, value, Logger, Grid_Square) %>%
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("ID", "Plot", "variable", "Grid_Square"), names_from = "Timestamp", values_from = "value")
        #}
    })

    output$teros_detail_graph <- renderPlotly({

        if(length(input$teros_table_rows_selected)) {
            latest_ts <- with_tz(Sys.time(), tzone = "EST")

            dropbox_data()$teros %>%
                group_by(ID, variable) %>%
                slice_tail(n = 10) %>%
                ungroup() %>%
                select(Timestamp, ID, variable, value, Logger, Grid_Square) %>%
                arrange(Timestamp) %>%
                pivot_wider(id_cols = c("ID", "variable", "Grid_Square"), names_from = "Timestamp", values_from = "value") %>%
                slice(input$teros_table_rows_selected) %>%
                select(variable, ID) ->
                tsensor_selected

            dropbox_data()$teros %>%
                filter(ID %in% tsensor_selected$ID, variable %in% tsensor_selected$variable) %>%
                ggplot(aes(Timestamp, value, group = interaction(ID, variable), color = ID)) +
                geom_line() +
                xlab("") +
                xlim(c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })


    # ------------------ Aquatroll tab table and graph -----------------------------

    output$troll_table <- DT::renderDataTable({
        autoInvalidate()

        dropbox_data()$aquatroll_200_long %>%
            group_by(Well_Name, variable) %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) ->
            aq200_long

        dropbox_data()$aquatroll_600_long %>%
            group_by(Well_Name, variable) %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) %>%
            bind_rows(aq200_long) %>%
            # at this point we have the full trolls dataset in long form
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("Well_Name", "variable", "Plot", "Instrument"), names_from = "Timestamp", values_from = "value")
    })

    output$troll_detail_graph <- renderPlotly({
        if(length(input$troll_table_rows_selected)) {

            latest_ts <- with_tz(Sys.time(), tzone = "EST")

            dropbox_data()$aquatroll_200_long %>%
                group_by(Well_Name, variable) %>%
                slice_tail(n = 10) %>%
                ungroup() %>%
                select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) ->
                aq200_long

            dropbox_data()$aquatroll_600_long %>%
                group_by(Well_Name, variable) %>%
                slice_tail(n = 10) %>%
                ungroup() %>%
                select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) %>%
                bind_rows(aq200_long) -> trolls

            trolls %>%
                pivot_wider(id_cols = c("Well_Name", "variable", "Plot", "Instrument"),
                            names_from = "Timestamp", values_from = "value") %>%
                slice(input$troll_table_rows_selected) %>%
                select(variable, Well_Name) ->
                aqsensor_selected

            # Get the full long-form trolls data, filter for what is selected
            # in the table, and plot
            dropbox_data()$aquatroll_200_long %>%
                bind_rows(dropbox_data()$aquatroll_600_long) %>%
                filter(Well_Name %in% aqsensor_selected$Well_Name,
                       variable %in% aqsensor_selected$variable) %>%
                ggplot(aes(Timestamp, value, group = interaction(Well_Name, variable), color = Well_Name)) +
                geom_line() +
                xlab("") +
                labs(color = "Well Name") +
                xlim(c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })


    # ------------------ Battery tab table and graph -----------------------------

    output$btable <- DT::renderDataTable({
        autoInvalidate()

        dropbox_data()$battery %>%
            select(Timestamp, BattV_Avg, Plot, Logger) %>%
            group_by(Plot, Logger) %>%
            distinct() %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("Plot", "Logger"), names_from = "Timestamp", values_from = "BattV_Avg") %>%
            datatable()
    })

    # ------------------ Maps tab -----------------------------

    # mapsServer is defined in R/maps_module.R
    statusmap <- mapsServer("mapsTab", STATUS_MAP = TRUE, dd = dropbox_data())
    output$status_map <- renderPlot(statusmap())
    datamap <- mapsServer("mapsTab", STATUS_MAP = FALSE, dd = dropbox_data())
    output$data_map <- renderPlot(datamap())


    # ------------------ Dashboard badges -----------------------------

    output$sapflow_bdg <- renderValueBox({
        valueBox(dropbox_data()$sapflow_bdg$percent_in[1],
                 "Sapflow",
                 color = dropbox_data()$sapflow_bdg$color[1],
                 icon = icon("tree")
        )
    })
    output$teros_bdg <- renderValueBox({
        valueBox(dropbox_data()$teros_bdg$percent_in[1],
                 "TEROS",
                 color = dropbox_data()$teros_bdg$color[1],
                 icon = icon("temperature-high")
        )
    })
    output$aquatroll_bdg <- renderValueBox({
        valueBox(dropbox_data()$aquatroll_bdg$percent_in[1],
                 "AquaTroll",
                 color = dropbox_data()$aquatroll_bdg$color[1],
                 icon = icon("water")
        )
    })
    output$battery_bdg <- renderValueBox({
        valueBox(dropbox_data()$battery_bdg$percent_in[1],
                 "Battery",
                 color = dropbox_data()$battery_bdg$color[1],
                 icon = icon("car-battery")
        )
    })


    # ------------------ Text alerts -----------------------------

    observeEvent({

        # This will calculate values and send out messages to everyone in "new_user" df
        # could just have people not choose what they want alerts for?
        #initial_alert()
        alertInvalidate()
    }, {
        # send_alerts is defined in R/alerts_module.R
        send_alerts(dropbox_data)
    })

}
