# Server code for the TEMPEST data dashboard
# June 2023

source("global.R")

server <- function(input, output, session) {

    dataInvalidate  <- reactiveTimer(15 * 60 * 1000) # 15 minutes
    alertInvalidate <- reactiveTimer(60 * 60 * 1000) # 60 minutes

    # ------------------ Check whether testing --------------------------

    # Note 1. TESTING is defined in the global environment (so "<<-")
    # Note 2. These checks have to be here as shiny.testmode isn't set until server runs
    # Check if we're running in a Shiny testing...
    TESTING <<- TESTING || isTRUE(getOption("shiny.testmode"))
    # ...or continuous integration environment
    TESTING <<- TESTING || Sys.getenv("CI") == "true"


    # ------------------ Read in sensor data -----------------------------

    # The server normally accesses the SERC Dropbox to download data
    # If we are TESTING, however, skip this and use local test data only
    if(!TESTING) {
        datadir <- "TEMPEST_PNNL_Data/Current_Data"
        token <- readRDS("droptoken.rds")
        cursor <- drop_dir(datadir, cursor = TRUE, dtoken = token)
    }

    # DASHBOARD_DATETIME is the datetime that the dashboard is showing
    # Normally this is just now (i.e., Sys.time()), but when testing
    # it will be the latest date of the static testing data
    DASHBOARD_DATETIME <- reactive({
        # Invalidate and re-execute this reactive when timer fires
        dataInvalidate()

        if(TESTING) {
            # Get the latest time in the test data
            battery <- readRDS("offline-data/battery")
            max(battery$Timestamp)
        } else {
            Sys.time()
        }
    })

    # dropbox_data is a list holding all the read-in data along with
    # several secondary products, e.g. the dashboard badge information
    # that's computed from the raw data
    dropbox_data <- reactive({
        # Invalidate and re-execute this reactive when timer fires
        dataInvalidate()

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
        ddt <- isolate({ DASHBOARD_DATETIME() })
        sapflow_list <- compute_sapflow(sapflow, ddt)
        teros_list <- compute_teros(teros, ddt)
        aquatroll_list <- compute_aquatroll(aquatroll, ddt)
        battery_list <- compute_battery(battery, ddt)

        # Return data and badge information
        c(sapflow_list, teros_list, aquatroll_list, battery_list)
    })


    # ------------------ Gear and progress circle --------------------------

    # gearServer is defined in R/gear_module.R
    # We pass it DASHBOARD_DATETIME (a reactive) so it can update
    # its date input field if the datetime changes
    progress <- gearServer("gear", session, DASHBOARD_DATETIME)

    observeEvent({
        input$prog_button
        dataInvalidate() # for actual app, we can have multiple triggers
    }, {
        elapsed <- difftime(reactive({ DASHBOARD_DATETIME() })(),
                            progress()$EVENT_START,
                            units = "hours")
        circleval <- round(as.numeric(elapsed) / progress()$EVENT_HOURS, 2)

        # Don't show a flood progress indicator if too far beyond the end
        if(circleval < 0.0 || circleval > 1.05) circleval <- NA

        update_progress("circle", circleval)
    })


    # ------------------ Main dashboard bad sensor tables --------------------

    output$DDT <- reactive({
        # Ensure that DDT (dashboard datetime) is displayed EST
        paste(format(DASHBOARD_DATETIME(), tz = "EST"), "EST")
    })

    output$sapflow_bad_sensors <- DT::renderDataTable({
        ddt <- reactive({ DASHBOARD_DATETIME() })()

        dropbox_data()[["sapflow"]] %>%
            filter_recent_timestamps(FLAG_TIME_WINDOW, ddt) ->
            sapflow

        vals <- bad_sensors(sapflow, sapflow$Value, "Tree_Code", limits = SAPFLOW_RANGE)

        datatable(vals, options = list(searching = FALSE, pageLength = 5))
    })

    output$teros_bad_sensors <- DT::renderDataTable({
        dropbox_data()[["teros_bad_sensors"]] %>%
            datatable(options = list(searching = FALSE, pageLength = 5))
    })

    output$troll_bad_sensors <- DT::renderDataTable({
        dropbox_data()[["aquatroll_bad_sensors"]] %>%
            datatable(options = list(searching = FALSE, pageLength = 5))
    })

    output$batt_bad_sensors <- DT::renderDataTable({
        ddt <- reactive({ DASHBOARD_DATETIME() })()
        dropbox_data()[["battery"]] %>%
            filter_recent_timestamps(FLAG_TIME_WINDOW, ddt) ->
            battery

        battery[!between(battery$BattV_Avg, min(VOLTAGE_RANGE), max(VOLTAGE_RANGE)), ] %>%
            select(Logger) ->
            bounds

        nas <- battery[is.na(battery$BattV_Avg), ] %>% select(Logger)
        vals <- unique(bind_rows(nas, bounds))

        datatable(vals, options = list(searching = FALSE, pageLength = 5))
    })


    # ------------------ Main dashboard graphs ---------------------------

    # Define a semi-transparent rectangle to indicate flood start/stop
    # We have to use a geom_rect to accommodate the faceted TEROS plot
    # Each plot passes the ymin and ymax (bc plotly won't do -Inf/Inf) to `...`
    shaded_flood_rect <- function(...)
        reactive({
            geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,

                      aes(xmin = progress()$EVENT_START,
                          xmax = progress()$EVENT_STOP, ...))
        })() # remove the reactive before returning

    output$sapflow_plot <- renderPlotly({
        # Average sapflow data by plot and 15 minute interval
        # This graph is shown when users click the "Sapflow" tab on the dashboard
        ddt <- reactive({ DASHBOARD_DATETIME() })()
        dropbox_data()[["sapflow"]] %>%
            filter_recent_timestamps(GRAPH_TIME_WINDOW, ddt) ->
            sapflow

        if(nrow(sapflow)) {
            sapflow %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Plot, Logger, Timestamp_rounded) %>%
                summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, Value, color = Plot, group = Logger)) +
                shaded_flood_rect(ymin = min(SAPFLOW_RANGE), ymax = max(SAPFLOW_RANGE)) +
                geom_line() +
                xlab("") +
                coord_cartesian(xlim = c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) +
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

        ddt <- reactive({ DASHBOARD_DATETIME() })()
        dropbox_data()[["teros"]] %>%
            filter_recent_timestamps(GRAPH_TIME_WINDOW, ddt) ->
            teros

        if(nrow(teros)) {
            teros %>%
                left_join(TEROS_RANGE, by = "variable") %>%
                # Certain versions of plotly seem to have a bug and produce
                # a tidyr::pivot error when there's a 'variable' column; rename
                rename(var = variable) %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Plot, var, Logger, Timestamp_rounded) %>%
                summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
                left_join(TEROS_RANGE, by = c("var" = "variable")) ->
                tdat

            ggplot(tdat) +
                shaded_flood_rect(ymin = low, ymax = high) +
                facet_wrap(~var, scales = "free", ncol = 2) +
                geom_line(aes(Timestamp_rounded, value, color = Plot, group = Logger)) +
                xlab("") +
                coord_cartesian(xlim = c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) +
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

        ddt <- reactive({ DASHBOARD_DATETIME() })()
        bind_rows(dropbox_data()[["aquatroll_200_long"]],
                  dropbox_data()[["aquatroll_600_long"]]) %>%
            filter_recent_timestamps(GRAPH_TIME_WINDOW, ddt) ->
            full_trolls_long

        if(nrow(full_trolls_long) > 1) {
            full_trolls_long %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Logger_ID, Well_Name, Timestamp_rounded, variable) %>%
                summarise(Well_Name = Well_Name,
                          value = mean(value, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, value, color = Well_Name)) +
                coord_cartesian(xlim = c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) +
                shaded_flood_rect(ymin = min(AQUATROLL_TEMP_RANGE), ymax = max(AQUATROLL_TEMP_RANGE)) +
                geom_line() +
                facet_wrap(~variable, scales = "free") +
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
        ddt <- reactive({ DASHBOARD_DATETIME() })()
        dropbox_data()[["battery"]] %>%
            filter_recent_timestamps(GRAPH_TIME_WINDOW, ddt) ->
            battery

        if(nrow(battery)) {
            battery %>%
                ggplot(aes(Timestamp, BattV_Avg, color = as.factor(Logger))) +
                shaded_flood_rect(ymin = min(VOLTAGE_RANGE), ymax = max(VOLTAGE_RANGE)) +
                geom_line() +
                labs(x = "", y = "Battery (V)", color = "Logger") +
                coord_cartesian(xlim = c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) +
                geom_hline(yintercept = VOLTAGE_RANGE, color = "grey", linetype = 2) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })


    # ------------------ Sapflow tab table and graph -----------------------------

    output$sapflow_table <- DT::renderDataTable(datatable({
        dataInvalidate()

        dropbox_data()[["sapflow_table_data"]]
    }))

    output$sapflow_detail_graph <- renderPlotly({

        if(length(input$sapflow_table_rows_selected)) {
            ddt <- reactive({ DASHBOARD_DATETIME() })()
            dropbox_data()[["sapflow_table_data"]] %>%
                slice(input$sapflow_table_rows_selected) %>%
                pull(Tree_Code) ->
                trees_selected

            dropbox_data()[["sapflow"]] %>%
                filter(Tree_Code %in% trees_selected) %>%
                ggplot(aes(Timestamp, Value, group = Tree_Code, color = Plot)) +
                shaded_flood_rect(ymin = min(SAPFLOW_RANGE), ymax = max(SAPFLOW_RANGE)) +
                geom_line() +
                xlab("") +
                xlim(c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) +
                geom_hline(yintercept = SAPFLOW_RANGE, color = "grey", linetype = 2) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })


    # ------------------ TEROS tab table and graph -----------------------------

    output$teros_table <- renderDataTable({
        dataInvalidate()

        dropbox_data()[["teros"]] %>%
            group_by(ID, variable) %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            select(Timestamp, ID, Plot, variable, value, Logger, Grid_Square) %>%
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("ID", "Plot", "variable", "Grid_Square"),
                        names_from = "Timestamp", values_from = "value")
    })

    output$teros_detail_graph <- renderPlotly({

        if(length(input$teros_table_rows_selected)) {
            ddt <- reactive({ DASHBOARD_DATETIME() })()

            dropbox_data()[["teros"]] %>%
                group_by(ID, variable) %>%
                slice_tail(n = 10) %>%
                ungroup() %>%
                select(Timestamp, ID, variable, value, Logger, Grid_Square) %>%
                arrange(Timestamp) %>%
                pivot_wider(id_cols = c("ID", "variable", "Grid_Square"),
                            names_from = "Timestamp", values_from = "value") %>%
                slice(input$teros_table_rows_selected) %>%
                select(variable, ID) ->
                tsensor_selected

            dropbox_data()[["teros"]] %>%
                filter(ID %in% tsensor_selected$ID, variable %in% tsensor_selected$variable) %>%
                ggplot(aes(Timestamp, value, group = interaction(ID, variable), color = ID)) +
                geom_line() +
                xlab("") +
                xlim(c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })


    # ------------------ Aquatroll tab table and graph -----------------------------

    output$troll_table <- DT::renderDataTable({
        dataInvalidate()

        dropbox_data()[["aquatroll_200_long"]] %>%
            group_by(Well_Name, variable) %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) ->
            aq200_long

        dropbox_data()[["aquatroll_600_long"]] %>%
            group_by(Well_Name, variable) %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) %>%
            bind_rows(aq200_long) %>%
            # at this point we have the full trolls dataset in long form
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("Well_Name", "variable", "Plot", "Instrument"),
                        names_from = "Timestamp", values_from = "value")
    })

    output$troll_detail_graph <- renderPlotly({
        if(length(input$troll_table_rows_selected)) {

            ddt <- reactive({ DASHBOARD_DATETIME() })()

            dropbox_data()[["aquatroll_200_long"]] %>%
                group_by(Well_Name, variable) %>%
                slice_tail(n = 10) %>%
                ungroup() %>%
                select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) ->
                aq200_long

            dropbox_data()[["aquatroll_600_long"]] %>%
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
            dropbox_data()[["aquatroll_200_long"]] %>%
                bind_rows(dropbox_data()[["aquatroll_600_long"]]) %>%
                filter(Well_Name %in% aqsensor_selected$Well_Name,
                       variable %in% aqsensor_selected$variable) %>%
                ggplot(aes(Timestamp, value, group = interaction(Well_Name, variable), color = Well_Name)) +
                geom_line() +
                xlab("") +
                labs(color = "Well Name") +
                xlim(c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })


    # ------------------ Battery tab table and graph -----------------------------

    output$btable <- DT::renderDataTable({
        dataInvalidate()

        dropbox_data()[["battery"]] %>%
            select(Timestamp, BattV_Avg, Plot, Logger) %>%
            group_by(Plot, Logger) %>%
            distinct() %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("Plot", "Logger"),
                        names_from = "Timestamp", values_from = "BattV_Avg") %>%
            datatable()
    })

    # ------------------ Maps tab -----------------------------

    # mapsServer is defined in R/maps_module.R
    statusmap <- mapsServer("mapsTab", STATUS_MAP = TRUE,
                            dd = dropbox_data(),
                            ddt = DASHBOARD_DATETIME())
    output$status_map <- renderPlot(statusmap())
    datamap <- mapsServer("mapsTab", STATUS_MAP = FALSE,
                          dd = dropbox_data(),
                          ddt = DASHBOARD_DATETIME())
    output$data_map <- renderPlot(datamap())


    # ------------------ Dashboard badges -----------------------------

    output$sapflow_bdg <- renderValueBox({
        valueBox(dropbox_data()[["sapflow_bdg"]]$percent_in[1],
                 "Sapflow",
                 color = dropbox_data()[["sapflow_bdg"]]$color[1],
                 icon = icon("tree")
        )
    })
    output$teros_bdg <- renderValueBox({
        valueBox(dropbox_data()[["teros_bdg"]]$percent_in[1],
                 "TEROS",
                 color = dropbox_data()[["teros_bdg"]]$color[1],
                 icon = icon("temperature-high")
        )
    })
    output$aquatroll_bdg <- renderValueBox({
        valueBox(dropbox_data()[["aquatroll_bdg"]]$percent_in[1],
                 "AquaTroll",
                 color = dropbox_data()[["aquatroll_bdg"]]$color[1],
                 icon = icon("water")
        )
    })
    output$battery_bdg <- renderValueBox({
        valueBox(dropbox_data()[["battery_bdg"]]$percent_in[1],
                 "Battery",
                 color = dropbox_data()[["battery_bdg"]]$color[1],
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
