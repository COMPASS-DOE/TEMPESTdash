# Data processing
# Compute badge data, etc. starting from the raw-ish sapflux, TEROS, etc. data

compute_sapflow <- function(sapflow, latest_ts) {

    sapflow %>%
        filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
               Timestamp < latest_ts) ->
        sapflow_filtered

    sapflow_filtered %>%
        summarise(flag_sensors(Value, limits = SAPFLOW_RANGE)) ->
        sapflow_bdg

    sapflow_filtered %>%
        mutate(bad_sensor = which_outside_limits(Value,
                                                 left_limit = SAPFLOW_RANGE[1],
                                                 right_limit = SAPFLOW_RANGE[2])) %>%
        filter(bad_sensor) %>%
        select(Plot, Tree_Code, Logger, Grid_Square, Out_Of_Plot) %>%
        distinct(Tree_Code, Logger, .keep_all = TRUE) ->
        sapflow_bad_sensors

    sapflow %>%
        group_by(Tree_Code) %>%
        slice_tail(n = 10) %>%
        ungroup() %>%
        select(Timestamp, Plot, Tree_Code, Value, Logger, Grid_Square) %>%
        arrange(Timestamp) %>%
        pivot_wider(id_cols = c("Tree_Code", "Plot", "Grid_Square") ,
                    names_from = "Timestamp",
                    values_from = "Value") ->
        sapflow_table_data

    list(sapflow = sapflow,
         sapflow_filtered = sapflow_filtered,
         sapflow_bdg = sapflow_bdg,
         sapflow_table_data = sapflow_table_data,
         sapflow_bad_sensors = sapflow_bad_sensors)
}


compute_teros <- function(teros, latest_ts) {
    # TEROS is awkward, because we only have one badge, but three
    # variables within a single dataset. We compute out-of-limits for each
    # variable, and then combine to a single value and badge color
    teros %>%
        filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
               Timestamp < latest_ts) %>%
        left_join(TEROS_RANGE, by = "variable") ->
        teros_filtered

    teros_filtered %>%
        group_by(variable) %>%
        summarise(flag_sensors(value, limits = c(low[1], high[1]))) %>%
        summarise(fraction_in = weighted.mean(fraction_in, n)) %>%
        mutate(percent_in = paste0(round(fraction_in * 100, 0), "%"),
               color = badge_color(1 - fraction_in)) ->
        teros_bdg

    teros_filtered %>%
        group_by(variable) %>%
        mutate(bad_sensor = which_outside_limits(value,
                                                 left_limit = low[1],
                                                 right_limit = high[1]),
               .keep = "all") %>%
        filter(bad_sensor) %>%
        ungroup() %>%
        select(Plot, ID, variable, Depth, Logger, Grid_Square) %>%
        distinct(ID, Logger, .keep_all = TRUE) ->
        teros_bad_sensors

    list(teros = teros,
         teros_filtered = teros_filtered,
         teros_bad_sensors = teros_bad_sensors,
         teros_bdg = teros_bdg)
}


compute_aquatroll <- function(aquatroll, latest_ts) {
    # Aquatroll is similar: one badge, two datasets
    aquatroll$aquatroll_600 %>%
        select(Timestamp, Logger_ID, Well_Name, Temp) %>%
        mutate(Sensor = 600) -> a600

    aquatroll$aquatroll_200 %>%
        select(Timestamp, Logger_ID, Well_Name, Temp) %>%
        mutate(Sensor = 200) %>%
        bind_rows(a600) %>%
        filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
               Timestamp < latest_ts) ->
        aquatroll_filtered

    aquatroll_filtered %>%
        mutate(bad_sensor = which_outside_limits(Temp,
                                                 left_limit = AQUATROLL_TEMP_RANGE[1],
                                                 right_limit = AQUATROLL_TEMP_RANGE[2])) %>%
        filter(bad_sensor) %>%
        select(Well_Name, Logger_ID) %>%
        arrange(Well_Name, Logger_ID) ->
        aquatroll_bad_sensors

    aquatroll_filtered %>%
        summarise(flag_sensors(Temp, limits = AQUATROLL_TEMP_RANGE)) ->
        aquatroll_bdg

    list(aquatroll_600 = aquatroll$aquatroll_600,
         aquatroll_200 = aquatroll$aquatroll_200,
         aquatroll_filtered = aquatroll_filtered,
         aquatroll_bad_sensors = aquatroll_bad_sensors,
         aquatroll_bdg = aquatroll_bdg)
}


compute_battery <- function(battery, latest_ts) {

    battery %>%
        filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
               Timestamp < latest_ts) ->
        battery_filtered

    battery_filtered %>%
        summarise(flag_sensors(BattV_Avg, limits = VOLTAGE_RANGE)) ->
        battery_bdg

    list(battery = battery,
         battery_bdg = battery_bdg)
}
