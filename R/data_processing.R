# Data processing
# Compute badge data, etc. starting from the raw-ish sapflux, TEROS, etc. data
# BBL June 2023

# Utility function used throughout the code: filter a dataset to a recent
# window (hours) going from `ddt` (dashboard date time)
# This assumes there's a `Timestamp` column in `x`
filter_recent_timestamps <- function(x, window, ddt) {
    stopifnot(window > 0)
    filter(x, Timestamp > ddt - window * 60 * 60,
           Timestamp <= ddt)
}

# All the compute_ functions take the raw data as well as `latest_ts`
# in case we want to be able to look at past data (although this functionality
# doesn't exist yet)

# Compute sapflow data products: badge information, table data, bad sensor info
compute_sapflow <- function(sapflow, ddt) {

    sapflow %>%
        filter_recent_timestamps(FLAG_TIME_WINDOW, ddt) ->
        sapflow_filtered

    sapflow_filtered %>%
        summarise(flag_sensors(Value, limits = SAPFLOW_RANGE)) ->
        sapflow_bdg

    sapflow_filtered %>%
        mutate(bad_sensor = which_outside_limits(Value,
                                                 left_limit = SAPFLOW_RANGE[1],
                                                 right_limit = SAPFLOW_RANGE[2])) %>%
        filter(bad_sensor) %>%
        select(Plot, Sapflow_ID, Logger, Grid_Square, Out_Of_Plot) %>%
        distinct(Sapflow_ID, Logger, .keep_all = TRUE) ->
        sapflow_bad_sensors

    sapflow %>%
        group_by(Sapflow_ID) %>%
        slice_tail(n = 10) %>%
        ungroup() %>%
        select(Timestamp, Plot, Sapflow_ID, Value, Logger, Grid_Square) %>%
        arrange(Timestamp) %>%
        pivot_wider(id_cols = c("Sapflow_ID", "Plot", "Grid_Square") ,
                    names_from = "Timestamp",
                    values_from = "Value") ->
        sapflow_table_data

    list(sapflow = sapflow,
         sapflow_bdg = sapflow_bdg,
         sapflow_table_data = sapflow_table_data,
         sapflow_bad_sensors = sapflow_bad_sensors)
}


# Compute TEROS data products: badge information, table data, bad sensor info
compute_teros <- function(teros, ddt) {
    # TEROS is awkward, because we only have one badge, but three
    # variables within a single dataset. We compute out-of-limits for each
    # variable, and then combine to a single value and badge color

    teros %>%
        filter_recent_timestamps(FLAG_TIME_WINDOW, ddt) %>%
        left_join(TEROS_RANGE, by = "variable") ->
        teros_filtered

    teros_filtered %>%
        group_by(variable) %>%
        summarise(flag_sensors(value, limits = c(low[1], high[1]))) %>%
        summarise(fraction_in = weighted.mean(fraction_in, n)) %>%
        # average the fraction in values
        mutate(percent_in = if_else(all(is.finite(fraction_in)),
                                    paste0(round(fraction_in * 100, 0), "%"),
                                    "--"),
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

    teros_filtered %>%
        # retain only the 10 most recent observations
        arrange(Timestamp) %>%
        group_by(ID, variable) %>%
        slice_tail(n = 10) %>%
        ungroup() %>%
        select(Timestamp, ID, Plot, variable, Grid_Square, Depth, value) %>%
        pivot_wider(id_cols = c("ID", "Plot", "variable", "Grid_Square", "Depth"),
                    names_from = "Timestamp", values_from = "value") ->
        teros_table_data

    list(teros = teros,
         teros_bad_sensors = teros_bad_sensors,
         teros_table_data = teros_table_data,
         teros_bdg = teros_bdg)
}


# Compute Aquatroll data products: badge information, table data, bad sensor info
compute_aquatroll <- function(aquatroll, ddt) {
    # Aquatroll is similar: one badge, two datasets
    aquatroll$aquatroll_600 %>%
        select(Timestamp, Logger_ID, Well_Name, Temp) %>%
        mutate(Sensor = 600) -> a600

    aquatroll$aquatroll_200 %>%
        select(Timestamp, Logger_ID, Well_Name, Temp) %>%
        mutate(Sensor = 200) %>%
        bind_rows(a600) %>%
        filter_recent_timestamps(FLAG_TIME_WINDOW, ddt) ->
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

    # The pivoted data are used repeatedly in the server, so provide too
    aquatroll$aquatroll_200 %>%
        pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity"),
                     names_to = "variable", values_to = "value") ->
        aquatroll_200_long

    aquatroll$aquatroll_600 %>%
        pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity", "DO_mgl"),
                     names_to = "variable", values_to = "value") ->
        aquatroll_600_long

    aquatroll_200_long %>%
        arrange(Timestamp) %>%
        group_by(Well_Name, variable) %>%
        slice_tail(n = 10) %>%
        ungroup() %>%
        select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) ->
        aq200_long

    aquatroll_600_long %>%
        arrange(Timestamp) %>%
        group_by(Well_Name, variable) %>%
        slice_tail(n = 10) %>%
        ungroup() %>%
        select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) %>%
        bind_rows(aq200_long) %>%
        # at this point we have the full trolls dataset in long form
        pivot_wider(id_cols = c("Well_Name", "variable", "Plot", "Instrument"),
                    names_from = "Timestamp", values_from = "value") ->
        aquatroll_table_data

    list(aquatroll_600 = aquatroll$aquatroll_600,
         aquatroll_200 = aquatroll$aquatroll_200,
         aquatroll_600_long = aquatroll_600_long,
         aquatroll_200_long = aquatroll_200_long,
         aquatroll_table_data = aquatroll_table_data,
         aquatroll_bad_sensors = aquatroll_bad_sensors,
         aquatroll_bdg = aquatroll_bdg)
}


# Compute battery data products: badge information
compute_battery <- function(battery, ddt) {

    battery %>%
        filter_recent_timestamps(FLAG_TIME_WINDOW, ddt) ->
        battery_filtered

    battery_filtered %>%
        summarise(flag_sensors(BattV_Avg, limits = VOLTAGE_RANGE)) ->
        battery_bdg

    list(battery = battery,
         battery_bdg = battery_bdg)
}

compute_redox <- function(redox, ddt) {

    redox %>%
        # retain only the 10 most recent observations
        arrange(Timestamp) %>%
        group_by(Depth_cm, Ref, Plot) %>%
        slice_tail(n = 10) %>%
        ungroup() %>%
        pivot_wider(id_cols = c("Plot", "Depth_cm", "Ref"),
                    names_from = "Timestamp", values_from = "Redox") ->
        redox_table_data

    list(redox = redox, redox_table_data = redox_table_data)

}
