## One-off script to work up initial redox data from Roy
##
## Peter Regier edited by Stephanie Pennington 06-10-2024
## 2024-05-10
##
# ########## #
# ########## #

# 1. Setup ---------------------------------------------------------------------

process_dir <- function(datadir, pattern, read_function,
                        dropbox_token = NULL,
                        progress_bar = NULL, ...) {

    local <- is.null(dropbox_token)

    # Get our file list, either locally or in Dropbox
    if(local) {
        s_files <- list.files(datadir, pattern = pattern, full.names = TRUE)
    } else {
        # We don't want users to need rdrop2 to use this package (i.e. we don't
        # want to put it in DESCRIPTION's Imports:), so check for availability
        if(requireNamespace("rdrop2", quietly = TRUE)) {
            # Generate list of 'current' (based on token) files
            s_dir <- rdrop2::drop_dir(datadir, dtoken = dropbox_token)
            s_files <- grep(s_dir$path_display, pattern = pattern, value = TRUE)
        } else {
            stop("rdrop2 package is not available")
        }
    }

    # Function called by lapply below; handles progress bar and calls file reader
    f <- function(filename, read_function, token, total_files) {
        if(!is.null(progress_bar)) progress_bar(1 / total_files)
        # Read file, either locally or from Dropbox
        if(local) {
            read_function(filename, ...)
        } else {
            read_file_dropbox(filename, dropbox_token, read_function, ...)
        }
    }
    x <- lapply(s_files, f, read_function, dropbox_token, length(s_files))
    bind_rows(x)
}

# 2. Read in data --------------------------------------------------------------

process_redox <- function(token, datadir) {

    if(!is.null(getDefaultReactiveDomain())) {
        progress <- incProgress
    } else {
        progress <- NULL
    }

    pattern <- "Redox15\\.dat$"

    process_dir(datadir, pattern, read_datalogger_file, dropbox_token = token) %>%
        clean_names() %>%
        separate(logger, into = c("one", "two", "plot"), sep = "_") %>%
        select(-one, -two) -> df

    # 3. Format data ---------------------------------------------------------------

    set_depths <- function(data){
        data %>%
            pivot_longer(cols = contains("redox_"), names_to = "sensor", values_to = "redox_mv") %>%
            separate(sensor, into = c("scrap", "ref", "sensor"), sep = "_") %>%
            mutate(depth_cm = case_when(sensor == "1" | sensor == "5" | sensor == "9" | sensor == "13" | sensor == "17" ~ 5,
                                        sensor == "2" | sensor == "6" | sensor == "10" | sensor == "14" | sensor == "18" ~ 15,
                                        sensor == "3" | sensor == "7" | sensor == "11" | sensor == "15" | sensor == "19" ~ 30,
                                        sensor == "4" | sensor == "8" | sensor == "12" | sensor == "16" | sensor == "20" ~ 50,
                                        TRUE ~ 0)) %>%
            select(-c(statname, scrap))
    }

    df_raw <- set_depths(df)

    df_raw %>%
        ungroup() %>%
        mutate(Timestamp = lubridate::as_datetime(timestamp, tz = "EST"),
               plot = case_match(plot, "control" ~ "Control",
                                "fresh" ~ "Freshwater",
                                "salt" ~ "Saltwater")) %>%
        group_by(Timestamp, plot, depth_cm, ref) %>%
        summarize(mean_redox = mean(redox_mv, na.rm = T)) %>%
        rename(Plot = plot, Depth_cm = depth_cm, Redox = mean_redox, Ref = ref)

}
