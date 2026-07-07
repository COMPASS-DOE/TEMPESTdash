# Script to process DO sensors

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)

process_do <- function(token, datadir) {

    if(!is.null(getDefaultReactiveDomain())) {
        progress <- incProgress
    } else {
        progress <- NULL
    }

    pattern <- "Pyro\\.dat$"

    compasstools:::process_dir(datadir, pattern, read_datalogger_file, dropbox_token = token) %>%
        pivot_longer(ch1_Status:ch4_PerO2, names_to = c("Channel", "Variable"), names_sep = "_", values_to = "Value") %>%
        separate(Logger, into = c("one", "Logger")) %>%
        mutate(Timestamp = ymd_hms(TIMESTAMP, tz = "EST"),
               Plot = case_when(Logger == "12" ~ "Control",
                                Logger == "21" ~ "Freshwater",
                                Logger == "33" ~ "Saltwater",
                                .default = Logger),
               Depth_cm = case_when(Channel == "ch1" ~ "5",
                                    Channel == "ch2" ~ "15",
                                    Channel == "ch3" ~ "30",
                                    Channel == "ch4" ~ "50",
                                    .default = Channel)) %>%
        filter(Variable %in% c("PerAirSat", "Temp")) %>%
        select(Logger, Plot, Timestamp, Variable, Value, Depth_cm) %>%
        filter(Logger != 13)

}
