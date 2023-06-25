# Script to read raw sapflow data and process

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)

# This only needs to be done once
sf_inventory <- read_csv("design-doc-copies/sapflow_inventory copy.csv", col_types = "ccdcdddclc")

process_sapflow <- function(token, datadir) {

    if(!is.null(getDefaultReactiveDomain())) {
        progress <- incProgress
    } else {
        progress <- NULL
    }

    sf_raw <- compasstools::process_sapflow_dir(datadir, tz = "EST",
                                                token, progress)
    sf_raw %>%
        left_join(sf_inventory, by = c("Logger", "Port")) %>%
        filter(!is.na(Tree_Code)) %>% # remove ports that don't have any sensors
        select(Plot, Timestamp, Record, BattV_Avg, Port, Value, Logger, Tree_Code, Grid_Square, Out_Of_Plot, Species, Installation_Date) %>%
        mutate(Deep_Sensor = grepl("D", Tree_Code),
               Grid_Letter = substring(Grid_Square, 1, 1),
               Grid_Number = substring(Grid_Square, 2, 2)) -> sapflow

    nomatch_ports <- anti_join(sf_raw, sf_inventory, by = c("Logger", "Port"))

    if(nrow(nomatch_ports) > 0) {
        warning("There were logger/port combinations that I couldn't find in sapflow_inventory.csv:")
    }

    # Cut the memory footprint of the sapflow data by almost half and return
    select(sapflow, Plot, Timestamp, Value, Tree_Code, Logger, BattV_Avg, Out_Of_Plot, Species, Grid_Square)
}
