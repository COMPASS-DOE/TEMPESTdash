library(readr)
library(lubridate)
library(dplyr)
library(tidyr)

# This only needs to be done once
teros_inventory <- read_csv("design_doc_copies/TEROS_Network_Location copy.csv",
                            col_types = "cccccddccccd")

process_teros <- function(token, datadir) {

    if(!is.null(getDefaultReactiveDomain())) {
        progress <- incProgress
    } else {
        progress <- NULL
    }
    teros_primitive <- compasstools::process_teros_dir(datadir, tz = "EST",
                                                      dropbox_token = token,
                                                      progress_bar = progress)


    teros_primitive %>%
        left_join(teros_inventory, by = c("Logger" = "Data Logger ID",
                                          "Data_Table_ID" = "Terosdata table channel")) %>%
        select(- `Date of Last Field Check`) %>%
        rename("Active_Date" = "Date Online (2020)",
               "Grid_Square" = "Grid Square") %>%
        filter(!is.na(ID)) ->
        teros

    nomatch <- anti_join(teros, teros_inventory, by = c("Logger" = "Data Logger ID",
                                                            "Data_Table_ID" = "Terosdata table channel"))
    if(nrow(nomatch) > 0) {
        warning("There were logger/channel combinations that I couldn't find in teros_inventory.csv:")
    }

    # Cut the memory footprint of the TEROS data by almost half and return
    select(teros, Timestamp, Plot, ID, Grid_Square, Logger, variable, Depth, value)
}
