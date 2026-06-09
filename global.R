# These are global settings for the TEMPEST data dashboard
# June 2023

library(ggplot2)
theme_set(theme_minimal())
library(dplyr)
library(shiny)
library(DT)
library(readr)
library(lubridate)
library(rdrop2refreshtoken)
library(shinybusy)
library(plotly)
library(janitor)

if(!require("compasstools")) {
    stop("Need to devtools::install_github('COMPASS-DOE/compasstools@bypass-dropdir')")
}
library(compasstools)

# The TESTING_STATE flag causes the server to load static data in offline-data/
# When writing new code or debugging, it's often useful to set this to TRUE
# so as not to spend time downloading from Dropbox
TESTING <- FALSE

LOCAL <- TRUE

# Flooding event length (hours)
EVENT_LENGTH <- 10

TEXT_MSG_USERS <- tribble(
    ~name,     ~number,       ~carrier,
    "SP",      "3016063322",  "Verizon",
    "BBL",     "6086582217",  "T-Mobile",
    "AMP",     "5203491898",  "Verizon",
    "Julia",   "8644205609",  "Verizon"
)

GRAPH_TIME_WINDOW <- 24   # hours back from the dashboard datetime
GRAPH_TIME_INTERVAL <- "15 minutes"  # used by round_date in graphs
FLAG_TIME_WINDOW <- 1         # hours back from the dashboard datetime

# The 'no data' graph that's shown if no rows are selected, etc.
NO_DATA_GRAPH <- ggplot() +
    annotate("text", x = 1, y = 1, label = "(No selection)", size = 12) +
    theme(axis.title = element_blank(),
          axis.text  = element_blank(),
    )

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
        if(requireNamespace("rdrop2refreshtoken", quietly = TRUE)) {
            # Generate list of 'current' (based on token) files
            s_dir <- rdrop2refreshtoken::drop_dir(datadir, dtoken = dropbox_token)
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
