# These are global settings for the TEMPEST data dashboard
# June 2022

library(ggplot2)
theme_set(theme_minimal())
library(dplyr)
library(shiny)
library(DT)
library(readr)
library(lubridate)
library(rdrop2)
library(dygraphs)
library(xts)
library(shinybusy)

if(!require("compasstools")) {
    stop("Need to remotes::install_github('COMPASS-DOE/compasstools')")
}
library(compasstools)


TESTING <- FALSE

TEXT_MSG_USERS <- tribble(
    ~name,     ~number,       ~carrier,
    "SP",      "3016063322",  "Verizon",
    "BBL",     "6086582217",  "T-Mobile",
    "AMP",     "5203491898",  "Verizon",
    "Julia",   "8644205609",  "Verizon"
)

# The server normally accesses the SERC Dropbox to download data
# If we are TESTING, however, skip this and use local test data only
if(!TESTING) {
    datadir <- "TEMPEST_PNNL_Data/Current_Data"
    token <- readRDS("droptoken.rds")
    cursor <- drop_dir(datadir, cursor = TRUE, dtoken = token)
}
last_update <- NA

GRAPH_TIME_WINDOW <- 24   # hours back from present
GRAPH_TIME_INTERVAL <- "15 minutes"  # used by round_date in graphs
FLAG_TIME_WINDOW <- 1         # hours back from present

NO_DATA_GRAPH <- ggplot() +
    annotate("text", x = 1, y = 1, label = "(No data)", size = 12) +
    theme(axis.title = element_blank(),
          axis.text  = element_blank(),
    )