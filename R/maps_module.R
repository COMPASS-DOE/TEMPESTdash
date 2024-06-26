# Maps module
# BBL June 2023

# The module’s UI function
# The first argument to a UI function should always be `id`.
# This is the namespace for the module.
# https://shiny.posit.co/r/articles/improve/modules/
mapsUI <- function(id) {
    # `NS(id)` returns a namespace function
    ns <- NS(id)

    tabItem(
        tabName = "maps",
        selectInput(ns("plot_name"),
                    "Plot:",
                    choices = c("Control", "Freshwater", "Saltwater", "Shoreline"),
                    selected = "Control"),
        radioGroupButtons(ns("map_items"),
                          label = "Data to show:",
                          choices = c("Sapflow status" = "map_sapflow",
                                      "TEROS status" = "map_teros"),
                          selected = "map_teros"),
        checkboxGroupInput(ns("map_overlays"),
                           label = "Overlay:",
                           choices = c("Compass rose" = "map_rose", "Trees" = "map_trees"),
                           selected = "map_rose",
                           inline = TRUE),
        plotOutput("status_map", height = "600px"),
        selectInput(ns("data_map_variable"),
                    "TEROS variable:",
                    choices = c("TSOIL", "VWC", "EC"),
                    selected = "VWC"),
        selectInput(ns("teros_depth"),
                    "TEROS depth:",
                    choices = c("All", "5", "15", "30"),
                    selected = "All"),
        plotOutput("data_map", height = "600px")
    )
}


# Module server function - a fragment of server logic
# Parameters: id, STATUS_MAP (a flag), and dd (dashboard data, outside of the maps module namespace)
# see https://shiny.posit.co/r/articles/improve/modules/
mapsServer <- function(id, STATUS_MAP, dd, ddt) {
    moduleServer(
        id,
        ## Below is the module function
        function(input, output, session) {
            # Get the reactive inputs
            plot_name <- reactive({
                input$plot_name
            })
            map_overlays <- reactive({
                input$map_overlays
            })
            map_items <- reactive({
                input$map_items
            })
            data_map_variable <- reactive({
                input$data_map_variable
            })
            teros_depth <- reactive({
                input$teros_depth
            })

            # Generate the plot and return it
            plt <- reactive({
                make_plot_map(STATUS_MAP,
                              # inputs from the UI
                              data_map_variable = data_map_variable(),
                              teros_depth = teros_depth(),
                              plot_name = plot_name(),
                              map_overlays = map_overlays(),
                              map_items = map_items(),
                              ddt = ddt,
                              # inputs from the Dropbox data
                              sapflow_data = dd$sapflow,
                              sapflow_bad_sensors = dd$sapflow_bad_sensors,
                              teros_data = dd$teros,
                              teros_bad_sensors = dd$teros_bad_sensors)
            })
            return(plt)

        })
}


# Below here is the code from the old maps.R file

library(tibble)

# The TEMPEST plots are all oriented slightly differently, and have different
# layouts in terms of how the A-J and 1-8 axes are set up, orientation wrt
# magnetic north, and color of stakes:
plot_info <- tribble(
    ~plot,        ~lower_left, ~upper_left, ~north_degrees, ~color,
    "Control",    "A8",        "A1",         -50,           "green",
    "Freshwater", "A8",        "A1",         -20,           "blue",
    "Saltwater",  "J1",        "A1",         -25,           "red"
)

# Tree data - read it only once
readr::read_csv("design-doc-copies/inventory copy.csv",
                col_types = "ccdcdDccdDcDdcDdcDdclcc") %>%
    filter(In_Plot, Status_2023 %in% c("LI", "DS")) %>%
    select(Plot, Grid, Species_code, Tag, DBH_2023) %>%
    mutate(x = substr(Grid, 1, 1), y = substr(Grid, 2, 2)) ->
    map_tree_data

# Mapping from sapflow to trees
readr::read_csv("design-doc-copies/sapflow_inventory copy.csv",
                col_types = "ccdcdddclc") %>%
    rename(Sapflow_ID = Tree_Code) %>%
    select(Sapflow_ID, Tag) ->
    sapflow_inv

library(cowplot)

# Do the compass rose transparency and rotation calculations (plot-specific)
# once and store, IF magick is available
if(require(magick)) {
    rose_dat <- magick::image_read("map-data/compass-rose.png")
    roses <- list()
    for(i in seq_len(nrow(plot_info))) {
        img <- magick::image_rotate(rose_dat,
                                    degrees = plot_info$north_degrees[i]
        )
        roses[[plot_info$plot[i]]] <- magick::image_transparent(img, color = "white")
    }
}

# Main plotting function
make_plot_map <- function(STATUS_MAP,
                          # inputs from the UI
                          data_map_variable,
                          teros_depth,
                          plot_name,
                          map_overlays,
                          map_items,
                          ddt, # dashboard datetime
                          # inputs from the Dropbox data
                          sapflow_data,
                          sapflow_bad_sensors,
                          teros_data,
                          teros_bad_sensors) {

    show_rose <- "map_rose" %in% map_overlays
    show_trees <- "map_trees" %in% map_overlays
    show_teros <- "map_teros" %in% map_items
    show_sapflow <- "map_sapflow" %in% map_items

    # Construct plotting grid, flipping things around as needed
    plot_dat <- expand.grid(plot = plot_name,
                            x = as.factor(LETTERS[1:10]),
                            y = as.factor(1:8))

    pinfo <- plot_info[plot_info$plot == plot_name,]

    # If A is not lower left, reverse x factor levels
    if(substr(pinfo$lower_left, 1, 1) != "A") {
        plot_dat$x <- factor(plot_dat$x, levels = rev(levels(plot_dat$x)))
    }
    # If 1 is not lower left, reverse y factor levels
    if(substr(pinfo$lower_left, 2, 2) != "1") {
        plot_dat$y <- factor(plot_dat$y, levels = rev(levels(plot_dat$y)))
    }

    # Set up initial plot
    library(ggplot2)
    p <- ggplot(plot_dat, aes(x, y)) + ggtitle(plot_name) + geom_point(color = "white") +
        theme_bw() +
        theme(axis.title = element_blank(),
              axis.text = element_text(face = "bold", size = 16),
              plot.title = element_text(face = "bold", size = 16))

    # If same letter not in lower left and upper left, flip axes
    if(substr(pinfo$lower_left, 1, 1) != substr(pinfo$upper_left, 1, 1)) {
        p <- p + coord_flip()
    }

    # Overlays

    if(show_rose && exists("roses")) {
        # Draw into plot
        p <- p + cowplot::draw_image(roses[[plot_name]], x = 5, y = 4, scale = 8) # centered, easy
    }

    inv <- filter(map_tree_data, Plot == plot_name)
    if(show_trees) {
        # Trees
        p <- p + geom_point(data = inv,
                            na.rm = TRUE,
                            position = position_jitter(seed = 1234),
                            aes(size = DBH_2023), pch = 1) +
            guides(size = "none")
    }

    # Data layers

    # ------ TEROS status map ------

    if(show_teros && STATUS_MAP) {
        # Filter the sensors and bad sensors for the current plot and visualize
        teros_bad_sensors %>%
            filter(Plot == plot_name) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            tbs
        teros_data %>%
            distinct(Plot, ID, Grid_Square) %>%
            filter(Plot == plot_name, !ID %in% tbs$ID) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            td

        p <- p + geom_text(data = td,
                           na.rm = TRUE,
                           position = position_jitter(seed = 1234),
                           aes(label = ID), color = "green")

        # We draw bad sensors second, so they're on top of the good sensors
        p <- p + geom_label(data = tbs,
                            na.rm = TRUE,
                            position = position_jitter(seed = 1234),
                            aes(label = ID), color = "red", fontface = "bold")
    }

    # ------ TEROS values map ------

    if(show_teros && !STATUS_MAP) {
        # If user has selected a particular depth, filter to that
        if(teros_depth != "All") {
            teros_data <- filter(teros_data, Depth == as.numeric(teros_depth))
            teros_bad_sensors <- filter(teros_bad_sensors,
                                        Depth == as.numeric(teros_depth))
        }

        teros_data %>%
            # Isolate plot and variable user wants, and then all data in last hour
            filter(Plot == plot_name, variable == data_map_variable) %>%
            filter_recent_timestamps(window = 1, ddt) %>%
            # For each sensor, get its last timestamp of data
            group_by(ID) %>%
            filter(Timestamp == max(Timestamp)) %>%
            select(Plot, ID, Logger, Grid_Square, variable, value) ->
            td_with_data
        teros_bad_sensors %>%
            # Bad sensors: isolate to plot and variable...
            filter(Plot == plot_name, variable == data_map_variable,
                   # ...and IDs not in our good sensor list
                   !ID %in% td_with_data$ID) %>%
            bind_rows(td_with_data) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            td

        p <- p + ggtitle(paste(plot_name, strftime(ddt, '%F %T', usetz = TRUE)))
        p <- p + geom_point(data = td,
                            na.rm = TRUE,
                            position = position_jitter(seed = 1234),
                            aes(color = value, shape = variable), size = 6) +
            facet_wrap(~variable)
    }

    # ------ Sapflow status map ------

    if(show_sapflow && STATUS_MAP) {
        # Trees
        inv <- filter(map_tree_data, Plot == plot_name)

        # Note to self: ideally we would merge the sapflow data with tree inventory
        # data, so as to plot trees at the same time. But I'm not sure where the
        # sapflow ID -> tree tag mapping is

        sapflow_bad_sensors %>%
            filter(Plot == plot_name) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            sbs
        sbs$Sapflow_ID[sbs$Out_Of_Plot] <- paste(sbs$Sapflow_ID[sbs$Out_Of_Plot], "\n[OUT]")

        sapflow_data %>%
            distinct(Plot, Sapflow_ID, Grid_Square, Out_Of_Plot) %>%
            filter(Plot == plot_name, !Sapflow_ID %in% sbs$Sapflow_ID) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            sdat
        sdat$Sapflow_ID[sdat$Out_Of_Plot] <- paste(sdat$Sapflow_ID[sdat$Out_Of_Plot], "\n[OUT]")

        p <- p + geom_text(data = sdat,
                           na.rm = TRUE,
                           position = position_jitter(seed = 1234),
                           aes(label = Sapflow_ID), color = "green")

        # We draw bad sensors second, so they're on top of the good sensors
        p <- p + geom_label(data = sbs,
                            na.rm = TRUE,
                            position = position_jitter(seed = 1234),
                            aes(label = Sapflow_ID), color = "red", fontface = "bold")

    }

    # ------ Sapflow values map ------

    if(show_sapflow && !STATUS_MAP) {

        sapflow_data %>%
            # Isolate plot and variable user wants, and then all data in last hour
            filter(Plot == plot_name) %>%
            filter(Timestamp >= ddt - 1 * 60 * 60) %>%
            # For each sensor, get its last timestamp of data
            group_by(Sapflow_ID) %>%
            filter(Timestamp == max(Timestamp)) %>%
            filter(is.finite(Value)) %>%
            select(Plot, Sapflow_ID, Logger, Grid_Square, Value) ->
            sd_with_data
        sapflow_bad_sensors %>%
            # Bad sensors: isolate to plot and IDs not in our good sensor list
            filter(Plot == plot_name, !Sapflow_ID %in% sd_with_data$Sapflow_ID) %>%
            bind_rows(sd_with_data) %>%
            mutate(x = substr(Grid_Square, 1, 1), y = substr(Grid_Square, 2, 2)) ->
            sd_all

        # ...and join with tree-code-to-tag mapping, and then to inventory data
        # inv %>%
        #     left_join(sapflow_inv, by = "Tag") %>%
        #     left_join(sdat, by = "Sapflow_ID") ->
        #     sdat
        p <- p + ggtitle(paste(plot_name, strftime(ddt, '%F %T', usetz = TRUE)))
        p <- p + geom_point(data = sd_all,
                            na.rm = TRUE,
                            position = position_jitter(seed = 1234),
                            aes(color = Value), size = 6)
    }

    p
}
