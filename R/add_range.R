
# Function to add range selector to plotly
# Created 2024-06-07 | Stephanie Pennington

add_range <- function(p) {
    p %>%
        layout(
            xaxis = list(
                range = c(Sys.Date() - 1, Sys.Date()),
                type = "date",
                rangeselector = list(
                    buttons = list(
                        list(
                            count = 1,
                            label = "Today",
                            step = "day",
                            stepmode = "todate"),
                        list(step = "all"))),
                rangeslider = list(type = "date")))
}
