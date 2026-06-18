
tempest_rects <- function(...) {

       geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                  aes(xmin = as_datetime("2024-06-11 05:10:00", tz = "EST"),
                      xmax = as_datetime("2024-06-11 16:26:00", tz = "EST")),
                  ymin = 0, ymax = 1) +
        geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                  aes(xmin = as_datetime("2024-06-12 05:10:00", tz = "EST"),
                      xmax = as_datetime("2024-06-12 16:10:00", tz = "EST")),
                      ymin = 0, ymax = 1) +
        geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                  aes(xmin = as_datetime("2024-06-13 05:10:00", tz = "EST"),
                      xmax = as_datetime("2024-06-13 16:10:00", tz = "EST")),
                      ymin = 0, ymax = 1)


}
