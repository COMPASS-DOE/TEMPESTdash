
tempest_rects <- function(ymin, ymax) {
    list(
        geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                  aes(xmin = as_datetime("2026-06-08 05:00:00", tz = "EST"),
                  xmax = as_datetime("2026-06-08 15:00:00", tz = "EST"),
                  ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE),
        geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                  aes(xmin = as_datetime("2026-06-09 05:00:00", tz = "EST"),
                  xmax = as_datetime("2026-06-09 15:00:00", tz = "EST"),
                  ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE),
        geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                  aes(xmin = as_datetime("2026-06-10 05:00:00", tz = "EST"),
                  xmax = as_datetime("2026-06-10 15:00:00", tz = "EST"),
                  ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE),
        geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                  aes(xmin = as_datetime("2026-06-11 05:00:00", tz = "EST"),
                  xmax = as_datetime("2026-06-11 15:00:00", tz = "EST"),
                  ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE)
    )

}
