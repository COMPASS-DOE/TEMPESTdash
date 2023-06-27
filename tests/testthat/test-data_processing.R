# Test the functions inside R/data_processing.R

test_that("filter_recent_timestamps works", {

    timestamps <- seq.POSIXt(ymd_hms("2023-06-27 06:00:00"),
                             ymd_hms("2023-06-27 18:00:00"),
                             by = "hour")

    x <- data.frame(Timestamp = timestamps)

    # Dashboard datetime (ddt) before first entry
    y <- filter_recent_timestamps(x, window = 3, ddt = min(timestamps) - 1)
    expect_identical(nrow(y), 0L)
    # Dashboard datetime (ddt) exactly at first entry
    y <- filter_recent_timestamps(x, window = 3, ddt = min(timestamps))
    expect_identical(nrow(y), 1L)
    # Dashboard datetime (ddt) exactly at last entry
    y <- filter_recent_timestamps(x, window = 3, ddt = max(timestamps))
    expect_identical(nrow(y), 3L)
    y <- filter_recent_timestamps(x, window = 1, ddt = max(timestamps))
    expect_identical(nrow(y), 1L)
    # Invalid window size
    expect_error(filter_recent_timestamps(x, window = 0), "window > 0 is not TRUE")
})
