# Test the lexical_sort function from R/example.R
test_that("flag_sensors works", {

    # Basics: calculating fraction in and corresponding badge color
    vals <- 1:5
    # Some in, some out
    x <- flag_sensors(vals, limits = c(2, 4))
    expect_identical(x$n, length(vals))
    expect_identical(x$fraction_in, 0.6)
    expect_true(1- x$fraction_in >= BADGE_COLORS[x$color])
    # All in
    y <- flag_sensors(vals, limits = c(1, 5))
    expect_identical(y$fraction_in, 1.0)
    expect_true(1- y$fraction_in >= BADGE_COLORS[y$color])
    # All out
    z <- flag_sensors(vals, limits = c(6, 6))
    expect_identical(z$fraction_in, 0.0)
    expect_true(1- z$fraction_in >= BADGE_COLORS[z$color])

    # We can count NAs as errors, or not
    vals <- c(1, 2, NA, 4, 5)
    y <- flag_sensors(vals, limits = c(1, 5), na.rm = FALSE)
    expect_identical(y$fraction_in, 0.8)
    y <- flag_sensors(vals, limits = c(1, 5), na.rm = TRUE)
    expect_identical(y$fraction_in, 1.0)

    # No valid data behavior
    bad <- flag_sensors(NA, limits = c(0, 1), na.rm = TRUE)
    expect_true(is.nan(bad$fraction_in))
    expect_identical(bad$percent_in, "--")
    expect_identical(bad$color, "black")
})

test_that("badge_color works", {

    # Should throw an error if first entry in color values is not zero
    expect_error(badge_color(0, badge_colors = c(1, 2)), regexp = "must be zero")

    # Test below green, green boundary, yellow boundary, within yellow,
    # red boundary, within red, max red, beyond red
    bc <- c("green" = 0.0, "yellow" = 0.1, "red" = 0.3)
    vals <- c(-0.05, 0.0, 0.1, 0.2, 0.3, 0.4, 1.0, 1.05)
    x <- badge_color(vals, badge_colors = bc)
    expect_identical(x, c("black", "green", "yellow", "yellow", "red", "red", "red", "black"))

    # Invalid data should get black
    expect_identical(badge_color(NA_real_, bc), "black")
    expect_identical(badge_color(NaN, bc), "black")
})
