# Test the gear server logic

testServer(gearServer, {
    # Set initial values, as if user entered them
    session$setInputs(event_date = "2023-06-23")
    session$setInputs(event_start = "06:00:00")
    # Simulate a click
    session$setInputs(prog_button = 1)

    # The output is a reactive list with start, stop, and hours entries
    expect_true(is.reactive(gear_reactive))
    expect_is(gear_reactive(), "list")
    expect_is(gear_reactive()$EVENT_START, "POSIXct")
    expect_is(gear_reactive()$EVENT_STOP, "POSIXct")
    expect_identical(gear_reactive()$EVENT_HOURS, EVENT_LENGTH)
})
