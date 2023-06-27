library(shinytest2)


test_that("{shinytest2} recording: TEMPESTdash", {
  # Some UI elements (in particular, dropdownButton) seem to have some
  # randomness. Need to set a seed here so that everything is reproducible
  app <- AppDriver$new(name = "TEMPESTdash", seed = 1234, height = 739, width = 1087)
  app$expect_values()
})
