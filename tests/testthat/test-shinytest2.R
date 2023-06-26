library(shinytest2)


test_that("{shinytest2} recording: TEMPESTdash", {
  app <- AppDriver$new(name = "TEMPESTdash", seed = 1234, height = 739, width = 1087)
  app$expect_values()
})
