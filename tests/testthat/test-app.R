library(shinytest2)

# 1. App loads on setup page
test_that("app loads on setup page", {
  app <- AppDriver$new(run_app())
  app$wait_for_idle()
  expect_true("start" %in% names(app$get_values()$input))
  app$stop()
})

# 2. Random mode: Start switches to game page
test_that("random mode Start switches to game page", {
  app <- AppDriver$new(run_app())
  app$set_inputs(mode = "random", n_cards = 3)
  app$click("start")
  app$wait_for_idle()
  expect_true("pick" %in% names(app$get_values()$input))
  app$stop()
})

# 3. Pick switches to result page
test_that("Pick switches to result page", {
  app <- AppDriver$new(run_app())
  app$set_inputs(mode = "random", n_cards = 3)
  app$click("start")
  app$wait_for_idle()
  app$click("pick")
  app$wait_for_idle()
  expect_true("restart" %in% names(app$get_values()$input))
  app$stop()
})

# 4. Play again returns to setup
test_that("Play again returns to setup page", {
  app <- AppDriver$new(run_app())
  app$set_inputs(mode = "random", n_cards = 3)
  app$click("start")
  app$wait_for_idle()
  app$click("pick")
  app$wait_for_idle()
  app$click("restart")
  app$wait_for_idle()
  expect_true("start" %in% names(app$get_values()$input))
  app$stop()
})

# 5. Manual mode with valid numbers starts game
test_that("manual mode with valid numbers starts game", {
  app <- AppDriver$new(run_app())
  app$set_inputs(mode = "manual", number_1 = 100, multiplier_1 = 1)
  app$click("add_row")
  app$wait_for_idle()
  app$set_inputs(number_2 = 500, multiplier_2 = 1)
  app$click("start")
  app$wait_for_idle()
  expect_true("pick" %in% names(app$get_values()$input))
  app$stop()
})

# 6. Manual mode with only 1 number shows an error and stays on setup
test_that("manual mode with 1 number shows error and stays on setup page", {
  app <- AppDriver$new(run_app())
  app$set_inputs(mode = "manual", number_1 = 100, multiplier_1 = 1)
  app$click("start")
  app$wait_for_idle()
  expect_true("start" %in% names(app$get_values()$input))
  app$stop()
})
