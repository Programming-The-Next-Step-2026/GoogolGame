library(shinytest2)

# 1. App loads on setup page
test_that("app loads on setup page", {
  app <- AppDriver$new(play_googol())
  app$wait_for_idle()
  expect_true("start" %in% names(app$get_values()$input))
  app$stop()
})

# # 2. Random mode: Start switches to game page
# test_that("random mode Start switches to game page", {
#   app <- AppDriver$new(play_googol())
#   app$set_inputs(mode = "random", n_cards = 3)
#   app$click("start")
#   app$wait_for_idle()
#   # Reveal first card so the Pick button appears
#   app$click("next_card")
#   app$wait_for_idle()
#   expect_true("pick" %in% names(app$get_values()$input))
#   app$stop()
# })
#
# # 3. Pick switches to result page
# test_that("Pick switches to result page", {
#   app <- AppDriver$new(play_googol())
#   app$set_inputs(mode = "random", n_cards = 3)
#   app$click("start")
#   app$wait_for_idle()
#   app$click("next_card")
#   app$wait_for_idle()
#   app$click("pick")
#   app$wait_for_idle()
#   expect_true("restart" %in% names(app$get_values()$input))
#   app$stop()
# })
#
# # 4. Play again returns to setup
# test_that("Play again returns to setup page", {
#   app <- AppDriver$new(play_googol())
#   app$set_inputs(mode = "random", n_cards = 3)
#   app$click("start")
#   app$wait_for_idle()
#   app$click("next_card")
#   app$wait_for_idle()
#   app$click("pick")
#   app$wait_for_idle()
#   app$click("restart")
#   app$wait_for_idle()
#   expect_true("start" %in% names(app$get_values()$input))
#   app$stop()
# })

# 5. Manual mode with valid numbers starts game
test_that("manual mode with valid numbers starts game", {
  app <- AppDriver$new(play_googol())
  app$set_inputs(mode = "manual", number_1 = 100, multiplier_1 = "None")
  app$click("add_row")
  app$wait_for_idle()
  app$set_inputs(number_2 = 500, multiplier_2 = "None")
  app$click("start")
  app$wait_for_idle()
  # Reveal first card so the Pick button appears
  app$click("next_card")
  app$wait_for_idle()
  expect_true("pick" %in% names(app$get_values()$input))
  app$stop()
})

# 6. Manual mode with invalid entry (i.e. only 1 number) shows an error and stays on setup
test_that("manual mode with invalid entry shows error and stays on setup page", {
  app <- AppDriver$new(play_googol())
  app$set_inputs(mode = "manual", number_1 = 100, multiplier_1 = "None")
  app$click("start")
  app$wait_for_idle()
  expect_true("start" %in% names(app$get_values()$input))
  app$stop()
})
