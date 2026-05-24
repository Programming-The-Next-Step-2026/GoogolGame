# 1. Returns a value for each card in the sequence
test_that("get_card_states returns a value for each card in the sequence", {
  expect_length(get_card_states(c(42, 7, 198), 1), 3)
})

# 2. Marks cards before current_index as passed
test_that("get_card_states marks cards before current_index as passed", {
  states <- get_card_states(c(42, 7, 198), 3)
  expect_equal(states[1], "passed")
  expect_equal(states[2], "passed")
})

# 3. Marks the current card as current
test_that("get_card_states marks the current card as current", {
  states <- get_card_states(c(42, 7, 198), 2)
  expect_equal(states[2], "current")
})

# 4. Marks cards after current_index as hidden
test_that("get_card_states marks cards after current_index as hidden", {
  states <- get_card_states(c(42, 7, 198), 1)
  expect_equal(states[2], "hidden")
  expect_equal(states[3], "hidden")
})
