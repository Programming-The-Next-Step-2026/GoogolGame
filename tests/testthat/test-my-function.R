test_that("generate_sequence returns a vector of the correct length", {
  expect_length(generate_sequence(3), 3)
  expect_length(generate_sequence(5), 5)
})

test_that("generate_sequence returns values within range 0 to 100", {
  result <- generate_sequence(50)
  expect_true(all(result >= 0))
  expect_true(all(result <= 100))
})

test_that("generate_sequence returns unique values", {
  result <- generate_sequence(50)
  expect_equal(length(result), length(unique(result)))
})

test_that("get_current_number returns the correct value at a given index", {
  seq <- c(42, 7, 198)
  expect_equal(get_current_number(seq, 1), 42)
  expect_equal(get_current_number(seq, 2), 7)
  expect_equal(get_current_number(seq, 3), 198)
})

test_that("is_winner returns TRUE when pick is the highest number", {
  expect_true(is_winner(198, c(42, 7, 198)))
})

test_that("is_winner returns FALSE when pick is not the highest number", {
  expect_false(is_winner(42, c(42, 7, 198)))
})

test_that("create_manual_sequence returns the input when valid", {
  expect_equal(create_manual_sequence(c(100, 5000, 42)), c(100, 5000, 42))
})

test_that("create_manual_sequence throws error for non-numeric input", {
  expect_error(create_manual_sequence(c("a", "b", "c")))
})

test_that("create_manual_sequence throws error for values below 0", {
  expect_error(create_manual_sequence(c(-1, 500, 1000)))
})

test_that("create_manual_sequence throws error for values above 1000000", {
  expect_error(create_manual_sequence(c(100, 5000, 1000001)))
})

test_that("create_manual_sequence throws error for duplicate values", {
  expect_error(create_manual_sequence(c(100, 100, 42)))
})

test_that("create_manual_sequence throws error for fewer than 2 values", {
  expect_error(create_manual_sequence(c(100)))
})

test_that("get_card_states returns a value for each card in the sequence", {
  expect_length(get_card_states(c(42, 7, 198), 1), 3)
})

test_that("get_card_states marks cards before current_index as passed", {
  states <- get_card_states(c(42, 7, 198), 3)
  expect_equal(states[1], "passed")
  expect_equal(states[2], "passed")
})

test_that("get_card_states marks the current card as current", {
  states <- get_card_states(c(42, 7, 198), 2)
  expect_equal(states[2], "current")
})

test_that("get_card_states marks cards after current_index as hidden", {
  states <- get_card_states(c(42, 7, 198), 1)
  expect_equal(states[2], "hidden")
  expect_equal(states[3], "hidden")
})
