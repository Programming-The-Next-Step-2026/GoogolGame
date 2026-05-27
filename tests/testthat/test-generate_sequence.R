# 1. Returns a vector of the correct length
test_that("generate_sequence returns a vector of the correct length", {
  expect_length(generate_sequence(3), 3)
  expect_length(generate_sequence(5), 5)
})

# 2. Returns values within range 0 to 1000
test_that("generate_sequence returns values within range 0 to 1000", {
  result <- generate_sequence(500)
  expect_true(all(result >= 0))
  expect_true(all(result <= 1000))
})

# 3. Returns unique values
test_that("generate_sequence returns unique values", {
  result <- generate_sequence(50)
  expect_equal(length(result), length(unique(result)))
})
