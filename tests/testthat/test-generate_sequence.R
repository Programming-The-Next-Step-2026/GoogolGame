# 1. Returns a list with values and labels of the correct length
test_that("generate_sequence returns values and labels of correct length", {
  result <- generate_sequence(3)
  expect_length(result$values, 3)
  expect_length(result$labels, 3)
})

# 2. Returns unique values
test_that("generate_sequence returns unique values", {
  result <- generate_sequence(5)
  expect_equal(length(unique(result$values)), 5)
})

# 3. Returns positive numeric values
test_that("generate_sequence returns positive numeric values", {
  result <- generate_sequence(5)
  expect_true(all(result$values >= 0))
})

# 4. Returns character labels
test_that("generate_sequence returns character labels", {
  result <- generate_sequence(5)
  expect_type(result$labels, "character")
})

# 5. Labels and values are in sync
test_that("generate_sequence labels and values are in sync", {
  result <- generate_sequence(5)
  expect_equal(length(result$values), length(result$labels))
})
