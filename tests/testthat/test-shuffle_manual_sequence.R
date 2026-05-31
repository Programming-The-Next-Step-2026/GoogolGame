# 1. Returns a list with values and labels
test_that("shuffle_manual_sequence returns a list with values and labels", {
  result <- shuffle_manual_sequence(c(100, 2e6, 1e100), c("100", "2 Million", "1 Googol"))
  expect_type(result, "list")
  expect_named(result, c("values", "labels"))
})

# 2. Returns the same number of elements
test_that("shuffle_manual_sequence preserves length", {
  result <- shuffle_manual_sequence(c(100, 2e6, 1e100), c("100", "2 Million", "1 Googol"))
  expect_length(result$values, 3)
  expect_length(result$labels, 3)
})

# 3. Values and labels remain in sync after shuffling
test_that("shuffle_manual_sequence keeps values and labels in sync", {
  values <- c(100, 2e6, 1e100)
  labels <- c("100", "2 Million", "1 Googol")
  result <- shuffle_manual_sequence(values, labels)
  # Each label should still correspond to its value
  for (i in seq_along(result$values)) {
    original_index <- which(values == result$values[i])
    expect_equal(result$labels[i], labels[original_index])
  }
})

# 4. Contains the same values after shuffling
test_that("shuffle_manual_sequence contains the same values", {
  values <- c(100, 2e6, 1e100)
  result <- shuffle_manual_sequence(values, c("100", "2 Million", "1 Googol"))
  expect_setequal(result$values, values)
})
