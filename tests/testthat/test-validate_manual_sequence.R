# 1. Returns the input when valid
test_that("validate_manual_sequence returns the input when valid", {
  expect_equal(validate_manual_sequence(c(100, 5000, 42)), c(100, 5000, 42))
})

# 2. Throws error for non-numeric input
test_that("validate_manual_sequence throws error for non-numeric input", {
  expect_error(validate_manual_sequence(c("a", "b", "c")))
})

# 3. Throws error for values below 0
test_that("validate_manual_sequence throws error for values below 0", {
  expect_error(validate_manual_sequence(c(-1, 500, 1000)))
})

# 4. Throws error for duplicate values
test_that("validate_manual_sequence throws error for duplicate values", {
  expect_error(validate_manual_sequence(c(100, 100, 42)))
})

# 5. Throws error for fewer than 2 values
test_that("validate_manual_sequence throws error for fewer than 2 values", {
  expect_error(validate_manual_sequence(c(100)))
})
