# 1. Returns correct numeric values for "number" output
test_that("convert_manual_input returns correct numeric values", {
  expect_equal(
    convert_manual_input(c(2, 4), c("Googol", "Million"), "number"),
    c(2e100, 4e6)
  )
})

# 2. Returns correct display labels for "label" output
test_that("convert_manual_input returns correct display labels", {
  expect_equal(
    convert_manual_input(c(2, 4), c("Googol", "Million"), "label"),
    c("2 Googol", "4 Million")
  )
})

# 3. Returns plain number as string when multiplier is "None"
test_that("convert_manual_input returns plain number when multiplier is None", {
  expect_equal(
    convert_manual_input(c(42), c("None"), "label"),
    c("42")
  )
})

# 4. Returns unnamed numeric vector for "number" output
test_that("convert_manual_input returns unnamed numeric vector", {
  result <- convert_manual_input(c(1, 2), c("Million", "Billion"), "number")
  expect_null(names(result))
})

# 5. Handles all multipliers correctly for "number" output
test_that("convert_manual_input handles all multipliers correctly", {
  expect_equal(
    convert_manual_input(c(1, 1, 1, 1, 1),
                         c("None", "Million", "Billion", "Trillion", "Googol"),
                         "number"),
    c(1, 1e6, 1e9, 1e12, 1e100)
  )
})
