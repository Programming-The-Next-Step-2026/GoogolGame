# 1. Returns TRUE when pick is the highest number
test_that("is_winner returns TRUE when pick is the highest number", {
  expect_true(is_winner(198, c(42, 7, 198)))
})

# 2. Returns FALSE when pick is not the highest number
test_that("is_winner returns FALSE when pick is not the highest number", {
  expect_false(is_winner(42, c(42, 7, 198)))
})

# 3. Correctly distinguishes 1 googol from 2 googol
test_that("is_winner correctly distinguishes 1 googol from 2 googol", {
  expect_false(is_winner(1e100, c(1e100, 2e100)))
  expect_true(is_winner(2e100, c(1e100, 2e100)))
})
