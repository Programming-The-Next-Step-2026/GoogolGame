#' Validate and return a sequence of numbers entered by
#' the player in the Shiny app
#'
#' @param numbers A numeric vector of at least 2 values
#' @return A numeric vector
#' @details Player can manually enter numbers. Validates that the input contains
#'   at least 2 unique non-negative numeric values, then returns them as the
#'   game sequence. Uses checkmate package for validation and error messages.
#' @examples
#' validate_manual_sequence(c(100, 5000, 42))
#' validate_manual_sequence(c(0, 1e100, 12345, 678))
#' @importFrom checkmate assert_numeric
#' @export
validate_manual_sequence <- function(numbers) {
  assert_numeric(
    numbers,
    lower = 0,
    any.missing = FALSE,
    min.len = 2,
    unique = TRUE
  )
  numbers
}
