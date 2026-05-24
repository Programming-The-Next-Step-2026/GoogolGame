#' Generate a sequence of random numbers
#'
#' @param n Number of values to draw
#' @return A numeric vector of length n
#' @details Samples n unique integers from 0 to 1000 without replacement.
#' @examples
#' generate_sequence()
#' generate_sequence(n = 5)
#' @export
generate_sequence <- function(n = 3) {
  sample(0:1000, n)
}

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

#' Get the display state of each card
#'
#' @param sequence A numeric vector
#' @param current_index The index of the card currently revealed
#' @return A character vector with one entry per card: "passed", "current",
#' or "hidden"
#' @details Determines the state of each card based on how far the player has
#'   progressed. Cards before the current index are "passed", the current card
#'   is "current", and unrevealed cards are "hidden". In the Shiny UI, each
#'   card needs to render differently depending on its state (i.e. passed cards
#'   greyed out, current card highlighted in blue, hidden cards face-down); this
#'   function provides the state labels to drive that styling.
#' @examples
#' get_card_states(c(42, 7, 198), 1)
#' get_card_states(c(42, 7, 198), 2)
#' @export
get_card_states <- function(sequence, current_index) {
  sapply(seq_along(sequence), function(i) {
    if (i < current_index) "passed"
    else if (i == current_index) "current"
    else "hidden"
  })
}

#' Check if the player picked the highest number
#'
#' @param pick The value the player selected
#' @param sequence The full sequence of numbers
#' @return Logical, TRUE if pick is the maximum
#' @details The player wins only if their chosen number is strictly the largest
#'   in the full sequence, including numbers they never saw.
#' @examples
#' is_winner(198, c(42, 7, 198))
#' is_winner(42, c(42, 7, 198))
#' @export
is_winner <- function(pick, sequence) {
  pick == max(sequence)
}
