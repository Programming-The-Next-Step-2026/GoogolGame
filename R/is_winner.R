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
