#' Shuffle a manually entered sequence and its display labels together
#'
#' @param values A numeric vector of sequence values
#' @param labels A character vector of display labels corresponding to values
#' @return A list with elements \code{values} and \code{labels}, both shuffled
#'   in the same random order so they remain in sync.
#' @details Shuffling the sequence before the game starts prevents the player
#'   from predicting the order of numbers entered by a second person.
#' @examples
#' shuffle_manual_sequence(
#' c(100, 2e6, 1e100),
#' c("100", "2 Million", "1 Googol")
#' )
#' @export
shuffle_manual_sequence <- function(values, labels) {
  index <- sample(length(values))
  list(values = values[index], labels = labels[index])
}
