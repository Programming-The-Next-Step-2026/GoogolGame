#' Convert manually entered base numbers and multipliers to numeric values or
#' display labels
#'
#' @param base_numbers A numeric vector of base values entered by the player
#' @param multipliers A character vector of multiplier names (e.g. "Million",
#'   "Googol"). Must be one of "None", "Million", "Billion", "Trillion",
#'   "Googol".
#' @param output Either "number" to return numeric values for game logic, or
#'   "label" to return display labels for the UI (e.g. "2 Googol").
#' @return A numeric vector if output is "number", a character vector if output
#'   is "label".
#' @details Multiplier values: None = 1, Million = 1e6, Billion = 1e9,
#'   Trillion = 1e12, Googol = 1e100.
#' @examples
#' convert_manual_input(c(2, 4), c("Googol", "Million"), "number")
#' convert_manual_input(c(2, 4), c("Googol", "Million"), "label")
#' @export
convert_manual_input <- function(base_numbers, multipliers, output) {
  multiplier_values <- c(
    "None" = 1, "Million" = 1e6, "Billion" = 1e9,
    "Trillion" = 1e12, "Googol" = 1e100
  )
  if (output == "number") {
    # unname() strips the multiplier names carried over from subsetting
    unname(base_numbers * multiplier_values[multipliers])
  } else {
    ifelse(multipliers == "None", as.character(base_numbers),
           paste(base_numbers, multipliers))
  }
}
