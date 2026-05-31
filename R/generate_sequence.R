#' Generate a sequence of random numbers across different scales
#'
#' @param n Number of values to draw
#' @return A list with elements \code{values} (numeric vector) and
#'   \code{labels} (character vector). Small numbers are shown as plain
#'   integers; numbers from a million onwards are shown with a multiplier
#'   label (e.g. "3 Million", "7 Googol").
#' @details Samples n numbers by randomly selecting from eight ranges:
#'   0-20, 21-100, 101-1000, 1k-100k, 1-100 Million, 1-100 Billion,
#'   1-100 Trillion, and 1-100 Googol. Numbers from million onwards are
#'   expressed as a base number times a named multiplier for readability.
#'   Resamples until all values are unique.
#' @examples
#' generate_sequence()
#' generate_sequence(n = 5)
#' @export
generate_sequence <- function(n = 3) {

  # Small ranges: sample a plain integer and use it directly as the label
  small_ranges <- list(c(0, 20), c(21, 100), c(101, 1000), c(1000, 100000))

  # Large ranges: sample a base number (1-100) and multiply by a named
  # multiplier to produce a human-readable label (e.g. "3 Million")
  large_ranges <- list(
    list(name = "Million",  value = 1e6),
    list(name = "Billion",  value = 1e9),
    list(name = "Trillion", value = 1e12),
    list(name = "Googol",   value = 1e100)
  )

  all_ranges <- c(small_ranges, large_ranges)

  # Sample one number from a given range and return its value and label
  sample_range <- function(range) {
    if (is.list(range)) {
      # Large range: combine base number with multiplier name for the label
      base <- sample(1:100, 1)
      list(value = base * range$value, label = paste(base, range$name))
    } else {
      # Small range: use the sampled integer as both value and label
      val <- sample(range[1]:range[2], 1)
      list(value = val, label = as.character(val))
    }
  }

  # Keep resampling until all n values are unique to avoid duplicate cards
  repeat {
    selected <- sample(length(all_ranges), n, replace = TRUE)
    draws    <- lapply(all_ranges[selected], sample_range)
    values   <- sapply(draws, `[[`, "value")
    labels   <- sapply(draws, `[[`, "label")
    if (length(unique(values)) == n) break
  }

  list(values = values, labels = labels)
}
