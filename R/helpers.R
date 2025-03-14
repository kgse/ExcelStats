#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import e1071
#' @import modeest
#' @import knitr
#' @import kableExtra
#' @import tibble
#' @import rlang

# Custom function to format numbers
format_number <- function(x, digits = 6) {
  if (is.numeric(x)) {
    return(formatC(x, format = "f", digits = digits, drop0trailing = TRUE))
  }
  return(x)
}

# Custom function to handle mode
get_mode <- function(data) {
  mode_value <- suppressWarnings(mfv(data))
  if (length(mode_value) > 1) {
    return(max(mode_value))  # Return the highest mode
  }
  return(mode_value)
}
