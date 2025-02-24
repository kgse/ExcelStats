#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import e1071
#' @import modeest
#' @import knitr
#' @import kableExtra
#' @import tibble

# Custom function to format numbers
format_number <- function(x) {
  if (is.numeric(x)) {
    return(formatC(x, format = "f", digits = 6, drop0trailing = TRUE))
  }
  return(x)
}

# Custom function to handle mode
get_mode <- function(data) {
  mode_value <- suppressWarnings(mfv(data))
  if (length(mode_value) > 1) {
    return(NA)  # Return NA if there are multiple modes
  }
  return(mode_value)
}
