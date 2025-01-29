library(dplyr)
library(tibble)
library(tidyr)
library(e1071)  # For skewness and kurtosis
library(modeest)  # For mode
library(knitr)
library(kableExtra)

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

#' Calculate Descriptive Statistics as a Kable
#'
#' This function calculates descriptive statistics for a given variable in a data frame and returns them as a kable object for easy printing.
#'
#' @param df A data frame containing the variable.
#' @param variable The name of the variable to calculate statistics for.
#' @return A kable object with descriptive statistics.
#' @examples
#' set.seed(666)
#' df <- tibble(x = sample(1:10, 150, replace = TRUE))
#' ExcelStats_kbl(df, "x")
#' @export
ExcelStats_kbl <- function(df, variable) {
  data <- df[[variable]]

  stats <- tibble(
    Statistikk = c("Gjennomsnitt", "Standardfeil", "Median", "Modus", "Standardavvik", "Utvalgsvarians",
                   "Kurstosis", "Skjevhet", "Omrade", "Minimum", "Maksimum", "Sum", "Antall", "Konfidenskoeffisient_95"),
    Verdi = c(
      mean(data, na.rm = TRUE),
      sd(data, na.rm = TRUE) / sqrt(length(data)),
      median(data, na.rm = TRUE),
      get_mode(data),
      sd(data, na.rm = TRUE),
      var(data, na.rm = TRUE),
      kurtosis(data, na.rm = TRUE),
      skewness(data, na.rm = TRUE),
      range(data, na.rm = TRUE)[2] - range(data, na.rm = TRUE)[1],
      min(data, na.rm = TRUE),
      max(data, na.rm = TRUE),
      sum(data, na.rm = TRUE),
      length(data),
      qt(0.975, df = length(data) - 1) * (sd(data, na.rm = TRUE) / sqrt(length(data)))
    )
  )

  stats <- stats %>%
    mutate(Verdi = sapply(Verdi, format_number))

  # Convert to kable and add header
  stats_kable <- kable(stats, format = "latex", booktabs = TRUE, linesep = "") %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    add_header_above(setNames(2, variable), align = "c")

  return(stats_kable)
}
