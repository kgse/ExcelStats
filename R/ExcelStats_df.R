library(dplyr)
library(tibble)
library(tidyr)
library(e1071)  # For skewness and kurtosis
library(modeest)  # For mode

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

ExcelStats_df <- function(df, variable) {
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

  return(stats)
}
