library(dplyr)
library(tibble)
library(tidyr)
library(e1071)  # For skewness and kurtosis
library(modeest)  # For mode
library(knitr)
library(kableExtra)

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
