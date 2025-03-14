# source("R/helpers.R")

#' Calculate Descriptive Statistics for Multiple Variables as a Kable
#'
#' This function calculates descriptive statistics for given variables in a data frame and returns them as a kable object for easy printing.
#'
#' @param df A data frame containing the variables.
#' @param ... Unquoted variable names to calculate statistics for.
#' @return A kable object with descriptive statistics for each variable.
#' @examples
#' set.seed(666)
#' df <- tibble(x = sample(1:10, 150, replace = TRUE), y = sample(18:67, 150, replace = TRUE))
#' df %>% ExcelStats_kbl(x, y)
#' @export
ExcelStats_kbl <- function(df, ...) {
  variables <- enquos(...)

  stats_list <- lapply(variables, function(variable) {
    data <- df %>% pull(!!variable)
    var_name <- as_label(variable)

    tibble(
      Statistikk = c("Gjennomsnitt", "Standardfeil", "Median", "Modus", "Standardavvik", "Utvalgsvarians",
                     "Kurstosis", "Skjevhet", "Omrade", "Minimum", "Maksimum", "Sum", "Antall", "Konfidenskoeffisient_95"),
      !!var_name := c(
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
  })

  stats <- Reduce(function(x, y) full_join(x, y, by = "Statistikk"), stats_list)

  stats <- stats %>%
    mutate(across(-Statistikk, format_number))

  # Convert to kable and add header
  stats_kable <- kable(stats, format = "latex", booktabs = TRUE, linesep = "") %>%
    kable_styling(latex_options = c("striped", "hold_position"))

  return(stats_kable)
}
