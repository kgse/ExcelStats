#' Calculate Descriptive Statistics for a Categorical Variable as a Kable
#'
#' This function calculates descriptive statistics for a categorical variable in a data frame and returns them as a kable object for easy printing.
#'
#' @param df A data frame containing the variables.
#' @param categorical_var The name of the categorical variable.
#' @param value_var The name of the value variable.
#' @return A kable object with descriptive statistics for each category.
#' @examples
#' library(tibble)
#' library(ExcelStats)
#' set.seed(666)
#' df <- tibble(Kjønn = sample(c("Kvinne", "Mann"), 150, replace = TRUE), sofrwrk = rnorm(150))
#' df %>% ExcelStats_cat_kbl(Kjønn, sofrwrk)
#' @export
#' @importFrom stats sd median var qt setNames
ExcelStats_cat_kbl <- function(df, categorical_var, value_var, digits = 2) {
  categorical_var <- enquo(categorical_var)
  value_var <- enquo(value_var)

  data <- df %>%
    group_by(!!categorical_var) %>%
    summarise(
      Gjennomsnitt = mean(!!value_var, na.rm = TRUE),
      Standardfeil = sd(!!value_var, na.rm = TRUE) / sqrt(n()),
      Median = median(!!value_var, na.rm = TRUE),
      Modus = get_mode(!!value_var),
      Standardavvik = sd(!!value_var, na.rm = TRUE),
      Utvalgsvarians = var(!!value_var, na.rm = TRUE),
      Kurstosis = kurtosis(!!value_var, na.rm = TRUE),
      Skjevhet = skewness(!!value_var, na.rm = TRUE),
      Omrade = range(!!value_var, na.rm = TRUE)[2] - range(!!value_var, na.rm = TRUE)[1],
      Minimum = min(!!value_var, na.rm = TRUE),
      Maksimum = max(!!value_var, na.rm = TRUE),
      Sum = sum(!!value_var, na.rm = TRUE),
      Antall = n(),
      Konfidenskoeffisient_95 = qt(0.975, df = n() - 1) * (sd(!!value_var, na.rm = TRUE) / sqrt(n()))
    ) %>%
    pivot_longer(cols = -!!categorical_var, names_to = "Statistikk", values_to = "Verdi") %>%
    pivot_wider(names_from = !!categorical_var, values_from = Verdi)

  data <- data %>%
    mutate(across(where(is.numeric), ~ format_number(.x, digits)))

  # Convert to kable
  stats_kable <- kable(data, format = "latex", booktabs = TRUE, linesep = "") %>%
    kable_styling(latex_options = c("striped", "hold_position"))

  return(stats_kable)
}
