# ExcelStats

ExcelStats is an R package that provides functions to calculate and display descriptive statistics.

## Installation

To install the package, use the following commands in R:

   ```r
   # Install devtools if you haven't already
   install.packages("devtools")

   # Install the package from GitHub
   devtools::install_github("kgse/ExcelStats")
   ```

## Functions

### 1. ExcelStats_df

This function calculates descriptive statistics and returns them as a data frame.

### Usage

   ```r
   library(ExcelStats)
   library(tibble)
   
   # Example data
   set.seed(666)  # For reproducibility
   df <- tibble(x = sample(1:10, 150, replace = TRUE),
                y = sample(18:67, 150, replace = TRUE))
   
   # Calculate descriptive statistics
   stats_df <- df |> 
      ExcelStats_df(x, y, digits = 2)
   
   # Print the data frame
   print(stats_df)
   ```



### 2. ExcelStats_kbl

This function calculates descriptive statistics and returns them as a kable object for easy printing to PDF.

### Usage

   ```r
   library(ExcelStats)
   library(tibble)
   
   # Example data
   set.seed(666)  # For reproducibility
   df <- tibble(x = sample(1:10, 150, replace = TRUE),
                y = sample(18:67, 150, replace = TRUE))
   
   # Calculate descriptive statistics and print as kable (for .pdf)
   stats_kbl <- df |> 
      ExcelStats_kbl(x, y, digits = 2)
   
   # Print the data frame
   print(stats_kable)
   ```

### 3. ExcelStats_cat_df

This function calculates descriptive statistics for a categorical variable and returns them as a data frame.

### Usage

   ```r
   library(ExcelStats)
   library(tibble)
   
   # Example data
   set.seed(666)  # For reproducibility
   df <- tibble(Kjønn = sample(c("Kvinne", "Mann"), 150, replace = TRUE), sofrwrk = rnorm(150))
   
   # Calculate descriptive statistics for categorical variable
   stats_cat_df <- df |> 
      ExcelStats_cat_df(Kjønn, sofrwrk, digits = 2)
   
   # Print the data frame
   print(stats_cat_df)
   ```

### 4. ExcelStats_cat_kbl

This function calculates descriptive statistics for a categorical variable and returns them as a kable object for easy printing to PDF.

### Usage

   ```r
   library(ExcelStats)
   library(tibble)
   
   # Example data
   set.seed(666)  # For reproducibility
   df <- tibble(Kjønn = sample(c("Kvinne", "Mann"), 150, replace = TRUE), sofrwrk = rnorm(150))
   
   # Calculate descriptive statistics for categorical variable and print as kable (for .pdf)
   stats_cat_kbl <- df |> 
      ExcelStats_cat_kbl(Kjønn, sofrwrk, digits = 2)
   
   # Print the kable object
   print(stats_cat_kbl)
   ```

## Conclusion

ExcelStats makes it easy to calculate and display the same descriptive statistics as in Excel in R. 
