# ExcelStats

ExcelStats is an R package that provides functions to calculate and display descriptive statistics.

## Installation

To install the package, use the following commands in R:

   ```r
   # Install devtools if you haven't already
   install.packages("devtools")

   # Install the package from GitHub
   devtools::install_github("kgse/ExcelStat")
   ```

## Functions

### 1. ExcelStats_df

This function calculates descriptive statistics and returns them as a data frame.

### Usage

   ```r
   library(ExcelStats)
   
   # Example data
   set.seed(666)  # For reproducibility
   df <- tibble(x = sample(1:10, 150, replace = TRUE))
   
   # Calculate descriptive statistics
   stats_df <- ExcelStats_df(df, "x")
   
   # Print the data frame
   print(stats_df)
   ```



### 2. ExcelStats_kbl

This function calculates descriptive statistics and returns them as a kable object for easy printing to PDF.

### Usage

   ```r
   library(ExcelStats)
   
   # Example data
   set.seed(666)  # For reproducibility
   df <- tibble(x = sample(1:10, 150, replace = TRUE))
   
   # Calculate descriptive statistics and print as kable (for .pdf)
   stats_kable <- ExcelStats_kbl(df, "x")
   
   # Print the data frame
   print(stats_kable)
   ```


## Conclusion

ExcelStats makes it easy to calculate and display the same descriptive statistics as in Excel in R. 

