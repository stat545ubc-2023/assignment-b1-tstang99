    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    library(testthat)

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

    library(datateachr)
    set.seed(2023)

# Exercise 1: Make a Function (25 points)

In this exercise, you’ll be making a function and fortifying it. The
function need not be complicated. The function need not be “serious”,
but shouldn’t be nonsense.

    df_var <- function(df) {
      # Check if df is empty or not
      if(is_empty(df)) {return("Please use a non-empty dataframe.")}
      output <- c()
      for(i in 1:ncol(df)){
        # Verify numeric values in the dataframe 
        if(!is.numeric(unlist(df[,i]))) {
          return("You have a non numeric column, please select a dataframe with all numeric column")
        }
        if(is.numeric(unlist(df[,i]))) {
        output[i] <- var(df[,i])
        }
      }
    return(output)
    }

# Exercise 2: Document your Function (20 points)

In the same code chunk where you made your function, document the
function using roxygen2 tags. Be sure to include:

    #' df_var
    #'
    #' @description A function that calculates the variance for each numeric columns in a dataframe. 
    #' @param df The input dataframe that you want to find the variance of each column. 
    #' @return It returns a vector of variances corresponds to each numeric column in the dataframe. 
    df_var <- function(df) {
      # Check if df is empty or not
      if(is_empty(df)) {return("Please use a non-empty dataframe.")}
      output <- c()
      for(i in 1:ncol(df)){
        # Verify numeric values in the dataframe 
        if(!is.numeric(unlist(df[,i]))) {
          return("You have a non numeric column, please select a dataframe with all numeric column")
        }
        if(is.numeric(unlist(df[,i]))) {
        output[i] <- var(df[,i])
        }
      }
    return(output)
    }

# Exercise 3: Include examples (15 points)

Demonstrate the usage of your function with a few examples. Use one or
more new code chunks, describing what you’re doing.

    # Here we test a empty dataframe
    df <- data.frame()
    df_var(df)

    ## [1] "Please use a non-empty dataframe."

    # Here we test the cancer data from dataeachr package, it contains a non-numeric column.
    df_var(cancer_sample)

    ## [1] "You have a non numeric column, please select a dataframe with all numeric column"

    # Here we remove the non-numeric column from the cancer data above and test the new data.
    new_cancer <- cancer_sample %>%
      select(-diagnosis)
    ex <- df_var(new_cancer)
    ex

    ##  [1] 1.563015e+16 1.241892e+01 1.849891e+01 5.904405e+02 1.238436e+05
    ##  [6] 1.977997e-04 2.789187e-03 6.355248e-03 1.505661e-03 7.515428e-04
    ## [11] 4.984872e-05 7.690235e-02 3.043159e-01 4.087896e+00 2.069432e+03
    ## [16] 9.015114e-06 3.207029e-04 9.111982e-04 3.807242e-05 6.833290e-05
    ## [21] 7.001692e-06 2.336022e+01 3.777648e+01 1.129131e+03 3.241674e+05
    ## [26] 5.213198e-04 2.475477e-02 4.352409e-02 4.320741e-03 3.827584e-03
    ## [31] 3.262094e-04

# Exercise 4: Test the Function (25 points)

Running examples is a good way of checking by-eye whether your function
is working as expected. But, having a formal “yes or no” check is useful
when you move on to other parts of your analysis.

Write formal tests for your function. You should use at least three
non-redundant uses of an `expect_()` function from the `testthat`
package, and they should be contained in a `test_that()` function (or
more than one). They should all pass.

    test_that("Invalid groupby_names", {
      # Should get an error for a non dataframe input
      expect_error(df_var(rnorm(10)))
      # Checking if the answer match
      expect_equal(df_var(new_cancer), ex)
      # Check if the answer is numeric
      expect_true(is.numeric(ex))
      # Check if the answer is correct
      expect_equal(length(df_var(new_cancer)),31)
              })

    ## Test passed 🎊
