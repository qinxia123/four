<!-- badges: start -->
  [![R-CMD-check](https://github.com/qinxia123/four/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qinxia123/four/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->


#' Linear Regression Function
#'
#' This function performs multiple linear regression using ordinary least squares
#' and returns an object of class 'linreg' containing various statistics.
#'
#' @param formula A formula specifying the model to be fitted (e.g., y ~ x1 + x2).
#' @param data A data frame containing the variables in the model.
#' @return An object of class 'linreg' containing the following statistics:
#'   \item{coefficients}{Estimated coefficients for the regression model.}
#'   \item{fitted_values}{Fitted values from the regression.}
#'   \item{residuals}{Residuals (difference between observed and fitted values).}
#'   \item{degrees_of_freedom}{Degrees of freedom for the regression.}
#'   \item{residual_variance}{Estimated variance of the residuals.}
#'   \item{variance_coefficients}{Variance of the regression coefficients.}
#'   \item{t_values}{t-values for each coefficient.}
#'   \item{p_values}{p-values for each coefficient.}

#install
devtools::install_github("qinxia123/four")

#example
library(Four)

# load data
data(iris)
lin_obj <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

### print out the coefficients and coefficient names

print(lin_obj)

### ggplot2

plot(lin_obj)

### the vector of residuals eˆ

resid(lin_obj)


###  the predicted values yˆ

pred(lin_obj)


### the coefficients as a named vector.

coef(lin_obj)

### present the coefficients with their standard error, t-value and p-value as well as the estimate of σˆ and the degrees of freedom

summary(lin_obj)






