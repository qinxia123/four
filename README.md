<!-- badges: start -->
  [![R-CMD-check](https://github.com/qinxia123/four/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qinxia123/four/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->


# Four package

In this package, we create a package to handle linear regression models. 
We use the QR decomposition to create the most basic functionality in the R package and implement the results as an S3 class.
We also implement an object oriented system to handle special functions such as print(), plot(), resid(), pred(), coef() and summary().


## install package

devtools::install_github("qinxia123/four")

## load package

library(Four)

## load data

data(iris)

lin_obj <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

## print out the coefficients and coefficient names

print(lin_obj)

## plot

plot(lin_obj)

## the vector of residuals e_hat

resid(lin_obj)


## the predicted values y_hat

pred(lin_obj)


## the coefficients as a named vector.

coef(lin_obj)

## present the coefficients with their standard error, t-value and p-value as well as the estimate of σ_hat and the degrees of freedom

summary(lin_obj)






