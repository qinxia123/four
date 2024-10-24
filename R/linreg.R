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
#' @export
#' @importFrom stats median model.matrix sd
linreg <- function(formula, data) {

  if(!is.data.frame(data)){
    stop()
  }

  if(!inherits(formula, "formula")){
    stop()
  }

  # Create the design matrix X
  X <- model.matrix(formula, data)

  # Pick out the dependent variable y
  y_var <- all.vars(formula)[1]
  y <- data[[y_var]]

  # Perform QR decomposition of X
  qr_decomp <- qr(X)
  Q <- qr.Q(qr_decomp)
  R <- qr.R(qr_decomp)

  # Calculate the regression coefficients
  beta_hat <- solve(R) %*% t(Q) %*% y
  names(beta_hat) <- colnames(X)

  # Calculate the fitted values
  y_hat <- X %*% beta_hat

  # Calculate the residuals
  e_hat <- y - y_hat

  # Calculate degrees of freedom
  n <- nrow(X)  # Sample size
  p <- ncol(X)  # Number of parameters
  df <- n - p   # Degrees of freedom

  # Calculate the residual variance
  sigma_hat_squared <- sum(e_hat^2) / df

  # Calculate the variance of the regression coefficients
  R_inv <- solve(R)
  var_beta <- sigma_hat_squared * R_inv %*% t(R_inv)

  # Calculate the t-values for each coefficient
  t_values <- beta_hat / sqrt(diag(var_beta))

  # Calculate the p-values for each coefficient
  p_values <- 2 * stats::pt(abs(t_values), df = df, lower.tail = FALSE)

  # Create a list to store results
  result <- list(
    coefficients = beta_hat,
    fitted_values = y_hat,
    residuals = e_hat,
    degrees_of_freedom = df,
    residual_variance = sigma_hat_squared,
    variance_coefficients = var_beta,
    t_values = t_values,
    p_values = p_values,
    actual_values = y,
    call = match.call()
  )
  # Set the class of the result to 'linreg'
  class(result) <- "linreg"

  return(result)
}

utils::globalVariables(c("Fitted", "Residuals", "Std_Residuals"))

# S3 print method for linreg class
#' Print method for linreg objects
#'
#' This method prints the coefficients and their names for linreg objects.
#'
#' @param x A linreg object.
#' @param ... Additional arguments (not used).
#' @export
print.linreg <- function(x, ...) {
  cat("Call:\n")
  cat(deparse(x$call), "\n")  # Output the model's call information
  cat("\n")
  cat("Coefficients:\n")

  # Format the coefficients for output

  for(i in seq_along(x$coefficients)){
    cat(sprintf("%20s", names(x$coefficients)[i]))
  }
  cat("\n")
  for(j in seq_along(x$coefficients)){
    cat(sprintf("%20.2f", x$coefficients[j]))
  }
}

plot <- function(x,...) {
  UseMethod("plot")
}
#' Plot method for linreg objects
#'
#' This method plots the diagnostic plots for linreg objects.
#'
#' @param x A linreg object.
#' @param ... Additional arguments (not used).
#' @export
plot.linreg <- function(x, ...) {
  # Extract data
  y_hat <- x$fitted_values
  e_hat <- x$residuals


  # Create a data frame for plotting
  plot_data <- data.frame(Fitted = y_hat, Residuals = e_hat)

  # Calculate the median of residuals
  median_residuals <- median(e_hat)

  # Residuals vs Fitted
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Fitted, y = Residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = median_residuals, linetype = "dashed", color = "blue") +
    ggplot2::geom_line(color = "red", linewidth = 0.5) +
    ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
    ggplot2::theme_bw()

  # Scale-Location plot (sqrt of standardized residuals)
  standardized_residuals <- e_hat / sd(e_hat)
  plot_data$Std_Residuals <- sqrt(abs(standardized_residuals))

  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Fitted, y = Std_Residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(color = "red", linewidth = 0.5)  +
    ggplot2::labs(title = "Scale-Location", x = "Fitted values", y = expression(sqrt(abs(Standardized~Residuals)))) +
    ggplot2::theme_bw()


  # Combine plots
  gridExtra::grid.arrange(p1, p2, ncol = 1)
}


resid <- function(x,...) {
  UseMethod("resid")
}
#' Residuals method for linreg objects
#'
#' This method returns the residuals from a linreg object.
#'
#' @param x A linreg object.
#' @param ... Additional arguments (not used).
#' @return A vector of residuals.
#' @export
resid.linreg <- function(x,...) {
  return(x$residuals)
}

pred <- function(x,...) {
  UseMethod("pred", x)
}

#' Predicted values method for linreg objects
#'
#' This method returns the predicted values from a linreg object.
#'
#' @param x A linreg object.
#' @param ... Additional arguments (not used).
#' @return A vector of predicted values.
#' @export
pred.linreg <- function(x,...) {
  return(x$fitted_values)
}

#' Coefficients method for linreg objects
#'
#' This method returns the coefficients from a linreg object.
#'
#' @param x A linreg object.
#' @param ... Additional arguments (not used).
#' @return A named vector of coefficients.
#' @export
coef.linreg <- function(x,...) {
  return(x$coefficients)
}

summary <- function(x,...) {
  UseMethod("summary")
}
#' Summary method for linreg objects
#'
#' This method returns a summary of the linreg object, including coefficients,
#' standard errors, t-values, p-values, and estimate of σˆ.
#'
#' @param x A linreg object.
#' @param ... Additional arguments (not used).
#' @return A summary of the linreg object.
#' @export
summary.linreg <- function(x,...) {
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")

  # Extract coefficients and their statistics
  coefs <- x$coefficients
  se <- sqrt(diag(x$variance_coefficients))
  t_values <- x$t_values
  p_values <- x$p_values

  # Add significance stars based on p-values
  signif_codes <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*", "")))

  # Create a data frame for better formatting
  summary_table <- data.frame(
    Estimate = coefs,
    `Std. Error` = se,
    `t value` = t_values,
    `Pr(>|t|)` = p_values,
    Signif = signif_codes,
    check.names = FALSE  # To handle spaces in column names
  )

  # Print table with appropriate alignment
  print(format(summary_table, digits = 5, justify = "right"), row.names = TRUE)

  # Combine Residual standard error and Degrees of freedom on one line
  cat(sprintf("\nResidual standard error: %.5f on %d degrees of freedom\n",
              sqrt(x$residual_variance), x$degrees_of_freedom))
}
