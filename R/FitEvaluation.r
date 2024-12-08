evaluate_fit <- function(actual, predicted) {
  residuals <- actual - predicted
  rmse <- sqrt(mean(residuals^2))
  mae <- mean(abs(residuals))
  list(RMSE = rmse, MAE = mae)
}
