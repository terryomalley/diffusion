source("R/BassModel.r")
source("R/LogisticGrowthModel.r")
source("R/FitEvaluation.r")

# Simulate data using the Bass model
bass_model <- BassModel$new(m = 5000, p = 0.003, q = 0.05)
time <- 1:12
sales <- bass_model$simulate(time)

# Fit using both models
logistic_model <- LogisticGrowthModel$new(5000, 0.2, 6)
bass_fit <- bass_model$fit(time, sales)
logistic_fit <- logistic_model$fit(time, sales)

# Generate predictions
bass_pred <- numeric(length(time))
cum_sales <- 0


for (i in seq_along(time)) {
  bass_pred[i] <- (bass_fit$p + bass_fit$q * cum_sales / bass_fit$m) * (bass_fit$m - cum_sales)
  cum_sales <- cum_sales + bass_pred[i]
}
logistic_pred <- logistic_fit$m / (1 + exp(-logistic_fit$r * (time - logistic_fit$t_inf)))

# Evaluate fit
bass_eval <- evaluate_fit(sales, bass_pred)
logistic_eval <- evaluate_fit(sales, logistic_pred)

# Print results
print("Bass Model Fit:")
print(bass_eval)
print("Logistic Growth Model Fit:")
print(logistic_eval)

# Plot results
library(ggplot2)
df <- data.frame(
  Time = time,
  Actual = sales,
  Bass = bass_pred,
  Logistic = logistic_pred
)
ggplot(df, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Bass, color = "Bass")) +
  geom_line(aes(y = Logistic, color = "Logistic")) +
  labs(title = "Fitting Bass-Generated Data", y = "Sales") +
  scale_color_manual(values = c("Actual" = "black", "Bass" = "blue", "Logistic" = "red"))
