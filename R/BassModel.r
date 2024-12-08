BassModel <- R6::R6Class("BassModel",
  public = list(
    m = NULL, # Market potential
    p = NULL, # Innovation parameter
    q = NULL, # Imitation parameter

    initialize = function(m, p, q) {
      self$m <- m
      self$p <- p
      self$q <- q
    },

    simulate = function(time) {
      cum_sales <- numeric(length(time))
      for (t in seq_along(time)) {
        prev_cum <- if (t == 1) 0 else cum_sales[t - 1]
        cum_sales[t] <- (self$p + self$q * prev_cum / self$m) * (self$m - prev_cum)
      }
      return(cum_sales)
    },

    fit = function(time, sales) {
      likelihood <- function(params) {
        p <- params[1]
        q <- params[2]
        m <- max(sales)  # Fixed at observed max
        predicted <- BassModel$new(m, p, q)$simulate(time)
        -sum(dnorm(sales, mean = predicted, sd = 0.1 * mean(sales), log = TRUE))
      }

      fit <- optim(
        par = c(p = 0.01, q = 0.4),
        fn = likelihood,
        method = "L-BFGS-B",
        lower = c(0, 0),
        upper = c(1, 1)
      )

      self$p <- fit$par["p"]
      self$q <- fit$par["q"]
      return(fit)
    }
  )
)
