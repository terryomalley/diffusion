LogisticGrowthModel <- R6::R6Class(
  "LogisticGrowthModel",
  public = list(
    m = NULL,
    r = NULL,
    t_inf = NULL,
    
    initialize = function(m, r, t_inf) {
      self$m <- m
      self$r <- r
      self$t_inf <- t_inf
    },
    
    simulate = function(t) {
      sapply(t, function(ti) {
        self$m / (1 + exp(-self$r * (ti - self$t_inf)))
      })
    },
    
    fit = function(time, sales) {
      nll <- function(params) {
        m <- params[1]
        r <- params[2]
        t_inf <- params[3]
        pred_sales <- m / (1 + exp(-r * (time - t_inf)))
        -sum(dnorm(sales, mean = pred_sales, sd = 0.1 * mean(sales), log = TRUE))
      }
      fit <- optim(par = c(max(sales), 0.1, median(time)), fn = nll)
      list(m = fit$par[1], r = fit$par[2], t_inf = fit$par[3])
    }
  )
)
