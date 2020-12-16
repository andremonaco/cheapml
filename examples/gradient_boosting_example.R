# Gradient Boosting Runthrough --------------------------------------------

# load data simulation tool
# devtools::install_github('andrebleier/Xy')
library(Xy)

# simulate data
sim <- Xy(task = "regression") %>%
        add_linear(p = 2) %>%
        add_intercept() %>%
        add_noise() %>%
        simulate(n = 1000, r_squared = 0.9)

# simulation overview
sim

# get the formula
model_df <- sim %>% pull_xy()

# get the formula
eq <- sim %>% formula()
  
# source the function
source("algorithms/gradient_boosting.R")

# fit
mod <- grad_boost(formula = eq, data = model_df,
                  nu = 0.1, stop = 100,
                  grad.fun = gradient,
                  loss.fun = loss)

# our estimator
mod$theta

# the true underlying effect
sim %>% coef()
