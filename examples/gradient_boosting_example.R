# Gradient Boosting Runthrough --------------------------------------------

# load data simulation tool
# library(devtools)
# devtools::install_github('andrebleier/Xy')
library(Xy)

# simulate data
sim <- Xy(n = 1000, # 1000 observations
         numvars = c(2, 0), # 2 linear variables / 0 nonlinear
         noisevars = 0, # no noise variables
         catvars = 0, # omit dummy variables
         stn = 30, # signal to noise ratio 30:1
         intercept = TRUE
         )

# simulation overview
sim

# get the formula
eq <- sim$eq

# get the formula
model_df <- sim$data

# source the function
source("algorithms/grad_boost.R")

# fit
mod <- grad_boost(formula = eq, data = model_df,
                  nu = 0.1, stop = 100,
                  grad.fun = gradient,
                  loss.fun = loss)

# our estimator
mod$theta

# the true underlying effect
coef(sim)