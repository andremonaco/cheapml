# Regression Tree Runthrough ----------------------------------------------

# load data simulation tool
# library(devtools)
# devtools::install_github('andrebleier/Xy')
library(Xy)

# simulate data
sim <- Xy(n = 2500, # 2500 observations
          numvars = c(2, 0), # 2 linear variables / 0 nonlinear
          noisevars = 0, # no noise variables
          catvars = 0, # omit dummy variables
          stn = 5,
          intercept = TRUE
)

# simulation overview
sim

# get the formula
eq <- sim$eq

# get the formula
model_df <- sim$data

mod <- reg_tree(formula = eq, data = model_df, minsize = 100)

# our tree
mod$tree

# our fitted values
mod$fit