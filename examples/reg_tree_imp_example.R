# Regression Tree Runthrough ----------------------------------------------

# load data simulation tool
# library(devtools)
# devtools::install_github('andrebleier/Xy')
library(Xy)

# source the fitting function
source("reg_tree_imp.R")

# simulate data
sim <- Xy(n = 2500, # 2500 observations
          numvars = c(3, 0), # 2 linear variables / 0 nonlinear
          noisevars = 0, # no noise variables
          catvars = 0, # omit dummy variables
          stn = 5,
          intercept = FALSE
)

# simulation overview
sim

# get the formula
eq <- sim$eq

# get the formula
model_df <- sim$data

# source function
source("algorithms/reg_tree_imp.R")

# fit
mod <- reg_tree_imp(formula = eq, data = model_df, minsize = 100)

# our tree
mod$tree

# the feature importance
mod$imp

# check the feature importance of the simulation
varimp(sim)
