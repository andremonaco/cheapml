# Regression Tree Runthrough ----------------------------------------------

# load data simulation tool
# devtools::install_github('andrebleier/Xy')
library(Xy)

# source function
source("algorithms/reg_tree_imp.R")

# simulate data
sim <- Xy(task = "regression") %>%
  # add a non-linear feature
  add_linear(p = 5) %>%
  # add uninformative features
  add_uninformative(p = 5) %>%
  # add noise to the process
  add_noise() %>%
  # simulate
  simulate(n = 2500, r_squared = 0.95)

# simulation overview
sim

# get the simulation data
model_df <- sim %>% pull_xy()

# get the formula
eq <- sim %>% formula()

# fit
mod <- reg_tree_imp(formula = eq, data = model_df, minsize = 100)

# our tree
mod$tree

# the feature importance
mod$imp

# check the feature importance of the simulation
sim %>% importance()
