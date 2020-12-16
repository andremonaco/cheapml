# Regression Tree Runthrough ----------------------------------------------

# load data simulation tool
# devtools::install_github('andrebleier/Xy')
library(Xy)

# source function
source("algorithms/reg_tree.R")

# simulate data
sim <- Xy(task = "regression") %>%
  # add two linear features
  add_linear(p = 2) %>%
  # add an intercept
  add_intercept() %>%
  # add noise 
  add_noise() %>%
  # simulate the recipe from above
  simulate(n = 1000, r_squared = 0.9)

# simulation overview
sim

# get the formula
model_df <- sim %>% pull_xy()

# get the formula
eq <- sim %>% formula()

# fit
mod <- reg_tree(formula = eq, data = model_df, minsize = 400)

# our tree
mod$tree

# our fitted values
mod$fit