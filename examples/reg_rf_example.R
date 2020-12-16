# Regression Random Forest Runthrough ----------------------------------------------

# load data simulation tool
# devtools::install_github('andrebleier/Xy')
library(Xy)
library(dplyr)
library(ggplot2)
source("algorithms/reg_rf.R")
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

# show feature importance
imp_true <- sim %>% importance(plot=FALSE) %>% 
  dplyr::select(effect, mean) %>%
  dplyr::filter(effect != "e") %>%
  dplyr::rename("IMPORTANCE" = "mean",
                "FEATURES" = "effect") %>%
  mutate(TYPE = 'Truth')

# fit regression tree
mod_rt <- reg_tree_imp(formula = eq, data = model_df, minsize = 250)

# fit random forest
mod_rf <- reg_rf(formula = eq,
                 n_trees = 50, feature_frac = 0.8, data = model_df)

# importance regression tree
imp_rt <- mod_rt$importance
imp_rt$TYPE <- "Regression Tree"
  
# importance random forest
imp_rf <- mod_rf$importance
imp_rf$TYPE <- "Random Forest"

# merge to a plot_df
plot_df <- rbind(imp_rf, imp_rt, imp_true)

ggplot(plot_df, aes(x = FEATURES, y = IMPORTANCE, fill = TYPE)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "") +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(name = "", values = c("#013848", "#00A378", "#FF8000"))
  