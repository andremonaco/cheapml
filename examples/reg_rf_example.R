# Regression Random Forest Runthrough ----------------------------------------------

# load data simulation tool
# library(devtools)
# devtools::install_github('andrebleier/Xy')
library(Xy)
library(ggplot2)
source("algorithms/reg_rf.R")
source("algorithms/reg_tree_imp.R")

# simulate data
sim <- Xy(n = 2500, # 2500 observations
          numvars = c(5, 0), # 5 linear variables / 0 nonlinear
          noisevars = 5, # 5 noise variables
          catvars = 0, # omit dummy variables
          weights = c(-5, 20), # range of the beta weights
          stn = 5, # signal to noise ratio
          intercept = FALSE # intercept in the model
)

# simulation overview
sim

# get the formula
eq <- sim$eq

# get the formula
model_df <- sim$data

# plot the true variable importance
imp_true <- varimp(sim, plot = FALSE)

# subset to importance mean
imp_true <- imp_true[, c(1,2)]
names(imp_true)[2] <- "IMPORTANCE" 
imp_true$TYPE <- "Truth"

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
  