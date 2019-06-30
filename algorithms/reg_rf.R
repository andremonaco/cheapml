# Regression Random Forest ------------------------------------------------

#' reg_rf
#' Fits a random forest with a continuous scaled features and target 
#' variable (regression)
#'
#' @param formula an object of class formula
#' @param n_trees an integer specifying the number of trees to sprout
#' @param feature_frac an numeric value defined between [0,1]
#'                     specifies the percentage of total features to be used in
#'                     each regression tree
#' @param data a data.frame or matrix
#'
#' @importFrom plyr raply
#' @return
#' @export
#'
#' @examples # Complete runthrough see: www.github.com/andrebleier/cheapml
reg_rf <- function(formula, n_trees, feature_frac, data) {
  
  # source the regression tree function
  source("algorithms/reg_tree_imp.R")
  
  # load plyr
  require(plyr)
  
  # define function to sprout a single tree
  sprout_tree <- function(formula, feature_frac, data) {
    # extract features
    features <- all.vars(formula)[-1]
    
    # extract target
    target <- all.vars(formula)[1]
    
    # bag the data
    # - randomly sample the data with replacement (duplicate are possible)
    train <-
      data[sample(1:nrow(data), size = nrow(data), replace = TRUE),]
    
    # randomly sample features
    # - only fit the regression tree with feature_frac * 100 % of the features
    features_sample <- sample(features,
                              size = ceiling(length(features) * feature_frac),
                              replace = FALSE)
    
    # create new formula
    formula_new <-
      as.formula(paste0(target, " ~ -1 + ", paste0(features_sample,
                                              collapse =  " + ")))
    
    # fit the regression tree
    tree <- reg_tree_imp(formula = formula_new,
                         data = train,
                         minsize = ceiling(nrow(train) * 0.1))
    
    # save the fit and the importance
    return(list(tree$fit, tree$importance))
  }
  
  # apply the rf_tree function n_trees times with plyr::raply
  # - track the progress with a progress bar
  trees <- plyr::raply(
    n_trees,
    sprout_tree(
      formula = formula,
      feature_frac = feature_frac,
      data = data
    ),
    .progress = "text"
  )
  
  # extract fit
  fits <- do.call("cbind", trees[, 1])
  
  # calculate the final fit as a mean of all regression trees
  rf_fit <- apply(fits, MARGIN = 1, mean, na.rm = TRUE)

  # extract the feature importance
  imp_full <- do.call("rbind", trees[, 2])
  
  # build the mean feature importance between all trees
  imp <- aggregate(IMPORTANCE ~ FEATURES, FUN = mean, imp_full)
  
  # build the ratio for interpretation purposes
  imp$IMPORTANCE <- imp$IMPORTANCE / sum(imp$IMPORTANCE)
  
  # export
  return(list(fit = rf_fit,
              importance = imp[order(imp$IMPORTANCE, decreasing = TRUE), ]))
}