# Regression Tree ---------------------------------------------------------

# This is the splitting criterion we minimize (SSE [Sum Of Squared Errors]):
# $SSE = \sum_{i \in S_1} (y_i - \bar(y)1)^2 + \sum_{i \in S_2} (y_i - \bar(y)2)^2$
sse_var <- function(x, y) {
  splits <- sort(unique(x))
  sse <- c()
  for (i in seq_along(splits)) {
    sp <- splits[i]
    sse[i] <- sum((y[x < sp] - mean(y[x < sp]))^2) + sum((y[x >= sp] - mean(y[x >= sp]))^2) 
  }
  split_at <- splits[which.min(sse)]
  return(c(sse = min(sse), split = split_at))
}



#' reg_tree
#' Fits a simple regression tree with SSE splitting criterion. The estimator function
#' is the mean.
#' 
#' @param formula an object of class formula
#' @param data a data.frame or matrix
#' @param minsize a numeric value indicating the minimum size of observations
#'                in a leaf
#'
#' @return \itemize{
#' \item tree - the tree object containing all splitting rules and observations
#' \item imp - returns the feature importance
#' \item fit - our fitted values, i.e. X %*% theta
#' \item formula - the underlying formula
#' \item data - the underlying data
#' }
#' @export
#'
#' @examples # Complete runthrough see: www.github.com/andrebleier/cheapml
reg_tree_imp <- function(formula, data, minsize) {
  
  # coerce to data.frame
  data <- as.data.frame(data)
  
  # handle formula
  formula <- terms.formula(formula)
  
  # get the design matrix
  X <- model.matrix(formula, data)
  
  # extract target
  y <- data[, as.character(formula)[2]]
  
  # initialize while loop
  do_splits <- TRUE
  
  # create output data.frame with splitting rules and observations
  tree_info <- data.frame(NODE = 1, NOBS = nrow(data), FILTER = NA, TERMINAL = "SPLIT",
                          IMP_GINI = NA, SPLIT = NA, stringsAsFactors = FALSE)
  
  # keep splitting until there are only leafs left
  while(do_splits) {
    
    # which parents have to be splitted
    to_calculate <- which(tree_info$TERMINAL == "SPLIT")
    
    for (j in to_calculate) {
      
      # handle root node
      if (!is.na(tree_info[j, "FILTER"])) {
        # subset data according to the filter
        this_data <- subset(data, eval(parse(text = tree_info[j, "FILTER"])))
        # get the design matrix
        X <- model.matrix(formula, this_data)
      } else {
        this_data <- data
      }
      
      # estimate splitting criteria
      splitting <- apply(X,  MARGIN = 2, FUN = sse_var, y = this_data[, all.vars(formula)[1]])
      
      # get the min SSE
      tmp_splitter <- which.min(splitting[1,])
      
      # define maxnode
      mn <- max(tree_info$NODE)
      
      # paste filter rules
      current_filter <- c(paste(names(tmp_splitter), ">=", 
                            splitting[2,tmp_splitter]),
                      paste(names(tmp_splitter), "<", 
                            splitting[2,tmp_splitter]))
      
      # Error handling! check if the splitting rule has already been invoked
      split_here  <- !sapply(current_filter,
                             FUN = function(x,y) any(grepl(x, x = y)),
                             y = tree_info$FILTER)
      
      # append the splitting rules
      if (!is.na(tree_info[j, "FILTER"])) {
        current_filter  <- paste(tree_info[j, "FILTER"], 
                             current_filter, sep = " & ")
      } 
      
      # calculate metrics within the children
      metr <- lapply(current_filter,
                         FUN = function(i, x, data, formula) {
                          df <- subset(x = x, subset = eval(parse(text = i)))
                          nobs <- nrow(df)
                          w <- nobs/nrow(data)
                          y <- df[, all.vars(formula)[1]]
                          imp <- mean((y - mean(y, na.rm = TRUE))^2)
                          return(c(nobs, w*imp))
                         },
                         x = this_data, data = data, formula = formula)  
      
      # extract relevant information
      current_nobs <- sapply(metr, function(x) x[[1]])
      imp_sum_child <- sum(sapply(metr, function(x) x[[2]]))
      current_y <- this_data[, all.vars(formula)[1]]
      imp_parent <- nrow(this_data)/nrow(data) * mean((current_y-mean(current_y))^2)
      imp_gini <- imp_parent - imp_sum_child
      
      # insufficient minsize for split
      if (any(current_nobs <= minsize)) {
        split_here <- rep(FALSE, 2)
      }
      
      # create children data frame
      children <- data.frame(NODE = c(mn+1, mn+2),
                             NOBS = current_nobs,
                             FILTER = current_filter,
                             TERMINAL = rep("SPLIT", 2),
                             IMP_GINI = NA,
                             SPLIT = NA,
                             row.names = NULL)[split_here,]
      
      # overwrite state of current node, add gini importance and split variable
      tree_info[j, "TERMINAL"] <- ifelse(all(!split_here), "LEAF", "PARENT")
      tree_info[j, "IMP_GINI"] <- imp_gini
      if (tree_info[j, "TERMINAL"] == "PARENT") {
      tree_info[j, "SPLIT"] <- names(tmp_splitter)
      }
      
      # bind everything
      tree_info <- rbind(tree_info, children)
      
      # check if there are any open splits left
      do_splits <- !all(tree_info$TERMINAL != "SPLIT")
    } # end for
  } # end while
  
  # calculate fitted values
  leafs <- tree_info[tree_info$TERMINAL == "LEAF", ]
  fitted <- c()
  for (i in seq_len(nrow(leafs))) {
    # extract index
    ind <- as.numeric(rownames(subset(data, eval(parse(text = leafs[i, "FILTER"])))))
    # estimator is the mean y value of the leaf
    fitted[ind] <- mean(y[ind])
  }
  
  # calculate feature importance
  imp <- tree_info[, c("SPLIT", "IMP_GINI")]
  
  if (!all(is.na(imp$SPLIT))) {
  imp <- aggregate(IMP_GINI ~ SPLIT, FUN = function(x, all) sum(x, na.rm = T)/sum(all, na.rm = T), 
                    data = imp, all = imp$IMP_GINI)
  }
  
  # rename to importance
  names(imp) <- c("FEATURES", "IMPORTANCE")
  imp <- imp[order(imp$IMPORTANCE, decreasing = TRUE),]
  
  # return everything
  return(list(tree = tree_info, fit = fitted, formula = formula, 
              importance = imp, data = data))
}