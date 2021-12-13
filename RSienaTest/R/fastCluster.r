# Function to substitute all occurunces of the variable 'x' in an expression
substituteX <- function(x, expr) {
  do.call('substitute', list(expr, list(x=x)))
}

ClusterEvalQ.SplitByRow <- function(cl, expr, xgrid) {
  # Split the xgrid array into subsets/batches
  xbatches <- splitRows(xgrid,length(cl))

  # Create sets of expressions, replacing 'x' in each expression
  # with the correct subset of the xgrid array
  exprs <- lapply(xbatches, substituteX, expr=expr)

  # Evaluate the sets of expressions on each worker
  docall(c, clusterApply(cl, exprs, eval))
}
