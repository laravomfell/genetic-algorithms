fit.fun <- function(weights, data, pred){
  # Brier Score
  return(sum((apply(data, 1, function(x) sum(x * weights)) - pred) ^ 2) / nrow(data))
}

# Population Function (generates weights that sum up to 1)
gen.pop <- function(popsize, dims, low, up) {
  m <- matrix(runif(popsize*dims, low, up), ncol=dims)
  m <- ifelse(m < 0.01, 0, m)
  m <- sweep(m, 1, rowSums(m), FUN="/")
  m
}

# Population Function for PBIL (probability based)
gen.pop.pbil <- function(size, variables, intv, prob){
  # take prob within which interval the value shall be and sample from there
  
  population <- foreach(j = 1:size, .combine = rbind) %:% foreach(l = 1:variables, .combine=c) %do% 
    ifelse(runif(1) < prob[l], runif(1, intv[l], 1), runif(1, 0, intv[l]))
  
  # sparsity + normalize
  population <- ifelse(population < 0.01, 0, population)
  population <- sweep(population, 1, rowSums(population), FUN="/")
  
  # drop rownames introduced in foreach
  rownames(population) <- NULL
  return(population)
}