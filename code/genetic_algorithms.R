#=====================
#                    #
#  1. Stochastic HC  #
#                    #
#=====================

SHC <- cmpfun(function(X, Y, iter = 100L){
  
  # setting initial values
  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- 0 * X
  sum.weights <- 0L
  best        <- sample(ncol(X), 1)
  conv        <- vector(length = iter)
  
  # performing stochastic hill-climbing
  while(sum.weights < iter) {
    sum.weights   <- sum.weights + 1L
    pred          <- (pred + X) * (1L / sum.weights)
    errors        <- colSums((pred - Y) ^ 2) / nrow(pred)
    # randomly select another neighbor
    neighbor      <- sample(1:length(errors), 1)
    best          <- ifelse(errors[neighbor] < errors[best], neighbor, which.min(errors))
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
    conv[sum.weights] <- errors[best]
    
  }
  
  # returning model weights
  return(list(best=(weights / sum.weights), conv=conv))
})


SHC_bag <- cmpfun(function(X, Y, bags = 10L, p = 0.5, iter = 100L){
  
  # setting initial values
  i <- 0L
  N <- nrow(X)
  M <- ncol(X)
  W <- matrix(rbinom(bags * M, 1, p), ncol = M)
  conv <- list()
  
  # performing bagging
  while(i < bags)  {
    
    # doing ES on a bagged sample
    i         <- i + 1L
    ind       <- which(W[i, ] == 1)
    alg       <- SHC(X[, ind], Y, iter)
    W[i, ind] <- W[i, ind] * alg$best
    conv[[i]] <- alg$conv
  }
  
  # returning model weights
  return(list(best=(colSums(W) / bags), conv=conv))
})


#=====================
#                    #
#  2. Backtracking   #
#                    #
#=====================

bsa <- cmpfun(function(X, Y, popsize, low_val, up_val, mixrate, generations){
  conv <- list()
  
  # Initialization
  dims  <- ncol(X)               # Dimension of Model Library
  up    <- rep(up_val, dims)     # upper search space boundary
  low   <- rep(low_val, dims)    # lower search space boundary
  
  # generate population
  pop   <- gen.pop(popsize, dims, low, up)
  
  # evaluate fitness
  fitnesspop <- apply(pop, 1, fit.fun, data=X, pred=Y)
  
  # generate historic population
  hist_pop <- gen.pop(popsize, dims, low, up)
  
  for (j in 1:generations){
    
    if (runif(1) < runif(1)){hist_pop <- pop} else {hist_pop <- pop}
    
    # Permutation
    hist_pop <- hist_pop[sample(nrow(hist_pop)),]
    
    # scale factor F
    F <- runif(1) * 3 
    
    map <- matrix(0, ncol=dims, nrow=popsize)
    
    # Mutation + Crossover
    
    # two predefined strategies for mutation
    if (runif(1) < runif(1)){
      for (i in 1:nrow(map)){
        for (u in sample(ncol(map), size=ceiling(runif(1)*dims*mixrate))){
          map[i,u] <- 1
        }}
    } else {
      for (i in 1:nrow(map)){
        map[i, sample(1:ncol(map), 1)] <- 1
      }}
    
    # Trial Population
    
    mutant   <- pop
    mutation <- pop + map*F*(hist_pop - pop)
    mutant[map==1] <- 0
    
    # Boundary control (no negative values and the weights must sum to 1) + Sparsity
    
    mutation <- abs(mutation)
    mutation <- ifelse(mutation < 0.01, 0, mutation)
    
    
    # standardize and scale
    mutation <- sweep(mutation, 1, rowSums(mutation, na.rm=T), "/")
    mutation <- sweep(mutation, 1, 1-rowSums(mutant), "*")
    
    mutant[map==1] <- mutation[map==1]
    
    # Selection II
    fitness_mutant   <- apply(mutant, 1, fit.fun, data=X, pred=Y)
    
    ind              <- fitness_mutant < fitnesspop
    fitnesspop[ind]  <- fitness_mutant[ind]
    pop[ind,]        <- mutant[ind,] 
    
    #minimum         <- min(fitnesspop)
    minimizer       <- pop[which.min(fitnesspop),] 
    
    conv[[j]] <- fitnesspop
    
  }
  return(list(minimizer=minimizer, conv=conv))
})

bsa_bag <- cmpfun(function(X, Y, bags, p=0.5, popsize, low_val, up_val, mixrate, generations){
  
  # setting initial values
  i <- 0L
  M <- ncol(X)
  W <- matrix(rbinom(bags * M, 1, p), ncol = M)
  conv <- list()
  
  while(i < bags){
    i <- i + 1L
    ind <- which(W[i, ] == 1)
    alg <- bsa(X[,ind], Y, popsize, low_val, up_val, mixrate, generations)
    W[i, ind ] <- W[i, ind] * alg$minimizer
    conv[[i]] <- matrix(unlist(alg$conv), ncol=popsize, byrow=T)
    
  }
  # returning model weights
  return(list(opt=(colSums(W)/bags)/sum(colSums(W)/bags), conv=conv))
})



#=============
#            #
#  3. PBIL   #
#            #
#=============

pbil <- cmpfun(function(X, Y, popsize, generations, low_val, up_val, LR, mut_prob, mut_shift){
  conv <- list()
  
  # Initialization
  dims  <- ncol(X)               # Dimension of Model Library
  up    <- rep(up_val, dims)     # upper search space boundary
  low   <- rep(low_val, dims)    # lower search space boundary
  
  # vector of the intervals, in the beginning set all weights at 1/32 as the sum over all weights is then 1
  interval <- rep(1/dims, dims)
  
  # for continuous spaces, probability vector gives probability that weight_i > (low_i + up_i)/2 (the interval)
  popvec <- rep(0.5, dims)
  
  
  for (i in 1:generations){
    # Generate Population
    pop <- gen.pop.pbil(size=popsize, variables=dims, intv=interval, prob=popvec)
    
    # Evaluate Fitness and keep best
    fitnesspop <- apply(pop, 1, fit.fun, data=X, pred=Y)
    best  <- pop[which.min(fitnesspop),]
    
    # Update prob-vector
    popvec <- popvec*(1-LR) + LR*best
    
    # mutate prob-vector and conditionally update interval
    for (m in 1:length(popvec)){
      popvec[m] <- ifelse(runif(1) < mut_prob, popvec[m]*(1-mut_shift)+sample(c(0,1),1)*mut_prob, popvec[m])
      
      if (popvec[m] <= 0.1){
        popvec[m] <- 0.5
        up[m] <- interval[m]
        interval[m] <- (low[m]+up[m])/2
      } else {
        if (popvec[m] >= 0.9){
          popvec[m] <- 0.5
          low[m] <- interval[m]
          interval[m] <- (low[m]+up[m])/2 }}
    }
    conv[[i]] <- fitnesspop
  }
  return(list(best=best, conv=conv))
})

pbil_bag <- cmpfun(function(X, Y, bags, p = 0.5, popsize, generations, low_val, up_val, LR, mut_prob, mut_shift){
  l <- 0L
  M <- ncol(X)
  W <- matrix(rbinom(bags * M, 1, p), ncol = M)
  conv <- list()
  
  while(l < bags){
    l <- l + 1L
    ind <- which(W[l, ] == 1)
    alg <- pbil(X[,ind], Y, popsize, generations, low_val, up_val, LR, mut_prob, mut_shift)
    W[l, ind] <- W[l, ind] * alg$best
    conv[[l]] <- matrix(unlist(alg$conv), ncol=popsize, byrow=T)
  }
  return((colSums(W)/bags) / sum(colSums(W)/bags))
})


#=============
#            #
#  4. PSO    #
#            #
#=============

pso <- cmpfun(function(X, Y, popsize, low_val, up_val, iter, inertia, phi, speed){
  conv <- list()
  
  # Initialization
  dims  <- ncol(X)              # Problem Dimension
  up    <- rep(up_val, dims)    # vector of upper boundaries
  low   <- rep(low_val, dims)   # vector of lower boundaries
  phi1  <- phi2 <- phi          # social and cognitive components
  
  # generate population
  pop <- gen.pop(popsize, dims, low_val, up_val)
  
  # generate velocities
  velocities <- matrix(runif(popsize*dims, 0, speed), ncol=dims)
  
  # find fitness values
  fitnesspop <- apply(pop, 1, fit.fun, data = X, pred = Y)
  
  global_best <- pop[which.min(fitnesspop),] 
  minimum     <- min(fitnesspop)
  
  personal_best    <- pop
  personal_fitness <- apply(personal_best, 1, fit.fun, data = X, pred = Y)
  
  
  for (i in 1:iter){
    # Mutation
    velocities <- inertia * velocities + phi1*runif(popsize*dims)*(personal_best-pop) + phi2*runif(popsize*dims)*(global_best - pop)
    pop <- pop + velocities
    
    # Boundary control (no negative values and the weights must sum to 1) + Sparsity
    pop <- abs(pop)
    pop <- ifelse(pop < 0.01, 0, pop)
    pop <- sweep(pop, 1, rowSums(pop, na.rm=T), "/")
    
    fitnesspop <- apply(pop, 1, fit.fun, data = X, pred = Y)
    
    # update global best if better
    global_best <- if(min(fitnesspop) < minimum) { pop[which.min(fitnesspop),] } else { global_best }
    
    # update personal fitness
    personal_best[(min(fitnesspop) < personal_fitness),] <- pop[(min(fitnesspop) < personal_fitness),]
    personal_fitness <- apply(personal_best, 1, fit.fun, data=X, pred=Y)
    
    minimum <- min(fitnesspop)
    conv[[i]] <- fitnesspop
  }
  return(list(best=global_best, conv=conv))
})

pso_bag <- cmpfun(function(X, Y, bags, p=0.5, popsize, low_val, up_val, iter, inertia, phi, speed){
  
  # setting initial values
  i <- 0L
  M <- ncol(X)
  W <- matrix(rbinom(bags * M, 1, p), ncol = M)
  conv <- list()
  
  while(i < bags){
    i <- i + 1L
    ind <- which(W[i, ] == 1)
    alg <- pso(X[,ind], Y, popsize, low_val, up_val, iter, inertia, phi, speed)
    W[i, ind] <- W[i, ind] * alg$best
    conv[[i]] <- matrix(unlist(alg$conv), ncol=popsize, byrow=T)
    
  }
  # returning model weights
  return((colSums(W)/bags)/sum(colSums(W)/bags))
})

#=============
#            #
#  5.  GA    #
#            #
#=============

GA <- cmpfun(function(X, Y, popsize, low_val, up_val, elitism, p.cross, p.mut, generations){
  conv <- list()
  
  # Initialization
  dims  <- ncol(X)              # Problem Dimension
  up    <- rep(up_val, dims)    # vector of upper boundaries
  low   <- rep(low_val, dims)   # vector of lower boundaries
  elite <- ceiling(elitism * popsize)
  
  # initialize population
  pop <- gen.pop(popsize, dims, low_val, up_val)
  
  # evaluate fitness
  fitnesspop <- apply(pop, 1, fit.fun, data = X, pred = Y)
  
  for (i in 1:generations){
    sortedpop <- pop[which(!duplicated(pop[order(fitnesspop),], margin=1)),]
    
    # Roulette Wheel Selection
    wheel <- abs(fitnesspop)/sum(abs(fitnesspop))
    sel <- sample(1:popsize, size=popsize, prob = pmin(pmax(0, wheel), 1, na.rm = TRUE), replace=T)
    pop <- pop[sel,]
    
    # Crossover
    mating <- matrix(sample(1:popsize, size = popsize), ncol=2)
    
    for (j in popsize/2){
      if (p.cross > runif(1)){
        parents <- pop[mating[j,],]
        children <- parents
        cross.points <- sample(dims, size = floor(runif(1)*dims))
        if (length(cross.points) != 0 & length(cross.points) != dims){
          children[1, cross.points] <- parents[2, cross.points]
          children[2, cross.points] <- parents[1, cross.points]
        }
        pop[parents,] <- children
      }
    }
    
    # Mutation
    for (i in 1:popsize){
      # change to 1-point crossover
      if (p.mut > runif(1)){ pop[i, sample(1:dims, size=1)] <- runif(1, low_val, up_val) }
    }
    
    # Weights Control
    pop <- ifelse(pop < 0.01, 0, pop)
    pop <- sweep(pop, 1, rowSums(pop), FUN = "/")
    
    # Elitism
    fitnesspop <- apply(pop, 1, fit.fun, data = X, pred = Y)
    pop[order(fitnesspop, decreasing=T)[1:elite],] <- sortedpop[1:elite,]
    
    fitnesspop <- apply(pop, 1, fit.fun, data = X, pred = Y)
    minimizer <- pop[which.min(fitnesspop),]
    
    conv[[i]] <- fitnesspop
  }
  return(list(best=minimizer, conv=conv))
})


GA_bag <- cmpfun(function(X, Y, bags, p=0.5, popsize, low_val, up_val, elitism, p.cross, p.mut, generations){
  conv <- list()
  
  # setting initial values
  i <- 0L
  M <- ncol(X)
  W <- matrix(rbinom(bags * M, 1, p), ncol = M)

  while(i < bags){
    i <- i + 1L
    ind <- which(W[i, ] == 1)
    alg <- GA(X[,ind], Y, popsize, low_val, up_val, elitism, p.cross, p.mut, generations)
    W[i, ind ] <- W[i, ind] * alg$best
    conv[[i]] <- matrix(unlist(alg$conv), ncol=popsize, byrow=T)
  }
  # returning model weights
  return(list(best=(colSums(W)/bags)/sum(colSums(W)/bags), conv=conv))
})