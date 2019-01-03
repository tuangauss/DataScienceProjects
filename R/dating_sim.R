############################################
## The Optimal dating strategy
## Why we should always reject the first 37%
## An MC simulation
############################################

# calculate the theoretical probability of P(S_n,k)
theo_prob <- function(x){
  if (x == 1) return (1/100)
  else return ((x-1)/100 * (sum(1/((x:100)-1))))
}

# a util function to simulate the 'best-partner rank'
perm_rank <- function(n){
  return (sample(1:n, n))
}

# simulation(n) will run a MC simulation for the case of N=n
# returning the optimal M and the corresponding optimal probability
simulation <- function(n){
  M_range <- 2:n
  niter <- 1000 #for each value of M, we simulate 1000 times
  
  # declare a vector to store results, 
  # notice that if M = 1, the probability is 1/100
  prob_result <- rep(1/100, 100)
  
  # do a simulation for each value of M
  for (M in M_range){
    result <- rep(0, niter)
    for (i in 1:niter){
      order <- perm_rank(n) #simulate the order
      # find the best among the first M-1 that gets rejected
      highest_reject <- min(head(order, M-1))
      if (highest_reject != 1){
        accept <- order[order < highest_reject][1]
        # we consider ourselves successful if:
        # - rank 1 is not included in the first M-1 candidates
        # - rank 1 is the first person who is better than all we have seen
        if (accept == 1){
          result[i] <- 1 
        }
      }
    }
    prob_result[M] <- mean(result)
  }
  return (c(max(prob_result), which.max(prob_result)/n))
}

# applying simulation(n) to different values of n
opt_p <- sapply(2:30, function(x) simulation(x)[1])
plot(2:30, opt_p, ylim = c(0.2,1), main = 'Optimal probability \n P(S_n,k)',
     xlab = 'N', ylab = 'Probability')

opt_ratio <- sapply(2:30, function(x) simulation(x)[2])
plot(2:30, opt_ratio, ylim = c(0.2,1.1), main = 'Optimal ratio \n M/N',
     xlab = 'N', ylab = 'Ratio')
