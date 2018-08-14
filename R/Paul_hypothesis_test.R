#######################################
## Hypothesis testing procedure 
## The curious case of Paul the Octopus
## Fisher vs N-p approach
#######################################

# The script is self-containing, no extra module or libary needed


# graph binomial distribution, color extreme value and beyond
graph <- function(n,p, value){
  x <- seq(0,n)
  prob <- dbinom(x,size=n,prob=p)
  cols <- c("grey","red")[(h >= value) + 1] 
  barplot(prob,names.arg=0:n, col = cols,
          main=sprintf(paste('binomial distribution, size:',n, "prob:",p)))
}

graph(14,0.5,12)


# calculate p value for binomial distribtion, at x = 12
p_value = 1-pbinom(11,14,0.5)
#p_value = dbinom(12,14,0.5) + dbinom(13,14,0.5) + dbinom(14,14,0.5)

# Neyman- Pearson approach
# calculate current power
# a. Assuming type 1 error = 0.01
p_value = 1-pbinom(0:14, 14,0.5)
critical_value = which(p_value == p_value[p_value < 0.01][1])-1
type2 = pbinom(critical_value-1,14,0.75)

# b. More interesting problem
# You should try first before looking up the code
# if we want to achieve type 1 error < 1% and power > 90%, how many observation do we need to make?

stop = FALSE
for (n in 1:50){
  for (k in 0:n){
    type1 <- 1- pbinom(k,n,0.5)
    type2 <- pbinom(k-1,n,0.75)
    if (type1 < 0.01 & type2 <0.1){
      print (paste("n is ",toString(n),", k is", toString(k)))
      stop = TRUE
      break
    }
  }
  if (stop) break
}
# need 42 observations


# if we cut it some slack
# type 1 of 5% and type 2 of 20%
stop = FALSE
for (n in 1:50){
  for (k in 0:n){
    type1 <- 1- pbinom(k,n,0.5)
    type2 <- pbinom(k-1,n,0.75)
    if (type1 < 0.05 & type2 <0.2){
      print (paste("n is ",toString(n),", k is", toString(k)))
      stop = TRUE
      break
    }
  }
  if (stop) break
}
# still need 16 observations
