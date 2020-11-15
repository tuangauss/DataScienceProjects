library(ggplot2)

true_p <- 0.001
iter <- function(size){
  samp <- sample(x= c(1,0),
                 size = size,
                 prob = c(true_p, 1 - true_p),
                 replace = TRUE)
  cut <- which.max(samp) - 1
  upper_bound <- min(3/cut, 1)
  return(upper_bound)
}

res <- replicate(n = 100000, iter(size = 10000))
sum(res > true_p)

ggplot() + 
  aes(res) +
  geom_histogram(colour="black", fill="grey", bins = 100) +
  geom_vline(aes(xintercept = true_p), color = "red") +
  xlim(c(0, .05)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Upper Bound")
