library(ggplot2)

# conditional weibull
# subtract t_0 to get exepcted years left
sample_w <- function(u, lambda, kappa, t_0){
  (t_0^kappa - lambda^kappa*log(1-u))^(1/kappa) - t_0
}
# conditional pareto
sample_p <- function(u, t_0, alpha = 2){
  t_0*(1-u)^(-1/alpha) - t_0
}

result_w = c()
result_p = c()
year_range <- seq(0, 80, 10)
for(t in year_range){
  u <- runif(10000)
  samps_w <- sample_w(u, 77.1, 5.05, t)
  samps_p <- sample_p(u, t)
  result_w <- c(result_w, mean(samps_w))
  result_p <- c(result_p, mean(samps_p))
}

plot(year_range, result_w,
     type = "l", ylim = c(0, 100),
     main="Expected remaining year",
     xlab = "Year passed",
     ylab = "Years remaining",
     bty = "n")
lines(year_range, result_p, col = "green", lty = 2)
par(xpd=TRUE)
legend(x=4.5, y = 100,
       legend=c("Human life time", "Lindy's good"),
       lty=1:2,
       col = c("black", "green"),
       ncol=2)

# changing shape parameter
u <- runif(100000)
samps_p1 <- sample_p(u, 20, alpha = 2)
samps_p2 <- sample_p(u, 20, alpha = 1.5)
samps_p3 <- sample_p(u, 20, alpha = 3)
df <- data.frame("type"= c(rep("alpha = 1.5", 100000),
                           rep("alpha = 2", 100000),
                           rep("alpha = 3", 100000)),
                 "value" = c(samps_p2, samps_p1, samps_p3))
ggplot(df, aes(x=value, fill = type)) +
  geom_density(alpha = .3) +
  xlim(0, 50) + 
  ggtitle("pdf of years remaining after the first 20 years") + 
  xlab("Years remaining") +
  ylab("Probability")



#after 20 years
u <- runif(100000)
samps_w <- sample_w(u, 77.1, 5.05, 20)
mean_w <- mean(samps_w)
samps_p <- sample_p(u, 20)
mean_p <- mean(samps_p)
df <- data.frame("type"= c(rep("Human life time", 100000),
                   rep("Lindy's good", 100000)),
                 "value" = c(samps_w, samps_p))
ggplot(df, aes(x=value, fill = type)) +
  geom_density(alpha = .3) +
  xlim(0, 100) + 
  ggtitle("pdf of years remaining after the first 20 years") + 
  xlab("Years remaining") +
  ylab("Probability") +
  geom_vline(xintercept = mean_p, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = mean_w, linetype = "dashed", color = "red")
