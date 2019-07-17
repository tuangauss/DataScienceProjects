library (tidyverse)

bayes <- function(x, y_x, y_nx){
  num <- y_x * x
  denom <- y_x * x + y_nx * (1-x)
  return (num/denom)
}

bayes(0.01, 1, 1/7)

days <- seq(0,10,1)
posterior <- rep(0.01,11)
for (i in 2:11){
  post <- bayes(posterior[i-1], 1, 1/7)
  posterior[i] <- post
}

plot(days, posterior, main = "Posterior estimate",
     xlab = "Days", ylab = "Probability",
     pch = 19, frame = FALSE)


posterior_2<- rep(0.0001,11)
for (i in 2:11){
  post <- bayes(posterior_2[i-1], 1, 1/7)
  posterior_2[i] <- post
}

#https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
data.frame(days, posterior, posterior_2) %>%
  gather(prior, value, -days) %>%
  ggplot(aes(x=days, y = value, colour = prior)) +
  geom_point() +
  scale_color_discrete(name = "Value of prior \n on Day-1",
                      labels = c(0.01, 0.0001)) +
  scale_x_continuous(breaks = days) + 
  theme_classic()