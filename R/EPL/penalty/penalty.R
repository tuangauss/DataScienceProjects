################################
#### Data Science Project  #####
# Article:                     #  
# https://tinyurl.com/y2ynruqo #
################################

library(MASS)
library(tidyverse)
library(betareg)
library(xkcd)

# read raw_data
raw_data <- read.csv("./all_penalties.csv",
                     stringsAsFactors = FALSE)

# basic cleaning and group by player
player_data <- raw_data %>%
  mutate(name = str_squish(name),
         penalties = ifelse(penalties == '-',0,penalties),
         scored = as.numeric(ifelse(scored =='-', 0, scored))) %>%
  group_by(name) %>%
  summarise(total = sum(penalties),
            total_score = sum(scored))%>%
  mutate(ratio = total_score/(total)) %>%
  filter(total >= 4 & ratio > 0 & ratio < 1) %>%
  na.omit()

#### draw xkcd with dataman
xrange = c(0.2,1.0)
yrange = c(0,4)
ratioxy <- diff(xrange) / diff(yrange)
mapping <- aes(x=x,
               y=y,
               scale=scale,
               ratioxy=ratioxy,
               angleofspine = angleofspine,
               anglerighthumerus = anglerighthumerus,
               anglelefthumerus = anglelefthumerus,
               anglerightradius = anglerightradius,
               angleleftradius = angleleftradius,
               anglerightleg =  anglerightleg,
               angleleftleg = angleleftleg,
               angleofneck = angleofneck)

dataman <- data.frame( x= 0.3, y=3,
                       scale = 0.5,
                       ratioxy = ratioxy,
                       angleofspine =  -pi/2,
                       anglerighthumerus = -pi/6,
                       anglelefthumerus = -pi/2 -pi/6,
                       anglerightradius = pi/5,
                       angleleftradius = pi/5,
                       angleleftleg = 3*pi/2  + pi / 12 ,
                       anglerightleg = 3*pi/2  - pi / 12,
                       angleofneck = runif(1, min = 3 * pi / 2 - pi/10 , max = 3 * pi / 2 + pi/10))

# draw histogram of conversion rates
player_data %>%
  ggplot(aes(ratio)) +
  geom_histogram(breaks = 5:25/25,
                fill = hcl(0, 50, 80)) +
  xkcdaxis(c(0.1,1), c(0,80)) +
  labs (x = "\nHistogram of penalties conversion rate", y = "Count") +
  theme_xkcd() 

# fit a beta distribution on the histogram
m <- MASS::fitdistr(player_data$ratio, dbeta,
                    start = list(shape1 = 10, shape2 = 1),
                    lower=c(0.1,0.1))
alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

# plot the fit with some fun xkcd
ggplot(player_data) +
  geom_histogram(aes(ratio, y = ..density..),
                 breaks = 5:25/25,
                 fill = hcl(0, 50, 80)) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  xlab("\n Penalty Coversion Rate") + 
  theme_xkcd() +
  xkcdaxis(xrange, yrange) +
  xkcdman(mapping, dataman) +
  annotate("text", x=0.4, y = 4,
           label = "Does not look an amazing good fit\nBut it's okay",
           family="xkcd") +
  xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),
           data.frame(xbegin=0.36,ybegin=3,xend=0.42,yend=3.5),
           xjitteramount = 0.01)

# adjusted ratio:
adjusted_ratio <- player_data %>%
  mutate(eb_estimate = (total_score + alpha0) / (total + alpha0 + beta0)) %>%
  arrange(desc(eb_estimate))

# posterior plots for specific players:
specific_players <- adjusted_ratio %>%
  filter(name %in% c("Cristiano Ronaldo",
                     "Nicolas Pepe",
                     "Alexis Sanchez",
                     "Antoine Griezmann")) %>%
  mutate(alpha = total_score + alpha0,
         beta = total - total_score + beta0)

# draw posterior beta distribution for these players
specific_players %>%
  crossing(x=seq(0.4,0.99,.002)) %>%
  ungroup() %>%
  mutate(density=dbeta(x,alpha,beta)) %>%
  ggplot(aes(x, density, color = name)) +
  geom_line() +
  stat_function(fun=function(x) dbeta(x, alpha0, beta0), lty = 2, color = 'black') +
  xlab("Conversion rate") +
  theme_xkcd()

# draw actual vs adjusted ratio plot
ggplot(adjusted_ratio, aes(ratio, eb_estimate, color = total)) +
  geom_hline(yintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  geom_point() +
  geom_abline(color = "red") +
  scale_colour_gradient(breaks = c(0,20,30,50,70)) +
  xlim(0.5,1) +
  ylim(0.5,1) +
  xlab("Actual goal scoring average") +
  ylab("Posterior goal scoring average")

               
#### When it seems that a unimodal beta distribution is not a good fit
#### we can use E-M algorithm (implemented in the betareg package
#### to fit 2 beta distributions
m<- betamix(ratio ~ 1| 1, data = player_data, k = 1:3)

mu <- plogis(coef(m)[,1])
phi <- exp(coef(m)[,2])
a <- mu*phi
b <- (1-mu)*phi
# get the cluser
cl <- clusters(m)

# plotting
## separate histograms for both clusters
## TODO: convert back to ggplot code
hist(subset(player_data, cl == 1)$ratio, breaks = 5:25/25, freq = FALSE,
     col = hcl(0, 50, 80), main = "", xlab = "Penalty Conversion Rate", ylim = c(0, 9))

hist(subset(player_data, cl == 2)$ratio, breaks = 5:25/25, freq = FALSE,
     col = hcl(240, 50, 80), main = "", xlab = "Penalty Conversion Rate", ylim = c(0, 9), add = TRUE)

## lines for fitted densities
ys <- seq(0, 1, by = 0.01)
lines(ys, dbeta(ys, shape1 = a[1], shape2 = b[1]),
      col = hcl(0, 80, 50), lwd = 2)
lines(ys, dbeta(ys, shape1 = a[2], shape2 = b[2]),
      col = hcl(240, 80, 50), lwd = 2)

## lines for corresponding means
abline(v = mu[1], col = hcl(0, 80, 50), lty = 2, lwd = 2)
abline(v = mu[2], col = hcl(240, 80, 50), lty = 2, lwd = 2)

## repeat Bayesian updating
## only group specific this time
post <- posterior(m)
post[,1]
# posterior probabilies of being assigned to each group
player_data$post_1 <- post[,1]
player_data$post_2 <- post[,2]

player_data <- player_data %>%
  mutate(shrunkage_1 = (total_score + a[1])/(total + a[1] + b[1]),
         shrunkage_2 = (total_score + a[2])/(total + a[2] + b[2]),
         shrunkage_ave = (post_1*shrunkage_1 + post_2*shrunkage_2)) %>%
  arrange(desc(shrunkage_ave))

# plot
player_data %>%
  gather(type, value, ratio, shrunkage_ave)%>%
  mutate(type = ifelse(type == 'ratio',
                       'Raw scoring ratio',
                       'Average posterior'),
         type = relevel(factor(type), 'Raw scoring ratio')) %>%
  ggplot(aes(total_score, value)) +
  geom_point() +
  facet_wrap(~ type) +
  ylab("Estimate") +
  theme_bw()
