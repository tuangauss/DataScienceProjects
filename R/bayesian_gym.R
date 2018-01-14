# load libraries
library(rjags)
library(dplyr)
library(MASS)
library(ggplot2)

# load data
raw_data <- read.csv("~/data/Vietnamese_2016.csv", 
                     head = TRUE, sep = ";")
head(raw_data)
summary(raw_data$Age_gr)

# clean data
data <- raw_data %>%
  filter(Age_gr == "18-29") %>%
  filter(Sex == "male") %>%
  dplyr::select(height, weight, BMI) %>%
  mutate(height = as.numeric(gsub(",", ".", height))) %>%
  mutate(weight = as.numeric(gsub(",", ".", weight)))

# my info
m_height = 168
m_weight = 58
m_BMI =  m_weight / (m_height/100)^2

# visualization:
truehist(data$weight,nbins = 50, 
         main = paste("Histogram of Vietnamese male weight"), xlab = "Weight in kg")
abline(v=m_weight,col="black", lwd = 4)
abline(v=median(data$weight), col = "red", lty = 4, lwd = 4)
abline(v=mean(data$weight), col ="orange", lty = 4, lwd = 4)
text(m_weight-2, 0.12, "Me!!!")

my_data <- data.frame(height = m_height, weight = m_weight)
ggplot(data, aes(height, weight)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, colour = "blue", alpha = 0.4) + theme_minimal() +
  geom_point(data = my_data, color ="red", size = 5) + 
  labs (title = "Weight versus Height plot of 383 Vietnamese male and Tuan", subtitle = "***Red point is author's own measurement") + 
  theme(plot.title = element_text(color="#666666", face="bold", size=20, hjust=0))


# add standard least square line
model <- lm(data$weight ~ data$height) #fit linear model
label_text <- paste('Fitted model: ', round(coef(model)[1], 3), ' + ', round(coef(model)[2], 3), ' x', sep = '')
ggplot(data, aes(height, weight)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, colour = "blue", alpha = 0.4) + theme_minimal() +
  geom_smooth(method = "lm", fullrange=TRUE, color = "red") +
  geom_text(aes(x = 143, y = 55, label = label_text),hjust = 0, size = 6) +
  geom_point(data = my_data, color ="red", size = 5) + 
  labs (title = "Weight versus Height plot of 383 Vietnamese male and Tuan") + 
  theme(plot.title = element_text(color="#666666", face="bold", size=20, hjust=0))



#### Running JAGS model ####
############################

n <- nrow(data) #383 data points

mymodel <- "
model{
for(i in 1:n){
y[i] ~ dnorm(a + b*x[i], tau)
}
a ~ dnorm(0, 1e-6)
b ~ dnorm(0, 1e-6)
tau ~ dgamma(.01,.01)
sig <- 1/sqrt(tau)
}
"

jm <- jags.model(file = textConnection(mymodel), data=list(n=n, x=data$height, y=data$weight))
cs <- coda.samples(jm, c("a","b","sig"), 11000)
sample_data <- as.data.frame(cs[[1]][-(1:1000),])

cmean <- sample_data$a + sample_data$b*m_height  # "conditional mean"

m_perc <- pnorm(q = m_weight, mean = cmean, sd = sample_data$sig) 
truehist(m_perc, main = "Posterior distribution for my weight percentile", 
         xlab = "percentile", ylab = "Frequency")
mean(m_perc<=0.4)
mean(m_perc)


### What happen if I compare myself to American men
nls_data <-read.csv("~/data/national_longitudinal_survey.csv", head = TRUE)
nls_data <- nls_data %>%
  filter(Gender == "Male") %>%
  mutate (height = Height..inches.*2.54) %>%
  mutate (weight = Weight..lbs./2.2046) %>%
  dplyr::select(height,weight)

#4150 data points
n <- nrow(nls_data)

mymodel <- "
model{
for(i in 1:n){
y[i] ~ dnorm(a + b*x[i], tau)
}
a ~ dnorm(0, 1e-6)
b ~ dnorm(0, 1e-6)
tau ~ dgamma(.01,.01)
sig <- 1/sqrt(tau)
}
"

jm <- jags.model(file = textConnection(mymodel), data=list(n=n, x=nls_data$height, y=nls_data$weight))
cs <- coda.samples(jm, c("a","b","sig"), 11000)
sample_data <- as.data.frame(cs[[1]][-(1:1000),])

cmean <- sample_data$a + sample_data$b*m_height
m_perc <- pnorm(q = m_weight, mean = cmean, sd = sample_data$sig) 
truehist(m_perc)
