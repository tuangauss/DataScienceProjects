library(tidyverse)
source("prediction.R")
# in MAC, may have to go to font book to activate xkcd.ttf
#library(extrafont)
#font_import(path = ".", pattern="xkcd")
#fonts()
#loadfonts()

betting_house <- c("B365", "BW", "IW", "PS", "WH", "VC")

# easy computation of max odd or mean probability
# find max_odd if find_max is TRUE, else return Consensus Probability of event
row_max_prob <- function(df, row_idx, find_max){
  predict_outcome = df[row_idx, "predict_outcome"]
  if (is.na(predict_outcome)) return (NA)
  col_names <- paste0(betting_house, predict_outcome)
  val = ifelse(find_max, max(df[row_idx,col_names]), 1/mean(as.numeric(df[row_idx,col_names])))
  return (val)
}


##### find total return at every round
# based on prediction, max_odd, Consensus Probability and amount of capital to bet
# input in Round (Matchweek), method ("poisson", "merson", "random") and Amount of available capital
betting_round <- function (round, method, capital){
  total_return = 0
  
  round_data <- df_prediction %>%
    filter(Round == round) %>%
    mutate(method = method,
           predict_outcome = ifelse(method == "random", sample(c("H", "D", "A"),n(), replace = TRUE),
                            ifelse(method == 'poisson', poisson_predict, Merson_predict)))
  no_matches = dim(round_data)[1]
  round_data$max_odd <- sapply(1:no_matches, function(x) row_max_prob(round_data, x, TRUE))
  round_data$prob <- sapply(1:no_matches, function(x) row_max_prob(round_data, x, FALSE))
  
  round_data <- round_data %>%
    mutate (fraction = ((prob*max_odd - (1-prob))/max_odd),
            f_normalize = fraction/sum(fraction, na.rm = TRUE),
            bet_amount = f_normalize * capital,
            payoff = ifelse(FTR == predict_outcome, bet_amount*max_odd, 0),
            profit = payoff-bet_amount)
  
  return (sum(round_data$profit, na.rm = TRUE))
}

# inititate a table to store return result
# remove Paul's Merson bet
return_table <- data.frame(round = 0:30,
                           Poisson = rep(0,31),
                           random_bet = rep(0,31))

return_table[1,c("Poisson", "random_bet")] <- rep(1000,2)

for (i in 1:30){
  Poisson_return <- betting_round(i, "poisson",1000/30)
  random_return  <- betting_round(i, "random", 1000/30)
  #Merson_return  <- betting_round(i, "Merson",1000/30)

  return_table[i+1,"Poisson"]    <- Poisson_return
  return_table[i+1,"random_bet"] <- random_return
  #return_table[i+1,"Merson_bet"] <- Merson_return
}

# we are interested the change in the portfolio overtime
return_table$Poisson    <- cumsum(return_table$Poisson)
return_table$random_bet <- cumsum(return_table$random_bet)
#return_table$Merson_bet <- cumsum(return_table$Merson_bet)

return_table %>% 
  gather("method", "value", -round) %>%
  mutate(method = factor(method, levels = c('Poisson', 'random_bet'),
                         labels = c('Poisson prediction', 'random prediction'))) %>%
  ggplot(aes(x=round, y=value, group=method)) +
  geom_line(aes(color=method)) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  ggtitle("Portfolio value at the end of every matchweek") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 18, family="xkcd"),
        text=element_text(size = 13, family="xkcd"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12)) +
  ylab('Portfolio total value in dollars') +
  xlab ('Matchweek')


#############
# Extra note
# If you can invest as much as you want with $1000 buffer
# meaning that you set out to invest 1000/30 per round
# but if Kelly criterion asks for more, you can till afford it
############

##### Method to combine Kelly criterion and odds
betting_round <- function (round, predict_method, capital){
  total_return = 0
  
  round_data <- df_prediction %>%
    filter(Round == round)
  
  for (i in 1:dim(round_data)[1]){
    predict = ifelse(predict_method == "random", sample(c("H", "D", "A"),1), 
                     round_data[i,predict_method])
    if (is.na(predict)){
      total_return = total_return
    }
    else{
      # once I have the prediction, I find the one with the highest odd
      odds = as.vector (round_data[i, paste0(betting_house, predict)])
      odd = max(odds)
      
      predict_prob =  1/rowMeans(round_data[i,paste0(betting_house, predict)])
      bet_amount = ((predict_prob*odd - (1-predict_prob))/odd)*capital
      
      total_return = ifelse(round_data[i, "FTR"] == predict, 
                            total_return + bet_amount*(odd-1), 
                            total_return - bet_amount)
    }
  }
  return (total_return)
}
