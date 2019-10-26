library (dplyr)
source ('clean_data.R')

# function to simplify result
# from scoreline to who wins the match, H (Home), A(Away) or D(Draw)
result_calc <- function (h_goal, a_goal){
  result = ifelse(h_goal == a_goal, 'D', ifelse(h_goal > a_goal, 'H', 'A'))
  return (result)
}

# function to calibrate results
# The idea is to make sure that if Probability of wining of Home and Away is tight
# e.g: 0.451(H) vs 0.447 (A)
# then it should be thought as a draw
result_calibrate <- function(prob_h, prob_d, prob_a){
  result = ifelse(abs(prob_h - prob_a) < 0.01, "D",
                  ifelse (prob_h == pmax(prob_d,prob_h,prob_a), "H", 
                          ifelse(prob_d == pmax(prob_h,prob_d,prob_a), "D", "A" )))
  return (result)
}


# get most frequent score line of a match after n, sim time
get_score <- function (home, away, nsim){
  # try to get from history, pair
  subset <- hist_pair.pl[ which( hist_pair.pl$HomeTeam ==home | hist_pair.pl$AwayTeam ==away), ]
  # more efficient code, no need to retract back to dataframe many times
  ave_h_s = subset$ave_home_scored[1]
  ave_a_s = subset$ave_away_scored[1]
  
  t_ave_h_s = ave[ave$Team == home,]$ave_scored_h
  t_ave_a_c = ave[ave$Team == away,]$ave_conceded_a
  t_ave_h_c = ave[ave$Team == home,]$ave_conceded_h
  t_ave_a_s = ave[ave$Team == away,]$ave_scored_a
  result = character(length(nsim))
  for (i in 1:nsim){
    if ((dim(subset)[1] == 1) & (subset$match[1] > 3)){
      h_scored = rpois(1, ave_h_s)
      a_scored = rpois(1, ave_a_s)
    }
    # if we have no historical result of the match
    else{
      # take into account both attacking stat of home and defense stats of away
      h_scored = rpois(1, 1/2 * (t_ave_h_s + t_ave_a_c))
      a_scored = rpois(1, 1/2 * (t_ave_a_s + t_ave_h_c))
    } 
    result[i] = result_calc(h_scored, a_scored)
  }
  result_tab  = table(result)/nsim
  return (c(result_tab['H'], result_tab['D'], result_tab['A']))
}

nsim = 10000
matches <- mapply(get_score, new_season$HomeTeam, new_season$AwayTeam, nsim, SIMPLIFY = FALSE)
new_season$H <- sapply(matches, function(x) x[1])
new_season$D <- sapply(matches, function(x) x[2])
new_season$A <- sapply(matches, function(x) x[3])

df_prediction <- new_season %>%
  mutate(poisson_predict = result_calibrate(H,D,A))
                       
# The data about Paul Merson's prediction seems to get lost somehow                       
#df_prediction <- new_season %>%
#  mutate(poisson_predict = result_calibrate(H,D,A),
#         Merson_predict = result_calc(Merson.H, Merson.A))
