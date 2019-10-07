library (dplyr)
source ('clean_data.R')

# get score of a match
get_score <- function (home, away){
  # try to get from history, pair
  subset <- hist_pair.pl[ which( hist_pair.pl$HomeTeam ==home & hist_pair.pl$AwayTeam ==away), ]
  # only use this method if we have at least 4 matches
  if ((dim(subset)[1] == 1) & (subset$match[1] > 3)){
    h_scored = rpois(1, subset$ave_home_scored[1])
    a_scored = rpois(1, subset$ave_away_scored[1])
  }
  # if we have no historical result of the match
  else{
    # take into account both attacking stat of home and defense stats of away
    h_scored = rpois(1, 1/2 * (ave[ave$Team == home,]$ave_scored_h +
                                 ave[ave$Team == away,]$ave_conceded_a))
    a_scored = rpois(1, 1/2 * (ave[ave$Team == away,]$ave_scored_a +
                                 ave[ave$Team == home,]$ave_conceded_h))
  }
  return (list(h_scored, a_scored))
}

rank <- function (m_result){
  table <- data.frame(name = teams,
                      goal_score = rep(0,20),
                      goal_conceded = rep(0,20),
                      point = rep(0,20))
  # loop through all the results and then update
  for (i in 1:nrow(m_result)){
    home = m_result$HOME.TEAM[i]
    away = m_result$AWAY.TEAM[i]
    h_goal = m_result$h_scored[i]
    a_goal = m_result$a_scored[i]
    
    # add goal
    table[table$name == home,]$goal_score = table[table$name == home,]$goal_score + h_goal
    table[table$name == home,]$goal_conceded = table[table$name == home,]$goal_conceded + a_goal
    table[table$name == away,]$goal_score = table[table$name == away,]$goal_score + a_goal
    table[table$name == away,]$goal_conceded = table[table$name == away,]$goal_conceded + h_goal
    
    
    # calculate point
    if (h_goal > a_goal){
      table[table$name == home,]$point = table[table$name == home,]$point + 3
    }
    else if (h_goal < a_goal){
      table[table$name == away,]$point = table[table$name == away,]$point + 3
    }
    else{
      table[table$name == home,]$point = table[table$name == home,]$point + 1
      table[table$name == away,]$point = table[table$name == away,]$point + 1
    }
  }
  
  table$goal_dif <- table$goal_score - table$goal_conceded
  table <- table[order(-table$point, -table$goal_dif, -table$goal_score), ]
  
  return (table)
}

simulate <- function(fixtures){
  matches <- mapply(get_score, fixtures$HOME.TEAM, fixtures$AWAY.TEAM, SIMPLIFY = FALSE)
  fixtures$h_scored <- unlist(sapply(matches, function(x) x[1]))
  fixtures$a_scored <- unlist(sapply(matches, function(x) x[2]))
  table <- rank(fixtures)
  return (table)
}


nsim = 10000
tabulate_data <- data.frame(name = teams,
                            champion = rep(0,20),
                            runner_up = rep(0,20),
                            top_4 = rep(0,20),
                            top_6 = rep(0,20),
                            relegate = rep(0,20))
pb <- txtProgressBar(min = 0, max = nsim, style = 3)

for (sim in 1:nsim){
  table = simulate(fixtures)
  
  first = table$name[1]
  second = table$name[2]
  first_4 = table$name[1:4]
  first_6 = table$name[1:6]
  last_3 = table$name[18:20]
  
  tabulate_data <- tabulate_data %>%
    mutate(champion = ifelse(name == first, champion+1, champion),
           runner_up = ifelse(name == second, runner_up+1, runner_up),
           top_4 = ifelse(name %in% first_4, top_4+1, top_4),
           top_6 = ifelse(name %in% first_6, top_6+1, top_6),
           relegate = ifelse(name %in% last_3, relegate+1, relegate))
  setTxtProgressBar(pb, sim)
}

# convert to percentage
tabulate_data <- tabulate_data %>%
  mutate (champion = champion/nsim,
          runner_up = runner_up/nsim,
          top_4 = top_4/nsim,
          top_6 = top_6/nsim,
          relegate = relegate/nsim)
                                     
# write result into csv
write.csv(tabulate_data, "tabulate_data.csv", row.names = FALSE)

                                   
