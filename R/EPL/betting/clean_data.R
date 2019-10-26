########################################
# scripts to clean data to usable format
# source:
# - fixtures.csv: dedicatedexcel.com
# - Historical results: https://www.kaggle.com/thefc17/epl-results-19932018
#########################################
library (dplyr)

link_fixture = "https://raw.githubusercontent.com/tuangauss/DataScienceProjects/master/data/fixtures.csv"
link_history = "https://raw.githubusercontent.com/tuangauss/DataScienceProjects/master/data/history.csv"

fixtures <- read.csv(link_fixture, stringsAsFactors = FALSE)

# get the team
teams <- unique(fixtures$HOME.TEAM)

# extract historic results
history <- read.csv(link_history, stringsAsFactors = FALSE)

# get info from the 2010 up to 2018
seasons <- sapply(10:17, function(x) paste0(2000+x,'-',x+1))

recent.pl <- history %>%
  filter(Season %in% seasons, div == 'E0')

# because the two data comes from different source, so the teams name don't match
teams[!teams %in% recent.pl$HomeTeam]
unique(recent.pl$HomeTeam)

# now we need to fix it
pair_fix <- list(c('Manchester United', 'Man United'),
                 c('Newcastle United', 'Newcastle'),
                 c('Huddersfield Town', 'Huddersfield'),
                 c('Wolverhampton Wanderers', 'Wolves'),
                 c('Cardiff City', 'Cardiff'),
                 c('Leicester City', 'Leicester'),
                 c('Tottenham Hotspur', 'Tottenham'),
                 c('West Ham United', 'West Ham'),
                 c('Manchester City', "Man City"),
                 c('Brighton and Hove Albion', 'Brighton'))

# fix the recent.pl dataset
# for name-conformity
for (i in 1:length(pair_fix)){
  recent.pl <- recent.pl %>%
    mutate(HomeTeam = replace(HomeTeam,
                              HomeTeam == pair_fix[[i]][2],
                              pair_fix[[i]][1]),
           AwayTeam = replace(AwayTeam,
                              AwayTeam == pair_fix[[i]][2],
                              pair_fix[[i]][1]))
}


# a bland average dataframe
ave_home <- recent.pl %>%
  group_by(HomeTeam) %>%
  summarize (ave_scored_h = mean(FTHG), ave_conceded_h = mean(FTAG)) %>%
  filter (HomeTeam %in% teams) %>% rename(Team = HomeTeam)

ave_away <- recent.pl %>%
  group_by(AwayTeam) %>%
  summarize (ave_scored_a = mean(FTAG), ave_conceded_a = mean(FTHG)) %>%
  filter (AwayTeam %in% teams)  %>% rename(Team = AwayTeam)

ave <- merge(ave_home, ave_away, by = 'Team')


# more precise result with pairwise
hist_pair.pl <- recent.pl %>%
  group_by(HomeTeam, AwayTeam) %>%
  filter (HomeTeam %in% teams, AwayTeam %in% teams) %>%
  summarize (match = n(), ave_home_scored = mean(FTHG), ave_away_scored = mean(FTAG))

# data set for new season
# just clean the data name for readability
new_season <- fixtures %>%
  rename(HomeTeam = HOME.TEAM,
         AwayTeam = AWAY.TEAM)

# clean data form memory
rm(history, seasons, recent.pl, pair_fix, ave_home, ave_away, fixtures)
