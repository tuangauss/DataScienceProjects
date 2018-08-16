library (dplyr)
library (ggplot2)
library (extrafont)
# probably need to run in a mac
download.file("http://simonsoftware.se/other/xkcd.ttf",
              dest="xkcd.ttf", mode="wb")
font_import(path = ".", pattern="xkcd")
fonts()
loadfonts()

# extract historic results
history <- read.csv("history.csv", stringsAsFactors = FALSE)

# get info from the 2010 up to 2018
seasons <- sapply(10:17, function(x) paste0(2000+x,'-',x+1))


graph_func <- function(season){
  data <- history %>% 
    filter (Season %in% season, div == 'E0') %>%
    mutate (total = FTAG + FTHG)
  
  ave_Score <- mean(data$total)
  
  prob_data <- data %>%
    group_by(total) %>%
    summarize (prob = n()/nrow(data))
  
  ggplot(data=prob_data, aes(x=total, y=prob)) +
    geom_bar(stat="identity", color="blue", fill="grey") +
    scale_x_continuous(breaks=seq(0,10,1)) +
    geom_line(aes(x = total, y = dpois(x=total, lambda = ave_score)), 
              col = "red", size = 0.5) +
    geom_point(aes(x = total, y = dpois(x=total, lambda = ave_score)), 
               col = "black", size = 3) +
    ggtitle("Probability of total goal per game") +
    theme(axis.line = element_line(size=1, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title=element_text(size = 20, family="xkcd-Regular"),
          text=element_text(size = 16, family="xkcd-Regular"),
          axis.text.x=element_text(colour="black", size = 12),
          axis.text.y=element_text(colour="black", size = 12))
}

graph_func(seasons)
graph_func(c('2017-18'))
