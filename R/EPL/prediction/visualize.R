library (dplyr)
library (ggplot2)
library (xkcd)
library (extrafont)

download.file("http://simonsoftware.se/other/xkcd.ttf",
              dest="xkcd.ttf", mode="wb")
system("cp xkcd.ttf ~/Library/Fonts")
font_import(path="~/Library/Fonts", pattern = "xkcd", prompt=FALSE)
fonts()
fonttable()
if(.Platform$OS.type != "unix") {
  ## Register fonts for Windows bitmap output
    loadfonts(device="win")
  } else {
  loadfonts()
}

# extract historic results
history <- read.csv("https://raw.githubusercontent.com/tuangauss/Various-projects/master/data/history.csv", stringsAsFactors = FALSE)

# get info from the 2010 up to 2018
seasons <- sapply(10:17, function(x) paste0(2000+x,'-',x+1))


graph_func <- function(season){
  if (season[1] == "2017-18"){
    title = "Last season: 2017-2018"
  }
  else{
    title = "From 2010-11 to 2017-18"
  }
  data <- history %>% 
    filter (Season %in% season, div == 'E0') %>%
    mutate (total = FTAG + FTHG)
  
  ave_score <- mean(data$total)
  
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
    ggtitle(title) + labs (x = "Total Goal", y = "Probability") +
    theme_xkcd()
}

graph_func(seasons)
graph_func(c('2017-18'))
