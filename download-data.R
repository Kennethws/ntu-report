#####################################################################
###------------------------------------------------------------------
### Topic : NTU Research Report
### Date  : 2020-06-17
### Author: Chao Wang
###------------------------------------------------------------------
#####################################################################

library(rvest)

nba.scrape <- function(n) {
  team <- matrix(0, 0, 24)
  
  for (i in 1:n) {
    pagenum <- 2020 - i
    url <- paste0('https://www.basketball-reference.com/playoffs/NBA_', pagenum, '.html')
    # scrape html
    h <- read_html(url)
    
    # only need to assign name once
    if (i == 1) {
      name <- h %>% 
        html_nodes('.center+ .center , .left+ .center , .sort_default_asc.left') %>% 
        html_text()
    }
    
    tab <- h %>% 
      html_nodes('#team-stats-per_game td') %>% 
      html_text() %>% matrix(nrow = 17, byrow = T)
    tab <- tab[-17,]
    
    team <- rbind(team, tab)
  }
  
  # set names and add rank column
  colnames(team) <- name
  
  team <- as_tibble(team) %>% 
    mutate_at(-1, as.numeric) %>% 
    mutate(rank = rep(1:16, n))
  
  return(team)
}

team <- nba.scrape(22)
