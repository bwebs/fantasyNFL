#http://blog.stattleship.com/stop-scraping-for-sports-data-start-stattleshipping/

token= '6384d1c63f4f81e041721964bfe88d92'

devtools::install_github("stattleship/stattleship-r")
library(stattleshipR)
set_token(token) 

sport='football'
league <- 'nfl'  
ep <- 'game_logs'  

q_body <- list(team_id='nfl-chi', status='ended', interval_type='regularseason')

gls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)
length(gls)
str(gls)
player_logs=do.call('rbind', lapply(gls, function(x) x$players))
#game_logs<-do.call('rbind', lapply(gls, function(x) x$game_logs)) 
colnames(player_logs)  
str(player_logs)
