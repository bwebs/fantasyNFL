library(igraph)
library(shiny)
library(shinydashboard)


source('functions.R')

skip=FALSE

if (!skip) {

source('FanDuel_scrape.R')
source('scrape_fantasypros.R')




  
  playerScrape = abbreviateTeams(playerScrape)
  playerScrape = addNameRM(playerScrape)
  playerScrape = createKey(playerScrape)
  playerScrapeMerge = merge(playerScrape, fdSalary[,c('Key','Salary','Team','Opponent')], by='Key')
  
  #testing 10% of data
  playerScrapeMerge = playerScrapeMerge[sample.int(length(playerScrapeMerge$Key), floor(length(playerScrapeMerge$Key)/10), replace=FALSE),]
  save(playerScrapeMerge, file='pSM.Rda')
}

load('pSM.Rda')
