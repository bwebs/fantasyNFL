
library(shiny)
library(shinydashboard)
library(plyr)
library(RCurl)
library(XML)
library(jsonlite)
library(DT)

source('functions.R')
source('CreateBuckets_shiny.R')
source('madden_adjustments.R')
source('2015_FP.R')
source('injuryTest.R')
source('FanDuel_Scrape.R')

skip=FALSE
debug=FALSE

if (!skip) {

source('scrape_fantasypros.R')




  
  playerScrape = abbreviateTeams(playerScrape)
  playerScrape = addNameRM(playerScrape)
  playerScrape = createKey(playerScrape)
  playerScrapeMerge = merge(playerScrape, fdSalary[,c('Key','Salary','Team','Opponent')], by='Key')
  
  #testing 10% of data

  if (debug){ playerScrapeMerge = playerScrapeMerge[sample.int(length(playerScrapeMerge$Key), floor(length(playerScrapeMerge$Key)/10), replace=FALSE),] }
  save(playerScrapeMerge, file='pSM.Rda')
}

if (skip) { load('pSM.Rda') }