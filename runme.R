#install.packages(c('rvest','stringr','tidyr','RCurl','XML','jsonlite','httr'), requirements=TRUE)

source('functions.R')

playerScrape = data.frame(id=as.character(),  Name=as.character(), Position=as.character(), Source=as.character()
                          , PassingTD=as.double(), PassingYards=as.double()
                          , PassingAttempts=as.double(), PassingCompletions=as.double()
                          , Interceptions=as.double(), RushingTD=as.double(), RushingYards=as.double()
                          , RushingAttempts=as.double(), ReceptionTD=as.double(), ReceptionYards=as.double()
                          , Receptions=as.double(), Misc2Point=as.double(), MiscFumTD=as.double()
                          , FumblesLost=as.double(),PredictedPoints=as.double(), stringsAsFactors = FALSE)
#source('espn_Scrape.R')
#source('NFL_scrape.R')
source('FanDuel_scrape.R')
#source('fox_scrape.R')
source('scrape_fantasypros.R')

playerScrape = abbreviateTeams(playerScrape)
playerScrape = addNameRM(playerScrape)
playerScrape = createKey(playerScrape)

#playerScrape = addTeams(playerScrape, fdSalary)

playerScrapeMerge = merge(playerScrape, fdSalary[,c('Key','Salary','Team','Opponent')], by='Key')
write.csv(playerScrapeMerge[,c('Key','Name','Position','Source','PredictedPoints','Salary','Team')], "Exports/databaseTest.csv", row.names=FALSE)

source('CreateBuckets.R')

