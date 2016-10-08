
#source('espn_Scrape.R')
#source('NFL_scrape.R')

#source('fox_scrape.R')
source('scrape_fantasypros.R')
source('FanDuel_Scrape.R')

playerScrape = abbreviateTeams(playerScrape)
playerScrape = addNameRM(playerScrape)
playerScrape = createKey(playerScrape)


#playerScrape = addTeams(playerScrape, fdSalary)

playerScrapeMerge = merge(playerScrape, fdSalary[,c('Key','Salary','Team','Opponent')], by='Key')



#write.csv(playerScrapeMerge[,c('Key','Name','Position','Source','PredictedPoints','Salary','Team')], "Exports/databaseTest.csv", row.names=FALSE)
source('avgPlayerCreate.R')
source('CreateBuckets.R')

