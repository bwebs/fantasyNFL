# 1.1 Verify
#Just checking QBs from ESPN
dim(results)
dim(playerScrape)
espnURL
sum(!(playerScrape$Name %in% results$player))
sum(!(results$player %in% playerScrape$Name))
playerScrape$Name[!(playerScrape$Name %in% results$player)]
playerScrape$Name[!(results$player %in% playerScrape$Name)]
# Matt Schaub shows up in both, but name doesn't match ?