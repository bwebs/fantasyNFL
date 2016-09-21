library(reshape)
#library(dplyr)
library(plyr)
str(playerScrape)

playerScrape2 = playerScrape
playerScrape2 = playerScrape[playerScrape$PredictedPoints>3,] # lop of low scorers
#playerScrape2$PredictedPoints = as.double(playerScrape2$PredictedPoints)

dataCheck = ddply(playerScrape2, .(Name, Position), summarize, mean=mean(PredictedPoints), min=min(PredictedPoints), max=max(PredictedPoints), freq=length(PredictedPoints))
dataCheck$range = dataCheck$max - dataCheck$min
write.csv(dataCheck,'Exports/dataCheck.csv')

dataCheck2 = ddply(playerScrape2, .(Source, Position), summarize, mean=mean(PredictedPoints), min=min(PredictedPoints), max=max(PredictedPoints), freq=length(PredictedPoints))
dataCheck2$range = dataCheck2$max - dataCheck2$min
write.csv(dataCheck2,'Exports/dataCheck2.csv')

cast(dataCheck2, Position ~ Source, value='mean', add.missing = TRUE)
table(playerScrape2$Source,playerScrape2$Position)

table(playerScrape2$Position, playerScrape2$Source)
