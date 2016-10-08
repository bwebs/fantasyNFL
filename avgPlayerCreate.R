print('Creating avgPlayer and applying Bonuses')

if (fdPoints) {
  print('Applying FanDuel weights')
  num_weights = length(unique(playerScrapeMerge$Source))
  playerScrapeMerge$Weights = ifelse(playerScrapeMerge$Source=='FanDuel', fdPoints.weight, (1-fdPoints.weight)/(num_weights-1) )
} else { playerScrapeMerge$Weights=1 }

#avgPlayer = ddply(playerScrapeMerge, .(Key), summarize, mean=mean(PredictedPoints), min=min(PredictedPoints), max=max(PredictedPoints), freq=length(PredictedPoints))
avgPlayer = ddply(playerScrapeMerge, .(Key), function(X) 
  data.frame(
    mean.weight=weighted.mean(X$PredictedPoints,X$Weights),
    mean.nonweight = mean(X$PredictedPoints),
    min=min(X$PredictedPoints),
    max=max(X$PredictedPoints),
    freq=length(X$PredictedPoints)
  ))

avgPlayer = merge(avgPlayer, fdSalary, by = 'Key', all = TRUE)
#str(avgPlayer)
#sum(is.na(avgPlayer$mean))
#sum(is.na(avgPlayer$Salary))
MISSING = avgPlayer[is.na(avgPlayer$mean.weight) | is.na(avgPlayer$Salary),]
write.csv(MISSING, paste0('Exports/week',week.prompt,'_MISSING.csv'))

#REMOVE DOUBTFUL, OUT, IR, leave probable and questionable
avgPlayer = avgPlayer[!(avgPlayer$`Injury Indicator` %in% c('O','IR','D')),]

#Remove MISSING
avgPlayer = avgPlayer[!(is.na(avgPlayer$mean.weight) | is.na(avgPlayer$Salary)),]

#length(avgPlayer$mean)
avgPlayer = avgPlayer[avgPlayer$mean.weight > 1,]
avgPlayer = avgPlayer[order(-avgPlayer$Salary,-avgPlayer$mean.weight),]

#temp=avgPlayer // avgPlayer=temp

if (madBonus) {avgPlayer = addMaddenBonus(avgPlayer)}
if (homeBonus) {avgPlayer = addHomeBonus(avgPlayer)}
if (dlBonus) {avgPlayer = deepLearningBonus(avgPlayer)}

bonuses = ifelse(rep(dlBonus,length(avgPlayer$Key)),avgPlayer$deepBonus,0) + 
  ifelse(rep(homeBonus,length(avgPlayer$Key)),avgPlayer$bonusPositionHome,0) + 
  ifelse(rep(madBonus,length(avgPlayer$Key)), avgPlayer$Bonus, 0 )

avgPlayer$mean = (1+bonuses)*avgPlayer$mean.weight

write.csv(avgPlayer,paste0('Exports/week',week.prompt,'_avgPlayerWithMaddentest.csv'), row.names=FALSE)
