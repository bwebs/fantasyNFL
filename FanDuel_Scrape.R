#fanduelFile = 'FanDuel-NFL-2016-10-09-16502-lineup-upload-template.csv'

print(paste0('Using FanDuel file: ',fanduelFile))

fd = read.csv(paste0('FanDuel/',fanduelFile), stringsAsFactors = FALSE)

#str(fd)
fdTemplate = fd[,1:9]

fd = fd[-1:-5,11:23]
names(fd) = fd[1,]
fd = fd[-1,]
fd$Name = paste(fd$`First Name`,fd$`Last Name`,sep=' ')
#fd$Position = slotCategoryId$pos[j]
fd$PassingCompletions = -1
fd$PassingAttempts = -1
#df[,c(3,14,15,16)]
fd$PassingYards = -1
fd$PassingTD = -1
fd$Interceptions = -1
fd$RushingAttempts = -1
fd$RushingYards = -1
fd$RushingTD = -1
fd$Receptions = -1
fd$ReceptionYards = -1
fd$ReceptionTD = -1
fd$MiscFumTD = -1
fd$Misc2Point = -1
fd$FumblesLost = -1
fd$Source = 'FanDuel'
for (i in 1:length(fd$Position)) {
  fd$Home[i] = ifelse(regexpr("@",fd$Game[i]) - regexpr(fd$Team[i],fd$Game[i]) < 0, TRUE, FALSE) 
}

fd$PredictedPoints = as.double(fd$FPPG)
fd$Salary = as.integer(fd$Salary)


#c('Id','Name','Position','Source','Injury Indicator','Injury Details','PredictedPoints','Salary','Team','Opponent','Home') %in% colnames(fd)
fdSalary = fd[,c('Id','Name','Position','Source','Injury Indicator','PredictedPoints','Salary','Team','Opponent','Home')] #,'Injury Details'
#str(fdSalary)
fdSalary = abbreviateTeams(fdSalary)
fdSalary = addNameRM(fdSalary)
fdSalary = createKey(fdSalary)
fdSalary$Opponent = ifelse(fdSalary$Opponent=='JAC','JAX',fdSalary$Opponent)
fdSalary$Team = ifelse(fdSalary$Team=='JAC','JAX',fdSalary$Team)

#fd= fd[,c(1,14,3,15:30)]
names(fd)[1]='id'

if (fdPoints) {
  print('Adding FanDuel points')
  if (!exists('playerScrape')) {
    playerScrape = fd
  } else {
    playerScrape = rbind(playerScrape, fd[,names(playerScrape)])
  }
}

