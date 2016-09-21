abbreviateTeams = function (pS) {
  #pS = playerScrape 
  if (!exists('Name', pS)) {
    stop("Could not find Name in dataframe")
  }

  translate = read.csv('Input/teamAbbr.csv', stringsAsFactors=FALSE)
  checkIn = which(pS$Name %in% translate$Name)
  for (i in checkIn ) {pS$Name[i] = translate$Abbreviation[which(translate$Name %in% pS$Name[i])[1]] }
  
  checkIn = which(pS$Team %in% translate$Name)
  for (i in checkIn ) {pS$Team[i] = translate$Abbreviation[which(translate$Name %in% pS$Team[i])[1]] }
  
  return(pS)
}

addNameRM = function(ps) {
  #ps=playerScrape
    temp = strsplit(gsub("[^[:alnum:] ]", "", ps$Name), " +")
  for (i in length(temp):1) {temp[i] = tolower(paste(unlist(temp[i]),collapse=''))}
  ps$NameRM = paste(temp, sep='')
  return(ps)
}
  
  

createKey = function (ps) {
  #ps = playerScrape
  
  ps$Key = paste(ps$Position,ps$NameRM, sep='')
  return(ps)
}

addTeams = function (ps, fd) {
  #ps = playerScrape
  if (!exists('Team',fd)) {stop("Could not find Team in Salary dataframe") }
  if (!exists('Key',fd)) {stop("Could not find Key in Salary dataframe") }
  
  ps = merge(ps, fdSalary[,c('Key','Team')], by = 'Key',sort=FALSE, all.x=TRUE)
}

convertPositions = function (ps) {
  #ps = mad
  translate = read.csv('Input/positionTranslation.csv', stringsAsFactors=FALSE)
  if (!exists('Position',ps)) {stop("Could not find Position in dataframe") }
  
  checkIn = which(ps$Position %in% translate$position)
  for (i in checkIn ) {ps$Position[i] = translate$translatesTo[which(translate$position %in% ps$Position[i])[1]] }
  
  return(ps)
  }