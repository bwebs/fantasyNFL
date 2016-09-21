require(httr)
require(XML)
require(RCurl)

catPos = data.frame(id = c(8,16,1,4,64,32768), pos = c('QB','RB','WR','TE','K','D'))

for ( j in 1:length(catPos$pos)) {
  for (page in 1:10000) {
    test = getURL(paste('http://www.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page=',page,'&position=',catPos$id[j],'&split=4', sep=''))
    doc = htmlParse(test)
    table = readHTMLTable(doc, trim=TRUE)
    sb = data.frame(lapply(table$playerTable, as.character), stringsAsFactors=FALSE)
    #print(length(names(sb)))
    # print(paste(j,',',page,',',length(names(sb))))
    if (length(names(sb)) == 1) { break }
    lSB = length(names(sb))
    sb = sb[,c(1,2,lSB)]
    for (i in 4:17) { sb[,i] = -1 }
    
    
    #if (length(names(sb)) == 18) {sb = sb[,c(1:17)]} #I need to look into these...
    #if (length(names(sb)) == 22) {sb = sb[,c(1:7,9:14,19:22)] }
    names(sb) = c('NameStatus','Game','PredictedPoints','PassingTD','PassingYards','PassingAttempts','PassingCompletions','Interceptions'
                  ,'RushingTD','RushingYards','RushingAttempts'
                  ,'ReceptionTD','ReceptionYards','Receptions'
                  ,'Misc2Point','MiscFumTD','FumblesLost')
    
    if (page==1 & j==1) { df = sb }  else { df = rbind(df,sb) }
    
    #str(df)
    removeChars = c('\r','\n','\t',',,,',',,',',,')
    for (i in 1:length(removeChars) ) { df$NameStatus = gsub(removeChars[i],",",df$NameStatus) }
    
  }
}
  df$id = ''
  df$Name =  substr(df$NameStatus, 1, regexpr(",",df$NameStatus)-1)
  df$Name = ifelse (df$Name == 'New York',substr(df$NameStatus,regexpr(",\\(",df$NameStatus)+2,regexpr("-",df$NameStatus)-2 ), df$Name)
  df$Position = substr(df$NameStatus, regexpr(" - ",df$NameStatus)+3,regexpr(")",df$NameStatus)-1)
  df$Position = ifelse(df$Position=='D/ST','D',df$Position)
  df$Source = 'Fox'
  df$PredictedPoints = as.double(sub('--','0',df$PredictedPoints))
  # df$PassingTD = as.double(sub('--','0',df$PassingTD))
  # df$PassingYards = as.double(sub('--','0',df$PassingYards))
  # df$PassingAttempts = as.double(sub('--','0',df$PassingAttempts))
  # df$PassingCompletions = as.double(sub('--','0',df$PassingCompletions))
  # df$Interceptions = as.double(sub('--','0',df$Interceptions))
  # df$RushingTD = as.double(sub('--','0',df$RushingTD))
  # df$RushingYards = as.double(sub('--','0',df$RushingYards))
  # df$RushingAttempts = as.double(sub('--','0',df$RushingAttempts))
  # df$ReceptionTD = as.double(sub('--','0',df$ReceptionTD))
  # df$ReceptionYards = as.double(sub('--','0',df$ReceptionYards))
  # df$Receptions = as.double(sub('--','0',df$Receptions))
  # df$Misc2Point = as.double(sub('--','0',df$Misc2Point))
  # df$MiscFumTD = as.double(sub('--','0',df$MiscFumTD))
  # df$FumblesLost = as.double(sub('--','0',df$FumblesLost))
  # df$PredictedPoints = as.double(sub('--','0',df$PredictedPoints))
  
df = df[,3:21]
  #names(df) %in% names(playerScrape)
  


  playerScrape = rbind(playerScrape, df)

