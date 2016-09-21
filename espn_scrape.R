#install.packages('rvest')
#install.packages('stringr')
#install.packages('tidyr')

library(rvest)
library(stringr)
library(tidyr)

urlbase = 'http://games.espn.com/ffl/tools/projections?'
am = '&'
slotCategoryId = data.frame(value=c(0,2,4,6,17,16), pos=c("QB",'RB','WR','TE','K','D')) #QB
startIndex = 0 #
increaseIndex = 40 #each page has 40 records

for (j in 1:length(slotCategoryId$value)) {
  #for (j in 1:1) {
  
  for (i in 0:50) { #max 50 pages?!?
    espnURL = paste(
      urlbase,
      'slotCategoryId=',
      slotCategoryId$value[j],
      am,
      'startIndex=',
      as.character(startIndex + increaseIndex * i),
      sep = ''
    )
    webpage = read_html(espnURL)
    sb_table <- html_nodes(webpage, 'table')
    sb <- html_table(sb_table, fill = TRUE)[[1]]
    if (length(sb$X1) == 2) {
      break
    } else {
      sb = sb[-(1:2), (1:13)]
      names(sb) = sb[1, ]
      sb = sb[-1, ]
      head(sb)
      if (i == 0) {
        df = sb
      } else {
        df = rbind(df, sb)
      }
    }
  }
  
  #str(df)
  df$id = ''
  
  if (j==6) { 
    df$Name = substr(df[,2],1,regexpr('D/ST', df[,2])-1)
  } else {
    df$Name = substr(df[,2],1,regexpr(',', df[,2])-1)
  }
  df$Position = slotCategoryId$pos[j]
  df$PassingCompletions = as.double(sub('--','0',substr(df[,3],0,regexpr('/', df[,3])-1)))
  df$PassingAttempts = as.double(sub('--','0',str_sub(df[,3],regexpr('/', df[,3])+1,-1)))
  #df[,c(3,14,15,16)]
  df$PassingYards = as.double(sub('--','0',df[,4]))
  df$PassingTD = as.double(sub('--','0',df[,5]))
  df$Interceptions = as.double(sub('--','0',df[,6]))
  df$RushingAttempts = as.double(sub('--','0',df[,7]))
  df$RushingYards = as.double(sub('--','0',df[,8]))
  df$RushingTD = as.double(sub('--','0',df[,9]))
  df$Receptions = as.double(sub('--','0',df[,10]))
  df$ReceptionYards = as.double(sub('--','0',df[,11]))
  df$ReceptionTD = as.double(sub('--','0',df[,12]))
  df$MiscFumTD = -1
  df$Misc2Point = -1
  df$FumblesLost = -1
  df$Source = 'ESPN'
  df$PredictedPoints = as.double(sub('--','0',df[,13])) / 16 # Season Stats

  
  df= df[,14:32]
  

  playerScrape = rbind(playerScrape, df)

}
#write.csv(playerScrape,"resultsTest.csv")



