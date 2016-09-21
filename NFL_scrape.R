library(rvest)
library(stringr)
library(tidyr)


urlbase = 'http://fantasy.nfl.com/research/projections?'
am = '&'
slotCategoryId = data.frame(value=c(1,2,3,4,7,8), pos=c("QB",'RB','WR','TE','K','D')) #QB
startIndex = 1 #
increaseIndex = 25 #each page has 25 records
week = 2

for (j in 1:length(slotCategoryId$value)) {

for (k in 1:17) {    
  for (i in 0:50) { #max 50 pages?!?
    espnURL = paste(
      urlbase,
      'position=',
      slotCategoryId$value[j],
      '&sort=projectedPts&statCatery=projectedStats&statSeason=2016&statType=weekProjectedStats&offset=',
      as.character(startIndex + increaseIndex * i),
      '&statweek=', 
      week,
      sep = ''
    )
    webpage = read_html(espnURL)
    sb_table <- html_nodes(webpage, 'table')
    sb <- try(html_table(sb_table, fill = TRUE)[[1]], silent=TRUE)
    
    if (class(sb) == "try-error") { # Break condition for when no more players on page
      break
    } else {
      names(sb) = paste(names(sb),sb[1, ],sep='-')
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
  if (j < 5) {
    df$id = ''
    df$Name = substr(df[,1],0,regexpr(slotCategoryId$pos[j], df[,1])-2)
    df$Position = slotCategoryId$pos[j]
    df$PassingCompletions = -1
    df$PassingAttempts = -1
    #df[,c(3,14,15,16)]
    df$PassingYards = as.double(sub('-','0',df[,3]))
    df$PassingTD = as.double(sub('-','0',df[,4]))
    df$Interceptions = as.double(sub('-','0',df[,5]))
    df$RushingAttempts = -1
    df$RushingYards = as.double(sub('-','0',df[,6]))
    df$RushingTD = as.double(sub('-','0',df[,7]))
    df$Receptions = -1
    df$ReceptionYards = as.double(sub('-','0',df[,8]))
    df$ReceptionTD = as.double(sub('-','0',df[,9]))
    df$MiscFumTD = as.double(sub('-','0',df$'Misc-FumTD'))
    df$Misc2Point = as.double(sub('-','0',df$'Misc-2PT'))
    df$FumblesLost = as.double(sub('-','0',df$'Fum-Lost'))
    df$Source = 'NFL'
    df$PredictedPoints = as.double(sub('-','0',df[,13]))
    df= df[,14:32]
  } else if (j==5) {
    df$id = ''
    df$Name = substr(df[,1],0,regexpr(slotCategoryId$pos[j], df[,1])-2)
    df$Position = slotCategoryId$pos[j]
    df$PassingCompletions = -1
    df$PassingAttempts = -1
    #df[,c(3,14,15,16)]
    df$PassingYards = -1
    df$PassingTD = -1
    df$Interceptions = -1
    df$RushingAttempts = -1
    df$RushingYards = -1
    df$RushingTD = -1
    df$Receptions = -1
    df$ReceptionYards = -1
    df$ReceptionTD = -1
    df$MiscFumTD = -1
    df$Misc2Point = -1
    df$FumblesLost = -1
    df$Source = 'NFL'
    df$PredictedPoints = as.double(sub('-','0',df[,9]))
    df= df[,10:28]
    
  } else {
    
    df$id = ''
    df$Name = substr(df[,1],0,regexpr('DEF', df[,1])-2)
    df$Position = slotCategoryId$pos[j]
    df$PassingCompletions = -1
    df$PassingAttempts = -1
    #df[,c(3,14,15,16)]
    df$PassingYards = -1
    df$PassingTD = -1
    df$Interceptions = -1
    df$RushingAttempts = -1
    df$RushingYards = -1
    df$RushingTD = -1
    df$Receptions = -1
    df$ReceptionYards = -1
    df$ReceptionTD = -1
    df$MiscFumTD = -1
    df$Misc2Point = -1
    df$FumblesLost = -1
    df$Source = 'NFL'
    df$PredictedPoints = as.double(sub('-','0',df$'Fantasy-Points'))
    df= df[,(ncol(df)-18):ncol(df)]
  }
  

  playerScrape = rbind(playerScrape, df)
}
}
write.csv(playerScrape,"resultsTest.csv")


