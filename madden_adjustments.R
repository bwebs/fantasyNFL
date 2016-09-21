addMaddenBonus = function (aP) {
  library(RCurl)
  library(XML)
  library(jsonlite)
  source('functions.R')
  source('injuryTest.R')
  
  #aP = avgPlayer
  

  mad = fromJSON('https://www.easports.com/madden-nfl/ratings/service/data?entityType=madden17_player&filter=iteration:3&limit=20000')$docs
  #mad = fromJSON('https://www.easports.com/madden-nfl/ratings/service/data?entityType=madden17_player&&limit=20000')$docs
  #temp = mad // mad=temp
  str(mad)
  names(mad)[names(mad)=='team']='Team'
  names(mad)[names(mad)=='position']='Position'
  print('1')
  mad = convertPositions(mad)
  print('2')
  mad$Name = paste(mad$firstName,mad$lastName, sep=' ')
  
  translateName = read.csv('Input/aP_mad_nameTranslation.csv', stringsAsFactors = FALSE)
  checkIn = which(mad$Name %in% translateName$Name.From.debug_mad)
  for (i in checkIn ) {mad$Name[i] = translateName$Name[which(translateName$Name.From.debug_mad %in% mad$Name[i])[1]] }
  
  
  mad = addNameRM(mad)
  mad = abbreviateTeams(mad)
  mad = createKey(mad)
  str(mad)
  
  #REMOVE PLAYERS OUT THIS WEEK HERE
  injury = injuryList()
  injury = merge(injury,fdSalary[,c('Key','Team')], by='Key', sort=FALSE, all.x=TRUE)
  #table(injury$NameRM %in% mad$NameRM)
  injury = injury[!(injury$InjuryStatus %in%  c('Out','Doubtful','Injury Reserve')),]
  
  mad = mad[!(mad$NameRM %in% injury$NameRM),]
  mad = mad[!(mad$NameRM %in% fdSalary$NameRM[fdSalary$`Injury Indicator` %in% c('O','IR','D')]),]
  
  mad = mad[order(mad$Team,mad$Position,-mad$ovr_rating),]
  mad$Rank = ave(-mad$ovr_rating, mad$Team, mad$Position, FUN=function(x) rank(x, ties.method="random"))
  
  #temp = mad // mad=temp
  
  #analysis 1
  #a1 = mad[mad$Position=="QB",]
  a.needs = data.frame(position=c('QB','WR','RB','OL','TE','DL','LB','CB','FS','SS','K'), need=c(1,3,2,5,1,4,3,3,1,1,1), stringsAsFactors = FALSE)
  teams = data.frame(Opponent=as.character(unique(mad$Team)), stringsAsFactors=FALSE)
  
  for (i in 1:length(a.needs$position)) {
    for (j in 1:a.needs$need[i]) {
      nextcol = ncol(teams)+1
      for (k in 1:length(teams$Opponent)) {
        fromMAD = which(mad$Team == teams$Opponent[k] & mad$Rank==j & mad$Position == a.needs$position[i] )[[1]]
        teams[k,nextcol] = mad$Key[fromMAD]
        teams[k,nextcol+1] = mad$ovr_rating[fromMAD]
      }
      names(teams)[c(nextcol,nextcol+1)] = c(paste(a.needs$position[i],j,'Key', sep='.'),paste(a.needs$position[i],j,'ovr', sep='.'))
    }
  }
  
  #str(teams)
  
  teams$defense   = rowMeans(teams[,
                                         c('DL.1.ovr','DL.2.ovr','DL.3.ovr','DL.4.ovr'
                                           ,'LB.1.ovr','LB.2.ovr','LB.3.ovr','CB.1.ovr'
                                           ,'CB.2.ovr','FS.1.ovr','SS.1.ovr') ])
  teams$secondary = rowMeans(teams[, c('CB.1.ovr','CB.2.ovr','CB.3.ovr'
                                               ,'FS.1.ovr','SS.1.ovr')])
  teams$v.D       = rowMeans(teams[,
                                    c('QB.1.ovr','RB.1.ovr','WR.1.ovr','WR.2.ovr'
                                      ,'TE.1.ovr','OL.1.ovr','OL.2.ovr','OL.3.ovr','OL.4.ovr') ])
  teams$v.QB      = rowMeans(teams[,c('CB.1.ovr','CB.2.ovr','SS.1.ovr','FS.1.ovr')])
  teams$v.RB      = rowMeans(teams[,c('DL.1.ovr','DL.2.ovr','DL.3.ovr','DL.4.ovr'
                                             ,'LB.1.ovr','LB.2.ovr','LB.3.ovr')])
  teams$v.TE      = rowMeans(teams[,c('LB.1.ovr','LB.2.ovr','LB.3.ovr')])
  teams$v.K       = rowMeans(teams[,c('DL.1.ovr','DL.2.ovr','DL.3.ovr','DL.4.ovr'
                                            ,'LB.1.ovr','LB.2.ovr','LB.3.ovr','CB.1.ovr'
                                            ,'CB.2.ovr','FS.1.ovr','SS.1.ovr') ])
  
  
  
  #temp = aP // aP=temp
  aP = merge(aP, mad[,c('Key','Rank')], all.x=TRUE, by='Key',sort=FALSE) #  ## Removed because missing Eli Rogers
  sum(is.na(aP$Rank))
  
  bonus.1 = .05
  bonus.2 = .05
  bonus.3 = .05
  bonus.4 = .05
  bonus.5 = .05
  bonus.6 = .05
  bonus.7 = .05
  
  aP$Bonus = 0
  for (i in 1:length(aP$Salary)) {
    oppo = which(teams$Opponent == aP$Opponent[i])
    madd = which(mad$Key == aP$Key[i])
    #print(c(oppo,madd))
    
    if (aP$Position[i]=='D' ) { #MY NOTES WERE UNCLEAR ON THIS ONE
      offensive = which(teams$Opponent == aP$Team[i])
      aP$Bonus[i] = ( mean(as.numeric(teams[offensive,
                                            c('DL.1.ovr','DL.2.ovr'
                                              ,'DL.3.ovr','DL.4.ovr'
                                              ,'LB.1.ovr','LB.2.ovr'
                                              ,'LB.3.ovr','CB.1.ovr'
                                              ,'CB.2.ovr'
                                              ,'FS.1.ovr','SS.1.ovr') ])) - mean(as.numeric(teams[oppo,
                                                                                                  c('QB.1.ovr','RB.1.ovr'
                                                                                                    ,'WR.1.ovr','WR.2.ovr'
                                                                                                    ,'TE.1.ovr','OL.1.ovr'
                                                                                                    ,'OL.2.ovr','OL.3.ovr','OL.4.ovr','K.1.ovr') ])) ) * bonus.6
      #print(paste(ap$Team[i],ap$Opponent[i],madd,offensive,aP$Bonus(i)),sep=',')
    }
    else if (length(madd)==0) {
      print(paste("Could not find",aP$Name[i],"in Madden Data",sep=' '))
    } else if (length(madd)>1) { print("More than one Key found in Madden")
    } else {
      
      if (aP$Position[i]=='QB') {
        aP$Bonus[i] = ( mad$ovr_rating[madd] - mean(as.numeric(teams[oppo,c('CB.1.ovr','CB.2.ovr','SS.1.ovr','FS.1.ovr')])) ) * bonus.1
      }
      
      else if (aP$Position[i]=='WR' & aP$Rank[i]==1) {
        aP$Bonus[i] = (mad$ovr_rating[madd] - teams[oppo,'CB.1.ovr'])*bonus.2
      }
      
      else if (aP$Position[i]=='WR' & aP$Rank[i]==2) {
        aP$Bonus[i] = (mad$ovr_rating[madd] - teams[oppo,'CB.2.ovr'])*bonus.3
      }
      
      else if (aP$Position[i]=='RB' ) {
        aP$Bonus[i] = (mad$ovr_rating[madd] - mean(as.numeric(teams[oppo,c('DL.1.ovr','DL.2.ovr','DL.3.ovr','DL.4.ovr','LB.1.ovr','LB.2.ovr','LB.3.ovr')])) ) *bonus.4
      }
      
      else if (aP$Position[i]=='TE' ) {
        aP$Bonus[i] = ( mad$ovr_rating[madd] - mean(as.numeric(teams[oppo,c('LB.1.ovr','LB.2.ovr','LB.3.ovr')])) ) * bonus.5
      }
      else if (aP$Position[i]=='K' ) { 
        aP$Bonus[i] = (mean(as.numeric(teams[oppo,
                                             c('DL.1.ovr','DL.2.ovr'
                                               ,'DL.3.ovr','DL.4.ovr'
                                               ,'LB.1.ovr','LB.2.ovr'
                                               ,'LB.3.ovr','CB.1.ovr'
                                               ,'CB.2.ovr','CB.3.ovr'
                                               ,'FS.1.ovr','SS.1.ovr') ])) - mad$ovr_rating[madd] ) * bonus.7
        
      }
    }
  }
  write.csv(teams,'Exports/teamsMadden.csv', row.names=FALSE)
  return(aP)
}