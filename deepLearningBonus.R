library(h2o)
deepLearningBonus = function(aP) {
  print('Adding Deep Learning Bonus, deepBonus')
  #aP = avgPlayer  
  
  varWeek=week.prompt
  iteration = week.prompt+1
  
  #str(aP)
  mad = fromJSON(paste0('https://www.easports.com/madden-nfl/ratings/service/data?entityType=madden17_player&filter=iteration:',iteration,'&limit=20000'))$docs
  #mad = fromJSON('https://www.easports.com/madden-nfl/ratings/service/data?entityType=madden17_player&&limit=20000')$docs
  #temp = mad // mad=temp
  #str(mad)
  names(mad)[names(mad)=='team']='Team'
  names(mad)[names(mad)=='position']='Position'
  mad = convertPositions(mad)
  mad$Name = paste(mad$firstName,mad$lastName, sep=' ')
  
  translateName = read.csv('Input/aP_mad_nameTranslation.csv', stringsAsFactors = FALSE)
  checkIn = which(mad$Name %in% translateName$Name.From.debug_mad)
  for (i in checkIn ) {mad$Name[i] = translateName$Name[which(translateName$Name.From.debug_mad %in% mad$Name[i])[1]] }
  
  mad = addNameRM(mad)
  mad = abbreviateTeams(mad)
  mad = createKey(mad)
  #str(mad)
  
  #REMOVE PLAYERS OUT THIS WEEK HERE
  injury = injuryList()
  injury = merge(injury,fdSalary[,c('Key','Team')], by='Key', sort=FALSE, all.x=TRUE)
  #table(injury$NameRM %in% mad$NameRM)
  injury = injury[!(injury$InjuryStatus %in%  c('Out','Doubtful','Injury Reserve')),]
  
  mad = mad[!(mad$NameRM %in% injury$NameRM),]
  mad = mad[!(mad$NameRM %in% fdSalary$NameRM[fdSalary$`Injury Indicator` %in% c('O','IR','D')]),]
  
  mad = mad[order(mad$Team,mad$Position,-mad$ovr_rating),]
  mad$Rank = ave(-mad$ovr_rating, mad$Team, mad$Position, FUN=function(x) rank(x, ties.method="random"))
  
  a.needs = data.frame(position=c('QB','WR','RB','OL','TE','DL','LB','CB','FS','SS','K'), need=c(1,3,2,5,1,4,3,3,1,1,1), stringsAsFactors = FALSE)
  teams = data.frame(Opponent=as.character(unique(mad$Team)), stringsAsFactors=FALSE)
  
  # Check for missing necessary positions and update the depth chart
  for (i in 1:length(a.needs$position)) {
    for (k in 1:length(teams$Opponent)) {
      # Check for Free Safety, replace with CB3
      if (length(which(mad$Team == teams$Opponent[k] 
                       & mad$Rank==1 
                       & mad$Position == a.needs$position[i] )) == 0 ) {
        replacement=0
        if (a.needs$position[i] == 'FS') {
          replacement = which(mad$Team == teams$Opponent[k] 
                              & mad$Rank==2
                              & mad$Position == 'SS')
          
          mad$Position[replacement] = 'FS'
          mad$ovr_rating[replacement] = mad$ovr_rating[replacement]*.95
          mad$Rank[replacement] = 1
          print(paste('Missing FS replacement',i,k,teams$Opponent[k],a.needs$position[i],replacement,mad$Position[replacement],mad$Key[replacement],sep=','))
        }
        if (a.needs$position[i] == 'K') {
          replacement = which(mad$Team == teams$Opponent[k] 
                              & mad$Rank==1
                              & mad$Position == 'P')
          mad$Position[replacement]='K'
          mad$ovr_rating[replacement] = mad$ovr_rating[replacement]*.7
          print(paste('Missing K replacement',i,k,teams$Opponent[k],a.needs$position[i],replacement,mad$Position[replacement],mad$Key[replacement],sep=','))
        }
        if (a.needs$position[i] == 'SS') {
          replacement = which(mad$Team == teams$Opponent[k] 
                              & mad$Rank==2
                              & mad$Position == 'FS')
          mad$Position[replacement]='SS'
          mad$ovr_rating[replacement] = mad$ovr_rating[replacement]*.95
          mad$Rank[replacement] = 1
          print(paste('Missing SS replacement',i,k,teams$Opponent[k],a.needs$position[i],replacement,mad$Position[replacement],mad$Key[replacement],sep=','))
        }
      }
    }
  }
  
  #temp = mad // mad=temp
  
  #analysis 1
  #a1 = mad[mad$Position=="QB",]
  
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
  
  mad$Depth = paste0(mad$Position,mad$Rank)
  
  h2o.init(nthreads = -1)  
  
  positions = c('QB','RB','WR','TE','K','D') #
  for (position in positions ) {
    print(paste0('Loading Deep Learning Bonuses: ',position))
    combine = merge(aP[aP$Position==position,],teams,by='Opponent')
    combine = combine[,!grepl('.Key',names(combine))]
    names(combine)[grepl('.ovr',names(combine))] = paste0('Opponent.',names(combine)[grepl('.ovr',names(combine))])
    #dim(combine)
    
    combine = merge(combine,teams,by.x='Team', by.y='Opponent')
    combine = combine[,!grepl('.Key',names(combine))]
    names(combine)[grepl('.ovr',names(combine))] = paste0('Team.',names(combine)[grepl('.ovr',names(combine))])
    #dim(combine)
    combine$Week = as.factor(varWeek)
    combine$variable = as.factor(ifelse(combine$Home, 'HomeAbr', 'AwayAbr'))
    
    #str(combine)
    combine = merge(combine,mad[,c('ovr_rating','Key','Depth')],by='Key', all.x=TRUE)
    #dim(combine)
    names(combine)[names(combine)=='ovr_rating'] = 'Overall'
    combine$Depth = as.factor(combine$Depth)
    
    combine$Overall[combine$Position=='D' & is.na(combine$Overall)]   = rowMeans(combine[combine$Position=='D' & is.na(combine$Overall),
                                                                                         c('Team.DL.1.ovr','Team.DL.2.ovr','Team.DL.3.ovr','Team.DL.4.ovr'
                                                                                           ,'Team.LB.1.ovr','Team.LB.2.ovr','Team.LB.3.ovr','Team.CB.1.ovr'
                                                                                           ,'Team.CB.2.ovr','Team.FS.1.ovr','Team.SS.1.ovr') ])
    combine = combine[!is.na(combine$Overall),]
    
    combine$Defense.Team = combine$Team.DL.1.ovr + combine$Team.DL.2.ovr + combine$Team.DL.3.ovr + combine$Team.DL.4.ovr + combine$Team.LB.1.ovr + combine$Team.LB.2.ovr + combine$Team.LB.3.ovr + combine$Team.CB.1.ovr + combine$Team.CB.2.ovr + combine$Team.FS.1.ovr + combine$Team.SS.1.ovr
    combine$Defense.Opponent = combine$Team.Opponent.DL.1.ovr + combine$Team.Opponent.DL.2.ovr + combine$Team.Opponent.DL.3.ovr + combine$Team.Opponent.DL.4.ovr + combine$Team.Opponent.LB.1.ovr + combine$Team.Opponent.LB.2.ovr + combine$Team.Opponent.LB.3.ovr + combine$Team.Opponent.CB.1.ovr + combine$Team.Opponent.CB.2.ovr + combine$Team.Opponent.FS.1.ovr + combine$Team.Opponent.SS.1.ovr
    combine$Offense.Team = combine$Team.QB.1.ovr + combine$Team.WR.1.ovr + combine$Team.WR.2.ovr + combine$Team.WR.3.ovr + combine$Team.RB.1.ovr + combine$Team.RB.2.ovr + combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.OL.3.ovr + combine$Team.OL.4.ovr + combine$Team.OL.5.ovr + combine$Team.TE.1.ovr
    combine$Offense.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.WR.2.ovr + combine$Team.Opponent.WR.3.ovr + combine$Team.Opponent.RB.1.ovr + combine$Team.Opponent.RB.2.ovr + combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.OL.3.ovr + combine$Team.Opponent.OL.4.ovr + combine$Team.Opponent.OL.5.ovr + combine$Team.Opponent.TE.1.ovr
    combine$Core.Offense.Team = combine$Team.QB.1.ovr + combine$Team.WR.1.ovr + combine$Team.RB.1.ovr + combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.TE.1.ovr
    combine$Core.Offense.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.RB.1.ovr + combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.TE.1.ovr
    combine$Core.Defense.Team = combine$Team.DL.1.ovr + combine$Team.DL.2.ovr + combine$Team.LB.1.ovr + combine$Team.CB.1.ovr + combine$Team.FS.1.ovr + combine$Team.SS.1.ovr
    combine$Core.Defense.Opponent = combine$Team.Opponent.DL.1.ovr + combine$Team.Opponent.DL.2.ovr + combine$Team.Opponent.LB.1.ovr + combine$Team.Opponent.CB.1.ovr + combine$Team.Opponent.FS.1.ovr + combine$Team.Opponent.SS.1.ovr
    combine$WRs.Team = combine$Team.WR.1.ovr + combine$Team.WR.2.ovr + combine$Team.WR.3.ovr
    combine$WRs.Opponent = combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.WR.2.ovr + combine$Team.Opponent.WR.3.ovr
    combine$OL.Team = combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.OL.3.ovr + combine$Team.OL.4.ovr
    combine$OL.Opponent = combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.OL.3.ovr + combine$Team.Opponent.OL.4.ovr
    combine$DL.Team = combine$Team.DL.1.ovr + combine$Team.DL.2.ovr + combine$Team.DL.3.ovr + combine$Team.DL.4.ovr
    combine$DL.Opponent = combine$Team.Opponent.DL.1.ovr + combine$Team.Opponent.DL.2.ovr + combine$Team.Opponent.DL.3.ovr + combine$Team.Opponent.DL.4.ovr
    combine$Secondary.Team = combine$Team.CB.1.ovr + combine$Team.CB.2.ovr + combine$Team.FS.1.ovr + combine$Team.SS.1.ovr
    combine$Secondary.Opponent = combine$Team.Opponent.CB.1.ovr + combine$Team.Opponent.CB.2.ovr + combine$Team.Opponent.FS.1.ovr + combine$Team.Opponent.SS.1.ovr
    combine$Box.Team = combine$Team.DL.1.ovr + combine$Team.DL.2.ovr + combine$Team.DL.3.ovr + combine$Team.DL.4.ovr + combine$Team.LB.1.ovr + combine$Team.LB.2.ovr + combine$Team.LB.3.ovr
    combine$Box.Opponent = combine$Team.Opponent.DL.1.ovr + combine$Team.Opponent.DL.2.ovr + combine$Team.Opponent.DL.3.ovr + combine$Team.Opponent.DL.4.ovr + combine$Team.Opponent.LB.1.ovr + combine$Team.Opponent.LB.2.ovr + combine$Team.Opponent.LB.3.ovr
    combine$LBs.Team = combine$Team.LB.1.ovr + combine$Team.LB.2.ovr + combine$Team.LB.3.ovr
    combine$LBs.Opponent = combine$Team.Opponent.LB.1.ovr + combine$Team.Opponent.LB.2.ovr + combine$Team.Opponent.LB.3.ovr
    combine$Ss.Team = combine$Team.FS.1.ovr + combine$Team.SS.1.ovr
    combine$Ss.Opponent = combine$Team.Opponent.FS.1.ovr + combine$Team.Opponent.SS.1.ovr
    combine$CBs.Team = combine$Team.CB.1.ovr + combine$Team.CB.2.ovr + combine$Team.CB.3.ovr
    combine$CB.Opponent = combine$Team.Opponent.CB.1.ovr + combine$Team.Opponent.CB.2.ovr + combine$Team.Opponent.CB.3.ovr
    combine$Matchup.All.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr - combine$Team.Opponent.LB.1.ovr - combine$Team.Opponent.LB.2.ovr - combine$Team.Opponent.LB.3.ovr - combine$Team.Opponent.CB.1.ovr - combine$Team.Opponent.CB.2.ovr - combine$Team.Opponent.FS.1.ovr - combine$Team.Opponent.SS.1.ovr + combine$Team.QB.1.ovr + combine$Team.WR.1.ovr + combine$Team.WR.2.ovr + combine$Team.WR.3.ovr + combine$Team.RB.1.ovr + combine$Team.RB.2.ovr + combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.OL.3.ovr + combine$Team.OL.4.ovr + combine$Team.OL.5.ovr + combine$Team.TE.1.ovr
    combine$Matchup.All.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.WR.2.ovr + combine$Team.Opponent.WR.3.ovr + combine$Team.Opponent.RB.1.ovr + combine$Team.Opponent.RB.2.ovr + combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.OL.3.ovr + combine$Team.Opponent.OL.4.ovr + combine$Team.Opponent.OL.5.ovr + combine$Team.Opponent.TE.1.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr - combine$Team.LB.1.ovr - combine$Team.LB.2.ovr - combine$Team.LB.3.ovr - combine$Team.CB.1.ovr - combine$Team.CB.2.ovr - combine$Team.FS.1.ovr - combine$Team.SS.1.ovr
    combine$Matchup.Cores.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.LB.1.ovr - combine$Team.Opponent.CB.1.ovr - combine$Team.Opponent.FS.1.ovr - combine$Team.Opponent.SS.1.ovr + combine$Team.QB.1.ovr + combine$Team.WR.1.ovr + combine$Team.RB.1.ovr + combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.TE.1.ovr
    combine$Matchup.Cores.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.RB.1.ovr + combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.TE.1.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.LB.1.ovr - combine$Team.CB.1.ovr - combine$Team.FS.1.ovr - combine$Team.SS.1.ovr
    combine$Matchup.QBRBDL.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr + combine$Team.QB.1.ovr + combine$Team.RB.1.ovr
    combine$Matchup.QBRBDL.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.RB.1.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr
    combine$Matchup.QBWR1RB1FS1CB1LB1.Team = combine$Team.Opponent.LB.1.ovr - combine$Team.Opponent.CB.1.ovr - combine$Team.Opponent.FS.1.ovr + combine$Team.QB.1.ovr + combine$Team.WR.1.ovr + combine$Team.RB.1.ovr
    combine$Matchup.QBWR1RB1FS1CB1LB1.Opponent = combine$Team.Opponent.QB.1.ovr + combine$Team.Opponent.WR.1.ovr + combine$Team.Opponent.RB.1.ovr - combine$Team.LB.1.ovr - combine$Team.CB.1.ovr - combine$Team.FS.1.ovr
    combine$Matchup.QBDL.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr + combine$Team.QB.1.ovr
    combine$Matchup.QBDL.Opponent = combine$Team.Opponent.QB.1.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr
    combine$Matchup.RBDL.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr + combine$Team.RB.1.ovr
    combine$Matchup.RB1DL.Opponent = combine$Team.Opponent.RB.1.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr
    combine$Matchup.RB12DL.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr + combine$Team.RB.2.ovr
    combine$Matchup.RB2DL.Opponent = combine$Team.Opponent.RB.2.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr
    combine$Matchup.OLDL.Team = combine$Team.Opponent.DL.1.ovr - combine$Team.Opponent.DL.2.ovr - combine$Team.Opponent.DL.3.ovr - combine$Team.Opponent.DL.4.ovr + combine$Team.OL.1.ovr + combine$Team.OL.2.ovr + combine$Team.OL.3.ovr + combine$Team.OL.4.ovr + combine$Team.OL.5.ovr
    combine$Matchup.OLDL.Opponent = combine$Team.Opponent.OL.1.ovr + combine$Team.Opponent.OL.2.ovr + combine$Team.Opponent.OL.3.ovr + combine$Team.Opponent.OL.4.ovr + combine$Team.Opponent.OL.5.ovr - combine$Team.DL.1.ovr - combine$Team.DL.2.ovr - combine$Team.DL.3.ovr - combine$Team.DL.4.ovr
    combine$Matchup.WRCB3.Team = combine$Team.Opponent.CB.3.ovr + combine$Team.WR.3.ovr
    combine$Matchup.WRCB3.Opponent = combine$Team.Opponent.WR.3.ovr - combine$Team.CB.3.ovr
    combine$Matchup.WRCB2.Team = combine$Team.Opponent.CB.2.ovr + combine$Team.WR.2.ovr
    combine$Matchup.WRCB2.Opponent = combine$Team.Opponent.WR.2.ovr - combine$Team.CB.2.ovr
    combine$Matchup.WRCB1.Team = combine$Team.Opponent.CB.1.ovr + combine$Team.WR.1.ovr
    combine$Matchup.WRCB1.Opponent = combine$Team.Opponent.WR.1.ovr - combine$Team.CB.1.ovr
    
    #dim(combine)
    
    path = paste0('../maddenProjections/modelOutputs/dl_grid',position,'_bestModelSave/')
    temp = list.files(path = path, pattern="*model")[1]
    
    m_loaded <- h2o.loadModel(paste0(path,temp))
    avgGuy = combine
    
    #Average at each Depth
    avgGuy$Overall = ave(avgGuy$Overall, avgGuy$Depth, FUN = mean)
    
    #sum(!(m_loaded@allparameters$x %in% names(avgGuy)))
    
    if (position != 'D') {
      factors= which(names(combine[,m_loaded@allparameters$x]) %in% c("Week","variable","Depth" ))
    } else {
      factors= which(names(combine[,m_loaded@allparameters$x]) %in% c("Week","variable" ))
    }
    combine.predict = h2o.cbind(as.h2o( combine[,m_loaded@allparameters$x])
                                , h2o.interaction( 
                                  as.h2o( combine[,m_loaded@allparameters$x])
                                  , factors=factors,pairwise=TRUE,max_factors = 3,min_occurrence = 2))
    combine$deepPredict = as.vector(h2o.predict(m_loaded, combine.predict))
    avgGuy.predict = h2o.cbind(as.h2o( avgGuy[,m_loaded@allparameters$x])
                               , h2o.interaction( 
                                 as.h2o( avgGuy[,m_loaded@allparameters$x])
                                 , factors=factors,pairwise=TRUE,max_factors = 3,min_occurrence = 2))
    combine$avgGuyPredict = as.vector(predict(m_loaded, avgGuy.predict))
    combine$Depth = ifelse(is.na(combine$Depth),combine$Position,combine$Depth)
    combine$deepBonus = combine$avgGuyPredict / ave(combine$avgGuyPredict, combine$Depth, FUN=mean) - 1
    if (position=='QB') { output = combine } else {output = rbind(output,combine)}
  }
  
  aP= merge(aP,output[,c('Key','deepBonus')],by='Key')
  return(aP)
  
  write.csv(output,paste0('Exports/week',varWeek,'_deepProjections.csv'))
}