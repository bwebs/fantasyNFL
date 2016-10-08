

doBuckets = function(playerScrapeMerge){

  debug = TRUE
  if (!debug) {
    
    withProgress(message = 'Optimizing...', value = 0, {
      avgPlayer = ddply(playerScrapeMerge, .(Key), summarize, mean=mean(PredictedPoints), min=min(PredictedPoints), max=max(PredictedPoints), freq=length(PredictedPoints))
      #avgPlayer$Position = as.character(avgPlayer$Position)
      #fd = read.csv('FanDuel/FanDuel_Week1.csv', stringsAsFactors = FALSE)
      #fd$FirstLast = paste(fd$First.Name,fd$Last.Name,sep=' ')
      avgPlayer = merge(avgPlayer, fdSalary, by = 'Key', all = TRUE)
      #str(avgPlayer)
      sum(is.na(avgPlayer$mean))
      sum(is.na(avgPlayer$Salary))
      MISSING = avgPlayer[is.na(avgPlayer$mean) | is.na(avgPlayer$Salary),]
      write.csv(MISSING, 'Exports/MISSING.csv')
      
      #REMOVE DOUBTFUL, OUT, IR, leave probable and questionable
      avgPlayer = avgPlayer[!(avgPlayer$`Injury Indicator` %in% c('O','IR','D')),]
      incProgress(0, detail = "Removing players who are out")
      
      #Remove MISSING
      avgPlayer = avgPlayer[!(is.na(avgPlayer$mean) | is.na(avgPlayer$Salary)),]
      
      length(avgPlayer$mean)
      avgPlayer = avgPlayer[avgPlayer$mean > 1,]
      avgPlayer = avgPlayer[order(-avgPlayer$Salary,-avgPlayer$mean),]
      
      #temp=avgPlayer // avgPlayer=temp
      
      incProgress(0, detail = "Determining player strengths")
      avgPlayer = addMaddenBonus(avgPlayer)
      names(avgPlayer)[names(avgPlayer)=='mean'] = 'mean.old'
      incProgress(0, detail = "Calculating home bonus/penalties")
      avgPlayer = addHomeBonus(avgPlayer)
      avgPlayer$mean = (1+avgPlayer$Bonus+avgPlayer$bonusPositionHome)*avgPlayer$mean
      
      write.csv(avgPlayer,'Exports/avgPlayerWithMaddentest.csv', row.names=FALSE)
      
      #length(avgPlayer$mean)
      
      minSalary = 58000
      maxSalary = 60000
      bonus.WR2 = -.1
      bonus.QBWR = .1
      bonus.RB2 = -.1
      bonus.WRTE = -.1
      
      
      
      QB = avgPlayer[avgPlayer$Position=='QB' ,c('Key','mean','Salary','Team')]
      # Remove QB's that are strictly beaten by another QB at the same price level
      #for (i in length(QB$Salary):2) { if (QB$Salary[i-1]==QB$Salary[i] & QB$mean[i-1]>QB$mean[i]) {QB = QB[-i,]}}
      
      RB = avgPlayer[avgPlayer$Position=='RB',c('Key','mean','Salary','Team')]
      #Same for RB's but keep 2
      #for (i in length(RB$Salary):3) { if (RB$Salary[i-2]==RB$Salary[i] & RB$mean[i-2]>RB$mean[i]) {RB = RB[-i,]}}
      
      WR = avgPlayer[avgPlayer$Position=='WR',c('Key','mean','Salary','Team')]
      #for (i in length(WR$Salary):4) { if (WR$Salary[i-3]==WR$Salary[i] & WR$mean[i-3]>WR$mean[i]) {WR = WR[-i,]}}
      
      TE = avgPlayer[avgPlayer$Position=='TE',c('Key','mean','Salary','Team')]
      #for (i in length(TE$Salary):2) { if (TE$Salary[i-1]==TE$Salary[i] & TE$mean[i-1]>TE$mean[i]) {TE = TE[-i,]}}
      
      K = avgPlayer[avgPlayer$Position=='K',c('Key','mean','Salary','Team')]
      #for (i in length(K$Salary):2) { if (K$Salary[i-1]==K$Salary[i] & K$mean[i-1]>K$mean[i]) {K = K[-i,]}}
      
      D = avgPlayer[avgPlayer$Position=='D',c('Key','mean','Salary','Team')]
      #for (i in length(D$Salary):2) { if (D$Salary[i-1]==D$Salary[i] & D$mean[i-1]>D$mean[i]) {D = D[-i,]}}
      
      lQB = length(QB$Salary)
      lRB = length(RB$Salary)
      lWR = length(WR$Salary)
      lTE = length(TE$Salary)
      lK = length(K$Salary)
      lD = length(D$Salary)
      
      
      
      
      QBWR = data.frame(a=as.integer(),d=as.integer(),Salary=as.integer(),PredictedPoints=as.double(),bonus_QBWR=as.double())
      count=0
      for ( a in 1:lQB) {
        for (d in 1:lWR) {
          count=count+1
          bonus = ifelse(QB$Team[a]==WR$Team[d],(QB$mean[a]+WR$mean[d])*bonus.QBWR,0)
          QBWR[count,] = c(a,d,QB$Salary[a]+WR$Salary[d],QB$mean[a]+WR$mean[d]+bonus,bonus)
        }
      }
      QBWR = QBWR[order(-QBWR$Salary,-QBWR$PredictedPoints),]
      for (k in length(QBWR$Salary):2) { if (QBWR$Salary[k-1]==QBWR$Salary[k] & QBWR$PredictedPoints[k-1]>QBWR$PredictedPoints[k]) {QBWR = QBWR[-k,]}}
      incProgress(0, detail = "Starting the optimization")
      QBWR2 = data.frame(a=as.integer(),d=as.integer(),e=as.integer(),Salary=as.integer(),PredictedPoints=as.double(),bonus_QBWR=as.double(), bonus_WR2=as.double())
      count=0
      for (j in 1:length(QBWR$Salary)) {
        WRadd = (1:lWR)[-QBWR$d[j]]
        for (e in WRadd) {
          if ( any(e == QBWR2$d & QBWR2$e == QBWR$d[j] & QBWR2$a == QBWR$a[j]) ) { 
            #print('break')
            break }
          count=count+1
          bonus.1 = ifelse(WR$Team[e]==WR$Team[QBWR$d[j]], (WR$mean[e]+WR$mean[QBWR$d[j]])*bonus.WR2,0)
          bonus.2 = ifelse(WR$Team[e]==QB$Team[QBWR$a[j]], (WR$mean[e]+QB$mean[QBWR$a[j]])*bonus.QBWR,0)
          QBWR2[count,] = c(QBWR$a[j],QBWR$d[j],e
                            ,QBWR$Salary[j]+WR$Salary[e]
                            ,QBWR$PredictedPoints[j]+WR$mean[e]+bonus.1+bonus.2-min(bonus.2,QBWR$bonus_QBWR[j])
                            ,max(QBWR$bonus_QBWR[j],bonus.2),bonus.1)
        }
      }
      QBWR2 = QBWR2[order(-QBWR2$Salary, -QBWR2$PredictedPoints),]
      for (k in length(QBWR2$Salary):2) { if (QBWR2$Salary[k-1]==QBWR2$Salary[k] & QBWR2$PredictedPoints[k-1]>QBWR2$PredictedPoints[k]) {QBWR2 = QBWR2[-k,]}}
      
      
      QBWR3 = data.frame(a=as.integer(),d=as.integer(),e=as.integer(),f=as.integer(),Salary=as.integer(),PredictedPoints=as.double(),bonus_QBWR=as.double(), bonus_WR2=as.double())
      count=0
      for (j in 1:length(QBWR2$Salary)) {
        WRadd = (1:lWR)[c(-QBWR2$d[j],-QBWR2$e[j])]
        for (f in WRadd) {
          #if ( any(f == QBWR2$d & QBWR2$e == QBWR$d[j] & QBWR2$a == QBWR$a[j]) ) { break }
          
          if (   any( QBWR3$d == f          & QBWR3$e == QBWR2$e[j] & QBWR3$f == QBWR2$d[j] & QBWR3$a == QBWR2$a[j] ) 
                 | any( QBWR3$d == f          & QBWR3$e == QBWR2$d[j] & QBWR3$f == QBWR2$e[j] & QBWR3$a == QBWR2$a[j] )
                 | any( QBWR3$d == QBWR2$e[j] & QBWR3$e == f          & QBWR3$f == QBWR2$d[j] & QBWR3$a == QBWR2$a[j] )
                 | any( QBWR3$d == QBWR2$d[j] & QBWR3$e == f          & QBWR3$f == QBWR2$e[j] & QBWR3$a == QBWR2$a[j] ) ) {break}
          count=count+1
          bonus.1 = ifelse(WR$Team[f]==WR$Team[QBWR2$d[j]], (WR$mean[f]+WR$mean[QBWR2$d[j]])*bonus.WR2,0)
          bonus.2 = ifelse(WR$Team[f]==QB$Team[QBWR2$a[j]], (WR$mean[f]+QB$mean[QBWR2$a[j]])*bonus.QBWR,0)
          bonus.3 = ifelse(WR$Team[f]==WR$Team[QBWR2$e[j]], (WR$mean[f]+WR$mean[QBWR2$e[j]])*bonus.WR2,0)
          QBWR3[count,] = c(QBWR2$a[j],QBWR2$d[j],QBWR2$e[j],f
                            ,QBWR2$Salary[j]+WR$Salary[f]
                            ,QBWR2$PredictedPoints[j]+WR$mean[f]+bonus.1+bonus.2+bonus.3-min(bonus.2,QBWR2$bonus_QBWR[j])
                            ,max(QBWR2$bonus_QBWR[j],bonus.2),bonus.1+bonus.3)
        }
      }
      QBWR3 = QBWR3[order(-QBWR3$Salary, -QBWR3$PredictedPoints),]
      for (k in length(QBWR3$Salary):2) { if (QBWR3$Salary[k-1]==QBWR3$Salary[k] & QBWR3$PredictedPoints[k-1]>QBWR3$PredictedPoints[k]) {QBWR3 = QBWR3[-k,]}}
      
      
      TEQBWR3 = data.frame(a=as.integer(),d=as.integer(),e=as.integer(),f=as.integer(),g=as.integer(),Salary=as.integer(),PredictedPoints=as.double(),bonus_QBWR=as.double(), bonus_WR2=as.double(), bonus_WRTE=as.double())
      count=0
      for (j in 1:length(QBWR3$Salary)) {
        for (g in 1:lTE) {
          count = count+1
          bonus.1 = ifelse(TE$Team[g]==WR$Team[QBWR3$d[j]], (TE$mean[g]+WR$mean[QBWR3$d[j]])*bonus.WRTE,0)
          bonus.2 = ifelse(TE$Team[g]==WR$Team[QBWR3$e[j]], (TE$mean[g]+WR$mean[QBWR3$e[j]])*bonus.WRTE,0)
          bonus.1 = ifelse(TE$Team[g]==WR$Team[QBWR3$f[j]], (TE$mean[g]+WR$mean[QBWR3$f[j]])*bonus.WRTE,0)
          TEQBWR3[count,] = c(QBWR3$a[j],QBWR3$d[j],QBWR3$e[j],QBWR3$f[j],g
                              ,QBWR3$Salary[j]+TE$Salary[g]
                              ,QBWR3$PredictedPoints[j]+TE$mean[g]+bonus.1+bonus.2+bonus.3
                              ,QBWR3$bonus_QBWR[j],QBWR3$bonus_WR2[j],bonus.1+bonus.2+bonus.3)
        }
      }  
      TEQBWR3 = TEQBWR3[order(-TEQBWR3$Salary, -TEQBWR3$PredictedPoints),]
      for (k in length(TEQBWR3$Salary):2) { if (TEQBWR3$Salary[k-1]==TEQBWR3$Salary[k] & TEQBWR3$PredictedPoints[k-1]>TEQBWR3$PredictedPoints[k]) {TEQBWR3 = TEQBWR3[-k,]}}
      
      
      RB12 = data.frame(b=as.integer(),c=as.integer(),Salary=as.integer(),PredictedPoints=as.double(), bonus_RB2=as.double())
      count=0
      for (b in 1:(lRB-1) ) {
        for (c in (b+1):lRB ) {
          count = count+1
          bonus.1 = ifelse(RB$Team[b]==RB$Team[c], (RB$mean[b]+RB$mean[c])*bonus.RB2,0)
          RB12[count,] = c(b,c,RB$Salary[b]+RB$Salary[c],RB$mean[b]+RB$mean[c]+bonus.1,bonus.1)
        }}
      RB12 = RB12[order(-RB12$Salary, -RB12$PredictedPoints),]
      for (k in length(RB12$Salary):2) { if (RB12$Salary[k-1]==RB12$Salary[k] & RB12$PredictedPoints[k-1]>RB12$PredictedPoints[k]) {RB12 = RB12[-k,]}}
      
      KD = data.frame(h=as.integer(),i=as.integer(),Salary=as.integer(),PredictedPoints=as.double())
      count=0
      for  ( h in 1:lK ) {
        for (i in 1:lD) {
          count=count+1
          KD[count,] = c(h,i,K$Salary[h]+D$Salary[i],K$mean[h]+D$mean[i])
        }
      }
      KD = KD[order(-KD$Salary,-KD$PredictedPoints),]
      str(KD)
      for (i in length(KD$Salary):2) { if (KD$Salary[i-1]==KD$Salary[i] & KD$PredictedPoints[i-1]>KD$PredictedPoints[i]) {KD = KD[-i,]}}
      
      
      KDRB12 = data.frame(b=as.integer(),c=as.integer(),h=as.integer(),i=as.integer(),Salary=as.integer(),PredictedPoints=as.double(), bonus_RB2=as.double())
      count = 0
      for (j in 1:length(RB12$Salary)) {
        for (k in 1:length(KD$Salary)) {
          count=count+1
          KDRB12[count,] = c(RB12$b[j],RB12$c[j],KD$h[k],KD$i[k]
                             ,RB12$Salary[j] + KD$Salary[k]
                             ,RB12$PredictedPoints[j] + KD$PredictedPoints[k], RB12$bonus_RB2[j])
        }
      }
      KDRB12 = KDRB12[order(-KDRB12$Salary,-KDRB12$PredictedPoints),]
      for (i in length(KDRB12$Salary):2) { if (KDRB12$Salary[i-1]==KDRB12$Salary[i] & KDRB12$PredictedPoints[i-1]>KDRB12$PredictedPoints[i]) {KDRB12 = KDRB12[-i,]}}
      
      
      n=length(TEQBWR3$Salary)*length(KDRB12$Salary)
      
      KDRBTEQBWR3 = data.frame(a=as.integer(),b=as.integer(),c=as.integer(),d=as.integer(),e=as.integer(),f=as.integer(),g=as.integer(),h=as.integer(),i=as.integer(),Salary=as.integer(),PredictedPoints=as.double(),bonus_QBWR=as.double(), bonus_WR2=as.double(), bonus_WRTE=as.double(), bonus_RB2=as.double())
      count=0
      for (j in 1:length(TEQBWR3$Salary)) {
        for (k in 1:length(KDRB12$Salary)) {
          count=count+1
          KDRBTEQBWR3[count,] = c(TEQBWR3$a[j],KDRB12$b[k],KDRB12$c[k],TEQBWR3$d[j],TEQBWR3$e[j],TEQBWR3$f[j],TEQBWR3$g[j],KDRB12$h[k],KDRB12$i[k]
                                  ,TEQBWR3$Salary[j]+KDRB12$Salary[k]
                                  ,TEQBWR3$PredictedPoints[j]+KDRB12$PredictedPoints[k]
                                  ,TEQBWR3$bonus_QBWR[j],TEQBWR3$bonus_WR2[j]
                                  ,TEQBWR3$bonus_WRTE[j], KDRB12$bonus_RB2[k])
          incProgress(1/n, detail = "This will take awhile!")
        }
      }
      
    })
    #KDRBTEQBWR3 = KDRBTEQBWR3[order(-KDRBTEQBWR3$Salary, -KDRBTEQBWR3$PredictedPoints),]
    #for (k in length(KDRBTEQBWR3$Salary):2) { if (KDRBTEQBWR3$Salary[k-1]==KDRBTEQBWR3$Salary[k] & KDRBTEQBWR3$PredictedPoints[k-1]>KDRBTEQBWR3$PredictedPoints[k]) {KDRBTEQBWR3 = KDRBTEQBWR3[-k,]}}
    
    KDRBTEQBWR3 = KDRBTEQBWR3[order(-KDRBTEQBWR3$PredictedPoints),]
    buckets = KDRBTEQBWR3
    BestCombos = buckets[minSalary<=buckets$Salary & buckets$Salary <=maxSalary,]
    #for (j in length(BestCombos$Salary):2) { if (BestCombos$Salary[j-1]==BestCombos$Salary[j] & BestCombos$PredictedPoints[j-1]>BestCombos$PredictedPoints[j]) {BestCombos = BestCombos[-j,]}}
    
    BestCombos.Key = data.frame(QB.Key=as.character(),RB1.Key=as.character(),RB2.Key=as.character(),WR1.Key=as.character(),WR2.Key=as.character(),WR3.Key=as.character(),TE.Key=as.character(),K.Key=as.character(),D.Key=as.character(),Salary=as.integer(),PredictedPoints=as.double(),stringsAsFactors=FALSE)
    for (j in 1:length(BestCombos$Salary)) {
      BestCombos.Key[j,] = c(QB$Key[BestCombos$a[j]],RB$Key[BestCombos$b[j]],
                             RB$Key[BestCombos$c[j]],WR$Key[BestCombos$d[j]],
                             WR$Key[BestCombos$e[j]],WR$Key[BestCombos$f[j]],
                             TE$Key[BestCombos$g[j]],K$Key[BestCombos$h[j]],
                             D$Key[BestCombos$i[j]],BestCombos$Salary[j],BestCombos$PredictedPoints[j] )
    }
    BestCombos.Names = data.frame(QB.Name=as.character(),RB1.Name=as.character(),RB2.Name=as.character(),WR1.Name=as.character(),WR2.Name=as.character(),WR3.Name=as.character(),TE.Name=as.character(),K.Name=as.character(),D.Name=as.character(),Salary=as.integer(),PredictedPoints=as.double(),stringsAsFactors=FALSE)
    BestCombos.Extra.Injury = data.frame(QB.Extra.Injury=as.character(),RB1.Extra.Injury=as.character(),RB2.Extra.Injury=as.character(),WR1.Extra.Injury=as.character(),WR2.Extra.Injury=as.character(),WR3.Extra.Injury=as.character(),TE.Extra.Injury=as.character(),K.Extra.Injury=as.character(),D.Extra.Injury=as.character(),stringsAsFactors=FALSE)
    BestCombos.Extra.Salary = data.frame(QB.Extra.Salary=as.character(),RB1.Extra.Salary=as.character(),RB2.Extra.Salary=as.character(),WR1.Extra.Salary=as.character(),WR2.Extra.Salary=as.character(),WR3.Extra.Salary=as.character(),TE.Extra.Salary=as.character(),K.Extra.Salary=as.character(),D.Extra.Salary=as.character(),stringsAsFactors=FALSE)
    for (j in 1:nrow(BestCombos.Key)){
      for (i in 1:8) {
        whichrow = which(grepl(BestCombos.Key[j,i],avgPlayer$Key))[[1]]  #LOOK OUT FOR DUPLICATES
        BestCombos.Names[j,i] = paste(avgPlayer$Name[whichrow],avgPlayer$Team[whichrow], sep=', ')
        BestCombos.Extra.Injury[j,i] = paste0(avgPlayer$`Injury Indicator`[whichrow])
        BestCombos.Extra.Salary[j,i] = paste0(avgPlayer$Salary[whichrow])
      }
      i=9
      whichrow = which(grepl(BestCombos.Key[j,i],avgPlayer$Key))[[1]]  #LOOK OUT FOR DUPLICATES
      BestCombos.Names[j,i] = paste0(avgPlayer$Name[whichrow])
      BestCombos.Extra.Salary[j,i] = avgPlayer$Salary[whichrow]
    }
    BestCombos.Names$Salary = BestCombos.Key$Salary
    BestCombos.Names$PredictedPoints = BestCombos.Key$PredictedPoints
    BestCombos.All = cbind(BestCombos.Names,BestCombos.Extra.Injury,BestCombos.Key[,1:9],BestCombos.Extra.Salary)
    #print('DONEDONEDONE')
    save(BestCombos.All, file='bestCombo.Rda')
  } else {
    load('bestCombo.Rda')
  }
  return(BestCombos.All)
}

#BestCombos.FP = data.frame(QB.Name=as.character(),RB1.Name=as.character(),RB2.Name=as.character(),WR1.Name=as.character(),WR2.Name=as.character(),WR3.Name=as.character(),TE.Name=as.character(),K.Name=as.character(),D.Name=as.character(),Salary=as.integer(),PredictedPoints=as.double(),stringsAsFactors=FALSE)
#write.csv(BestCombos.Names,'Exports/WEEK3_ANALYSIS.csv',row.names=FALSE)