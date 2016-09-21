addHomeBonus = function(aP){
  library(rvest)
  library(stringr)
  library(tidyr)
  library(jsonlite)
  library(reshape)
  library(plyr)

  skip=TRUE
  #aP = avgPlayer
  
  if (!skip) {

    
    urlbase = 'http://www.thehuddle.com/stats/'
    am = '&'
    slotCategoryId = data.frame(value=c('qb','rb','rb','te','pk','DF'), pos=c("QB",'RB','WR','TE','K','D')) 
    #df = data.frame(Player=as.character(),Team=as.character(), FP=as.numeric(), stringsAsFactors = FALSE)
    Scrape2015 = data.frame(Player=as.character(),Team=as.character(), FP=as.numeric(), stringsAsFactors = FALSE)
    
    for (j in 1:length(slotCategoryId$pos)) {
      for (year in 2014:2015) {
        for (week in 1:17) {
          url = paste0(urlbase
                       , year
                       ,'/plays_weekly.php?'
                       ,'week=',week,'&pos=',slotCategoryId$value[j],'&col=FPTS')
          webpage = read_html(url)
          sb_table <- html_nodes(webpage, 'table')
          sb <- try(html_table(sb_table, fill = TRUE)[[1]], silent=TRUE)
          if (class(sb) == "try-error") { # Break condition for when no more players on page
            break
          } else {
            names(sb) = paste(names(sb),sb[1, ],sep='-')
            sb = sb[-1, ]
            head(sb)
          }
          df = sb
          df$Player = sb$`Headings Legend-PLAYER`
          df$Team = sb$`Headings Legend-NFL`
          df$FP = sb$`TOTAL-FPTS`
          df$Week = week
          df$Year = year
          df$Position = slotCategoryId$pos[j]
          Scrape2015 = rbind(Scrape2015,df[,c('Player','Team','FP','Week','Position','Year')])
        }
      }
    }
    Scrapebackup = Scrape2015
    #Scrape2015 = Scrape2015[Scrape2015$FP>=1,]
    Scrape2015 = Scrape2015[Scrape2015$FP>=3,]
    
    for (year in 2014:2015) {
      for (week in 1:17) {
        urlbase = 'http://www.espn.com/nfl/schedule/_/year/'
        url = paste0(urlbase,year,'/week/',week)
        webpage = read_html(url)
        sb_table <- html_nodes(webpage, 'table')
        
        for (i in 1:length(sb_table)) {
          sb <- try(html_table(sb_table, fill = TRUE)[[i]], silent=TRUE)
          if (class(sb) == "try-error") { # Break condition for when no more players on page
            break
          } 
          if (names(sb)[1]=='BYE') {next}
          sb$Week = week
          sb$Year = year
          sb$Day = gsub("<.*?>", "", html_nodes(webpage, 'h2.table-caption'))[i]
          if (week==1 & i==1 & year==2014){
            df = sb
          }else{
            df = rbind(df,sb)
          }
        }
      }
    }
    table(df$Week, df$Year)
    matchups = df
    names(matchups) = c('Away','Home','Result','passing leader','rushing leader','receiving leader','Week','Day')
    
    matchups$Away
    for(i in 1:length(matchups$Away)) { 
      matchups$AwayAbr[i] = substring(matchups$Away[i],rev(gregexpr(' ',matchups$Away[i])[[1]])[1]+1)
      matchups$HomeAbr[i] = substring(matchups$Home[i],rev(gregexpr(' ',matchups$Home[i])[[1]])[1]+1)
    }
    
    matchups.melt = melt(matchups[,c('Week','AwayAbr','HomeAbr')], 'Week', stringsAsFactors=FALSE)
    #matchups.melt[,2] = sapply(matchups.melt[,2], as.character)
    matchups.melt[,3] = sapply(matchups.melt[,3], as.character)
    str(matchups.melt)
    
    Scrape2015$Team = ifelse(Scrape2015$Team=='JAC', 'JAX', Scrape2015$Team)
    Scrape2015$Team = ifelse(Scrape2015$Team=='LA', 'STL', Scrape2015$Team)
    Scrape2015$Team = ifelse(Scrape2015$Team=='WAS', 'WSH', Scrape2015$Team)
    Scrape2015 = merge(Scrape2015,matchups.melt, by.x=c('Week','Team'), by.y = c('Week','value'), all.x=TRUE,sort=FALSE)
    str(Scrape2015)
    
    Scrape2015$FP = as.numeric(Scrape2015$FP)
    Scrape2015 = Scrape2015[order(Scrape2015$FP),]
    #table(Scrape2015$FP,Scrape2015$variable)
    ddply(Scrape2015, .(variable), summarize, sum=sum(FP))
    Scrape2015.lm = lm(FP~variable*Position,Scrape2015)
    Scrape2015.lm2 = lm(FP~variable,Scrape2015)
    Scrape2015.lm3 = lm(FP~Team*variable,Scrape2015[Scrape2015$Position!='D',])
    Scrape2015.lm4 = lm(FP~variable:Player,Scrape2015)
    summary(Scrape2015.lm)
    summary(Scrape2015.lm2)
    summary(Scrape2015.lm3)
    summary(Scrape2015.lm4)
    table(Scrape2015$Year,Scrape2015$Team)
    
    #coef = Scrape2015.lm$coefficients
    predictVars = expand.grid(unique(Scrape2015$Position),unique(Scrape2015$variable))
    names(predictVars) = c('Position','variable')
    predictVars$predict = predict(Scrape2015.lm, predictVars)
    predictVars = rev(predictVars[order(predictVars$Position,predictVars$variable),])
    
    predictVars = ddply(predictVars, .(Position), transform, bonusPositionHome = ((predict /  predict[1]) - 1) )
    predictVars$Home = ifelse(predictVars$variable=='HomeAbr',TRUE,FALSE)
    
    save(predictVars, file='Input/bonusPositionHome.Rda')
    aP = merge(aP,predictVars[,c('Position','Home','bonusPositionHome')], by = c('Position','Home'),all.x=TRUE, sort=FALSE)
    skip = TRUE
  }
  
  if (skip) {
    load('Input/bonusPositionHome.Rda')
    
    #aP = avgPlayer
    aP = merge(aP,predictVars[,c('Position','Home','bonusPositionHome')], by = c('Position','Home'),all.x=TRUE, sort=FALSE)
  }
  
  return(aP)
  
}
