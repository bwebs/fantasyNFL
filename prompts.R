prompts = function(){
  repeat{
    madBonus = readline("Use original Madden Bonus? [Y/n]: ")
    if (madBonus %in% c('Y','n')) {
      madBonus <<- ifelse(madBonus=='Y',TRUE,FALSE)
      break  
    }
  }
  repeat{
    dlBonus = readline("Use Deep Learning Bonus? [Y/n]: ")
    if (dlBonus %in% c('Y','n')) {
      dlBonus <<- ifelse(dlBonus=='Y',TRUE,FALSE)
      break  
    }
  }
  
  repeat{
    homeBonus = readline("Use Home Bonus? [Y/n]: ")
    if (homeBonus %in% c('Y','n')) {
      homeBonus <<- ifelse(homeBonus=='Y',TRUE,FALSE)
      break  
    }
  }
  repeat{
    fdPoints <<- readline("Use FanDuels FP? [Y/n]: ")
    if (fdPoints %in% c('Y','n')) {
      fdPoints <<- ifelse(fdPoints=='Y',TRUE,FALSE)
      break  
    }
  }
  if (fdPoints) {
    repeat{
      fdPoints.weight = as.numeric(readline("Weight the FD points by what percent? (eg, 10) [0-100]: "))
      if (fdPoints.weight <=100 & fdPoints.weight >= 0 ) {
        fdPoints.weight <<- fdPoints.weight/100
        break  
      }
    }
  }
  repeat{
    week.prompt <<- as.integer(readline("Which week? [1-17]: "))
    if (week.prompt <=17 & week.prompt >= 1 ) {
      break  
    }
  }
  source('afterPrompt.R')
}
