injuryList = function() {
  require(httr)
  require(XML)
  require(RCurl)
  require(stringr)
  source('functions.R')
  
  #http://www.espn.com/nfl/injuries
  
  test = getURL('http://www.espn.com/nfl/injuries')
  doc = htmlParse(test)
  table = readHTMLTable(doc, trim=TRUE)[[1]]
  
  df= table
  #remove Header Rows, Comment Rows, and Team rows
  for (i in nrow(df):1) { if (df$V1[i]=='NAME' | substr(df$V1[i], 1, 7)=='Comment' | is.na(df$V3[i])) { df = df[-i,]}}
  
  df$Name = as.character(substr(df$V1,1,regexpr(',',df$V1)-1))
  df$Position = as.character(substr(df$V1,regexpr(',',df$V1)+2,nchar(as.character(df$V1))))
  df$InjuryStatus = as.character(df$V2)
  
  
  
  df = df[,c('Name','Position','InjuryStatus')]
  str(df)
  
  df = convertPositions(df)
  df = addNameRM(df)
  df = createKey(df)
  
  df = df[!(df$Key == 'WRjarvislandry'),]

  return(df)
}

#Remove