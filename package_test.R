#install required packages
install.packages(c("devtools","rstudioapi","shiny","miniUI","data.table","stringr","DT","XML","httr","tcltk","RCurl","Hmisc","readxl","RSelenium"), dependencies=TRUE, repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))

#install ffanalytics from github
devtools::install_github(repo = "dadrivr/FantasyFootballAnalyticsR",  subdir = "R Package/ffanalytics")

library("ffanalytics")

# test analysts
analysts[analysts$analystId == 4, "weight"] <- 0.5
str(analysts)


#QB from the start of season in 2016 from ESPN
myScrapeData <- runScrape(week = 0, season = 2016, analysts = 4, positions = c("QB"), fbgUser = NULL, fbgPwd = NULL)

str(myScrapeData)

#Change QB results to dataframe
results = data.frame(myScrapeData$QB@resultData)
#View the results
results[order(results$player),c("player","passInt","passYds")]
#check how many predictions we have
sum(results$passYds!=0, na.rm=TRUE)

write.csv(results,"qb_test.csv",row.names=FALSE)

analystCSV = 'https://raw.githubusercontent.com/dadrivr/FantasyFootballAnalyticsR/390804bef8c7cc2c3e655bd69146f016ec82273d/R%20Package/ffanalytics/data-raw/siteUrls.csv'
analURL = read.csv(analystCSV, stringsAsFactors=FALSE)
write.csv(analURL, 'test.csv')

str(siteUrls)
