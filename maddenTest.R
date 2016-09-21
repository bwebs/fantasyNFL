library(jsonlite)
library(curl)
library(RCurl)
library(httr)
library(curlconverter)


#myDF = fromJSON('http://www.easports.com/madden-nfl/ultimate-team/web-app/data/1/cards/player?sort=ovr&pos=QB&sort=ovr')$items
#myDF$team = myDF$teamInfo$abbr

myDF = fromJSON('https://www.easports.com/madden-nfl/ratings/service/data?entityType=madden17_player&&limit=20000')$docs
str(myDF)
write.csv(myDF,'Exports/madden.csv', row.names=FALSE)

teams = fromJSON('http://feeds.nfl.com/feeds-rs/teams/2016.json')$teams
test = fromJSON('http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2015&week=1&format=json')

# http://api.fantasy.nfl.com/v1/docs/service?serviceName=playersDetails
# http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json
# http://stackoverflow.com/questions/33489239/how-do-you-access-the-nfls-apis

test2 = fromJSON('http://api.fantasy.nfl.com/v1/players/researchinfo?season=2016&week=1&count=10000&format=json')
playerById = fromJSON('http://api.fantasy.nfl.com/v1/players/details?playerId=2508061&format=json ')

str(playerById)
table(test2$players$depthChartOrder)
str(test2)

# http://www.fantasyfootballnerd.com/service/players/json/a65a75kdbuxr/QB/
# http://www.fantasyfootballnerd.com/fantasy-football-api#players

test3 = fromJSON('http://www.fantasyfootballnerd.com/service/players/json/a65a75kdbuxr/QB/')


# https://developer.fantasydata.com
# https://api.fantasydata.net/v3/nfl/projections/{format}/FantasyDefenseProjectionsByGame/{season}/{week}
#devtools::install_github("hrbrmstr/curlconverter")
url = 'https://api.fantasydata.net/v3/nfl/projections/JSON/FantasyDefenseProjectionsByGame/2016/1'
var1.1 = 'Host'
var1.2 = 'api.fantasydata.net'
var2.1 = 'Ocp-Apim-Subscription-Key'
var2.2 = "4b609a2dc3a24636b980d4ae8886cfef"
varlist = list('Host'=var1.2,'Ocp-Apim-Subscription-Key'=var2.2)

res = getURL(url, httpheader=varlist)
rep1 = fromJSON(res)

url2 = 'https://api.fantasydata.net/v3/nfl/projections/JSON/PlayerGameProjectionStatsByWeek/2016REG/1' 
rep2 = fromJSON(getURL(url2, httpheader=varlist))
