require(httr)
require(XML)
require(RCurl)

fpro = getURL('https://www.fantasypros.com/nfl/daily-fantasy-lineup-optimizer.php')
doc = htmlParse(fpro)
table = readHTMLTable(doc, trim=TRUE, stringsAsFactors = FALSE)
df = table$'player-pool'
df$Name = substr(df$`Player Name`, 1, regexpr(',',df$`Player Name`)-1)
df$Position = substr(df$`Pos. Rank`,1,regexpr(' ',df$`Pos. Rank`)-1)
df$Position = ifelse(df$Position=='DST','D',df$Position)
df$PredictedPoints = as.double(df$`Proj. Pts`)
df$Source = 'FantasyPros'
playerScrape = df[,c('Name','Position','PredictedPoints','Source')]
