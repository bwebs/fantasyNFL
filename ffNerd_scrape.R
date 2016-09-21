require(jsonlite)
require(RCurl)

#http://www.fantasyfootballnerd.com/service/weekly-projections/xml/{$FFNKEY}/{$Pos}/{$WeekNo}
week=1
pos = c('QB','RB','WR')

category = data.frame(value=c(0,2,4,6,17,16), pos=c("QB",'RB','WR','TE','K','D')) #QB

url = 'http://www.fantasyfootballnerd.com/service/weekly-projections/json/a65a75kdbuxr/',week,'/',pos

df = fromJSON(getURL(url))

