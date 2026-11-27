# Run locally with CBRT >= 0.2.0; no API key required for bundled catalogue.
d <- as.data.frame(CBRT::allCBRTSeries)
keep <- grepl('GSYIH|GSYH|GDP',d$seriesCode,ignore.case=TRUE) | grepl('Gayrisafi Yurt|Gross Domestic',d$groupName,ignore.case=TRUE)
write.csv(d[keep,c('groupCode','groupName','freq','seriesCode','seriesName')],'gdp_catalogue.csv',row.names=FALSE,fileEncoding='UTF-8')
