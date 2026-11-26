# Quarterly GDP module. Existing fetch(), add() and lag_n() are provided by update_data.R.
# A reviewed CSV can override catalogue discovery without editing calculation code.
resolve_gdp_codes <- function() {
  groups <- c('demand_real','demand_nominal','gdp_sca','supply_real','supply_nominal')
  demand <- c('GDP','C','I','G','X','M')
  supply <- c('A','IND','F','GI','J','K','L','MN','OQ','RU')
  expected <- list(demand_real=demand,demand_nominal=demand,gdp_sca='GDP',supply_real=supply,supply_nominal=supply)
  norm <- function(x) { x <- tolower(chartr('İIıŞşĞğÜüÖöÇç','iiissgguuoocc',x)); gsub('[^a-z0-9]+',' ',x) }
  if(file.exists('inputs/gdp_series.csv')) {
    map <- read.csv('inputs/gdp_series.csv',stringsAsFactors=FALSE)
  } else {
    cat <- as.data.frame(CBRT::allCBRTSeries)
    g <- norm(cat$groupName); s <- norm(cat$seriesName)
    q <- (cat$freq==6 | grepl('quarter|uc ay|ceyrek',norm(cat$freqname))) & !grepl('arsiv|archiv',g)
    exp <- grepl('harcama|expenditure',g); prod <- grepl('uretim|production',g)
    adj <- grepl('arindiril|adjusted',g); chain <- grepl('zincir|chain',g); nom <- grepl('cari fiyat|current price',g)
    filters <- list(demand_real=exp & chain & !adj & !grepl('endeks|index',g),demand_nominal=exp & nom & !adj,gdp_sca=exp & chain & grepl('mevsim.*takvim|seasonal.*calendar|calendar.*seasonal',g),supply_real=prod & chain & !adj & grepl('a10|a 10',g),supply_nominal=prod & nom & !adj & grepl('a10|a 10',g))
    patterns <- c(GDP='gayrisafi yurt ici hasila|gross domestic product|\\bgsyh\\b|\\bgdp\\b|\\btoplam\\b',C='hanehalk.*nihai tuketim|household.*final consumption',I='gayrisafi sabit sermaye|gross fixed capital',G='devlet.*nihai tuketim|government.*final consumption',X='mal ve hizmet ihrac|exports of goods and services',M='mal ve hizmet ithal|imports of goods and services',A='tarim.*ormanc|agriculture.*forestry',IND='\\bsanayi\\b|\\bindustry\\b',F='\\binsaat\\b|\\bconstruction\\b',GI='ticaret.*ulas|trade.*transport',J='bilgi.*iletisim|information.*communication',K='finans.*sigorta|financial.*insurance',L='gayrimenkul|real estate',MN='mesleki.*idari|professional.*administrative',OQ='kamu yonetimi.*egitim|public administration.*education',RU='diger hizmet|other service')
    rows <- list()
    for(group in groups) for(item in expected[[group]]) {
      ix <- which(q & filters[[group]] & grepl(patterns[[item]],s))
      # Never silently choose between multiple catalogue matches.
      if(length(ix)!=1) stop(sprintf('GDP mapping needs review: %s/%s (%d matches). Run Rscript R/list_gdp_catalogue.R and supply inputs/gdp_series.csv.',group,item,length(ix)))
      rows[[length(rows)+1]] <- data.frame(group=group,item=item,code=cat$seriesCode[ix])
    }
    map <- do.call(rbind,rows)
  }
  stopifnot(all(c('group','item','code') %in% names(map)), !anyDuplicated(map[c('group','item')]), all(grepl('^TP\\.',map$code)), !anyDuplicated(map$code))
  out <- lapply(groups,function(g) { m <- map[map$group==g,]; stopifnot(setequal(m$item,expected[[g]])); setNames(m$code,m$item) }); names(out)<-groups
  out
}
update_gdp <- function() {
  codes <- resolve_gdp_codes()
  for(g in names(codes)) message(g, ": ", paste(names(codes[[g]]), codes[[g]], sep="=", collapse=", "))
  raw <- lapply(names(codes),function(g) fetch(codes[[g]],'01-01-2009',6,g)); names(raw)<-names(codes)
  # Normalize dates and join to a full calendar before calculating lags.
  for(g in names(raw)) { d<-raw[[g]]; m<-as.integer(format(d$date,'%m')); d$date<-as.Date(sprintf('%s-%02d-01',format(d$date,'%Y'),1+3*((m-1)%/%3))); stopifnot(!anyDuplicated(d$date)); raw[[g]]<-d }
  dates<-seq(min(do.call(c,lapply(raw,function(d)d$date))),max(do.call(c,lapply(raw,function(d)d$date))),by='quarter')
  raw<-lapply(raw,function(d) merge(data.frame(date=dates),d,by='date',all.x=TRUE,sort=TRUE))
  growth<-function(x,n) { l<-lag_n(x,n); ifelse(l>0,100*(x/l-1),NA_real_) }
  yoy<-growth(raw$demand_real$GDP,4); qoq<-growth(raw$gdp_sca$GDP,1)
  denom<-lag_n(raw$demand_nominal$GDP,4)
  contrib<-function(r,n,item) growth(r[[item]],4)*lag_n(n[[item]],4)/denom
  D<-sapply(c('C','I','G','X','M'),function(i) contrib(raw$demand_real,raw$demand_nominal,i))
  P<-sapply(names(codes$supply_real),function(i) contrib(raw$supply_real,raw$supply_nominal,i))
  demand<-cbind(CONSUMPTION=D[,'C'],INVESTMENT=D[,'I'],GOVERNMENT=D[,'G'],NET_EXPORTS=D[,'X']-D[,'M'])
  supply<-cbind(AGRICULTURE=P[,'A'],INDUSTRY=P[,'IND'],CONSTRUCTION=P[,'F'],SERVICES=rowSums(P[,c('GI','J','K','L','MN','OQ','RU'),drop=FALSE]))
  publish<-function(id,x,title,block,unit,src,method) add(id,dates,x,title,block,'Trimestral',unit,paste('TurkStat via CBRT EVDS:',paste(src,collapse=', ')),method)
  publish('GDP_YOY',yoy,'Real GDP YoY','GDP_EXPENDITURE','% y/y',codes$demand_real['GDP'],'100*(real GDP / same quarter last year - 1)')
  publish('GDP_QOQ_SCA',qoq,'Real GDP QoQ (SCA)','GDP_EXPENDITURE','% q/q',codes$gdp_sca,'100*(SCA index / previous quarter - 1); not annualised')
  for(side in c('demand','supply')) {
    mat<-if(side=='demand') demand else supply
    mat<-cbind(mat,RESIDUAL=yoy-rowSums(mat))
    mat[!is.finite(yoy)|!apply(is.finite(mat),1,all),]<-NA_real_
    if(!any(complete.cases(mat))) stop(paste('No complete GDP decomposition:',side))
    block<-if(side=='demand') 'GDP_EXPENDITURE' else 'GDP_PRODUCTION'
    src<-if(side=='demand') c(codes$demand_real,codes$demand_nominal) else c(codes$supply_real,codes$supply_nominal,codes$demand_real['GDP'],codes$demand_nominal['GDP'])
    for(i in colnames(mat)) publish(paste0('GDP_',toupper(side),'_',i),mat[,i],paste(side,i),block,'pp',src,if(i=='RESIDUAL') 'GDP YoY less displayed estimated contributions; includes omitted components and chain-linking discrepancy' else 'Estimated pp: lagged same-quarter nominal component/GDP share times real YoY growth; imports subtracted; services summed across sectors')
  }
}
