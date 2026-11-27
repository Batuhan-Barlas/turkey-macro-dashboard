# Run from repository root. No Shiny server. No API key in source.
# Original macro_db_data.R transformations retained, with explicit time ordering and gaps.
options(timeout=180)
key <- Sys.getenv('EVDS_API_KEY')
if (!nzchar(key)) stop('Set EVDS_API_KEY in your local environment or GitHub Actions secret.')
if (utils::packageVersion('CBRT') < '0.2.0') stop('Use CBRT >= 0.2.0 (EVDS3).')
series <- list()

fetch <- function(codes, start, frequency, group) {

  message("EVDS fetching: ", group,
          " | frequency=", frequency,
          " | series=", length(codes))

  d <- tryCatch(
    suppressWarnings(
      suppressMessages(
        CBRT::getDataSeries(
          unname(codes),
          CBRTKey = key,
          startDate = start,
          freq = frequency
        )
      )
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      msg <- gsub(key, "[REDACTED]", msg, fixed = TRUE)
      msg <- gsub("https?://[^[:space:]]+",
                  "[URL REDACTED]", msg)
      stop("EVDS request failed [", group, "]: ",
           msg, call. = FALSE)
    }
  )

  if (is.null(d)) {
    stop("EVDS returned NULL: ", group)
  }

  d <- as.data.frame(d)

  names(d) <- gsub("_", ".", names(d), fixed = TRUE)

  message("EVDS rows: ", nrow(d))
  message("EVDS columns: ", paste(names(d), collapse = ", "))

  required <- c("time", unname(codes))
  missing_cols <- setdiff(required, names(d))

  if (length(missing_cols) > 0) {
    stop("Missing EVDS columns [", group, "]: ",
         paste(missing_cols, collapse = ", "))
  }

  d <- d[, required, drop = FALSE]
  names(d) <- c("date", names(codes))

  if (frequency == 6 && is.numeric(d$date) &&
      !inherits(d$date, "Date")) {

    year <- floor(d$date)
    quarter <- round((d$date - year) * 4) + 1

    if (anyNA(quarter) ||
        any(!quarter %in% 1:4) ||
        any(year < 1900 | year > 2100) ||
        any(abs((d$date - year) * 4 -
                (quarter - 1)) > 1e-6)) {
      stop("Invalid quarterly dates: ", group)
    }

    d$date <- as.Date(
      sprintf("%04d-%02d-01",
              as.integer(year),
              as.integer(1 + 3 * (quarter - 1)))
    )

  } else {
    d$date <- as.Date(d$date)
  }

  if (anyNA(d$date)) {
    stop("Invalid dates: ", group)
  }

  if (frequency == 5) {
    d$date <- as.Date(format(d$date, "%Y-%m-01"))
  }

  d <- d[order(d$date), , drop = FALSE]

  if (anyDuplicated(d$date)) {
    stop("Duplicate dates: ", group)
  }

  for (nm in names(codes)) {
    d[[nm]] <- as.numeric(d[[nm]])

    if (!any(is.finite(d[[nm]]))) {
      stop("No numeric observations: ", group, " / ", nm)
    }

    message(
      "  ", nm,
      ": ", sum(is.finite(d[[nm]])),
      " valid observations"
    )
  }

  if (frequency == 5) {
    d <- merge(
      data.frame(
        date = seq(min(d$date), max(d$date), by = "month")
      ),
      d,
      by = "date",
      all.x = TRUE,
      sort = TRUE
    )
  }

  message("EVDS success: ", group,
          " | ", min(d$date), " to ", max(d$date))

  return(d)
}

add <- function(id, date, value, title, block, freq, unit, source, transform) {
  keep <- !is.na(date) & is.finite(value)
  date <- as.character(date[keep]); value <- as.numeric(value[keep])
  if (!length(value) || anyDuplicated(date) || is.unsorted(date)) stop(paste('Invalid output:',id))
  series[[paste0('TR_',id)]] <<- list(titulo_serie=title,subtitulo_descripcion=paste('Türkiye.',title,unit),transformacion_y_calculo=transform,fuente=source,frecuencia_y_cobertura_temporal=paste(min(date),'to',max(date)),tipo_grafico='Line',bloque=block,'área'=block,frecuencia=freq,unidad=unit,observaciones=unname(Map(function(d,v) list(d,v),date,value)))
}
lag_n <- function(x,n) { if(length(x)<=n) return(rep(NA_real_,length(x))); c(rep(NA_real_,n),head(x,-n)) }
roll <- function(x,n,fun) zoo::rollapplyr(x,n,fun,fill=NA_real_,partial=FALSE)
# Rates: keep native weekly and daily separate, avoiding implicit mixed-frequency retrieval.
rates <- c(GPL='TP.KTF10',AUTO='TP.KTF11',MORTGAGE='TP.KTF12',COMMERCIAL='TP.KTF17',CONSUMER='TP.KTFTUK',DEPOSIT='TP.TRY.MT06')
r <- fetch(rates,'04-01-2002',3,'weekly loan and deposit rates')
for (nm in names(rates)) add(nm,r$date,r[[nm]],nm,'INTEREST_RATES','Semanal','%',paste('CBRT EVDS',rates[[nm]]),'As published; weekly flow rate')
for (nm in c('COMMERCIAL','CONSUMER')) {
  id <- if(nm=='COMMERCIAL') 'COMM_SPREAD' else 'CONS_SPREAD'
  add(id,r$date,100*((1+r[[nm]]/100)/(1+r$DEPOSIT/100)-1),paste(nm,'spread'),'INTEREST_RATES','Semanal','%','CBRT EVDS','100 * ((1 + loan rate/100)/(1 + TL deposit rate/100) - 1)')
}
daily <- c(FUNDING='TP.APIFON4',REPO='TP.AOFOBAP')
d <- fetch(daily,'04-01-2002',1,'daily funding rates')
for (nm in names(daily)) add(nm,d$date,d[[nm]],nm,'INTEREST_RATES','Diaria','%',paste('CBRT EVDS',daily[[nm]]),'As published')
# Interest-rate corridor is optional until the live CBRT HTML table has been verified.
# A local, approved CSV with columns date, LOWER_BAND, UPPER_BAND can be supplied.
if(file.exists('inputs/rate_bands.csv')) {
  a <- read.csv('inputs/rate_bands.csv'); stopifnot(all(c('date','LOWER_BAND','UPPER_BAND') %in% names(a)))
  a$date <- as.Date(a$date); a <- a[order(a$date),]; stopifnot(!anyNA(a$date),!anyDuplicated(a$date))
  dates <- seq(min(a$date),Sys.Date(),by='day'); ix <- findInterval(dates,a$date)
  for(nm in c('LOWER_BAND','UPPER_BAND')) add(nm,dates,a[[nm]][ix],nm,'INTEREST_RATES','Diaria','%','CBRT interest-rate corridor (provided CSV)','Last effective rate carried forward')
} else message('Optional corridor CSV absent; corridor lines omitted.')
# CPI, 2025-base series from the supplied Shiny source.
cpi <- c(CPI_HEADLINE='TP.TUKFIY2025.GENEL',CPI_CORE_B='TP.FE25.OKTG03',CPI_CORE_C='TP.FE25.OKTG04',CPI_GOODS='TP.FE25.OKTG08',CPI_SERVICES='TP.FE25.OKTG23')
c <- fetch(cpi,'01-01-2003',5,'CPI indices')
for(nm in names(cpi)) { x <- c[[nm]]; add(nm,c$date,100*(x-lag_n(x,12))/lag_n(x,12),nm,'INFLATION','Mensual','% y/y',paste('CBRT EVDS / TurkStat',cpi[[nm]]),'100*(index/index[t-12]-1)') }
# TurkStat SA index levels: local explicit override or live workbook.
# A failed download stops deployment instead of silently dropping charts.
{
  if(file.exists('inputs/cpi_sa.csv')) {
    a <- read.csv('inputs/cpi_sa.csv')
  } else {
    source('R/fetch_tuik_cpi.R', encoding='UTF-8')
    a <- fetch_tuik_cpi()
  }
  cols <- c('CPI','Core_B','Core_C','Goods','Services')
  stopifnot(all(c('date',cols)%in%names(a))); a$date <- as.Date(format(as.Date(a$date),'%Y-%m-01'))
  stopifnot(!anyNA(a$date),!anyDuplicated(a$date)); a <- merge(data.frame(date=seq(min(a$date),max(a$date),by='month')),a,by='date',all.x=TRUE,sort=TRUE)
  for(nm in cols){x<-a[[nm]]; add(paste0('TREND_',toupper(nm)),a$date,roll(100*(x-lag_n(x,1))/lag_n(x,1),3,mean),paste(nm,'trend'),'INFLATION','Mensual','% m/m','TurkStat (official SA index levels)','Trailing 3-month arithmetic mean of SA month-on-month rates; not annualised')}
}
# BOP sign convention follows Shiny exactly; source values assumed USD million (verify).
bop <- c(BOP_CAD='TP.ODANA6.Q01',BOP_FIN='TP.ODANA6.Q13',BOP_NEO='TP.ODANA6.Q31',BOP_RES='TP.ODANA6.Q33')
b <- fetch(bop,'01-01-1992',5,'balance of payments')
for(nm in names(bop)) {
  x <- b[[nm]] * if(nm=='BOP_NEO') 1 else -1
  add(nm,b$date,x/1e3,nm,'BALANCE_OF_PAYMENTS','Mensual','USD bn',paste('CBRT EVDS',bop[[nm]]),'Shiny sign convention; assumed USD million divided by 1000')
  add(paste0(nm,'_12M'),b$date,roll(x,12,sum)/1e3,paste(nm,'12-month sum'),'BALANCE_OF_PAYMENTS','Mensual','USD bn',paste('CBRT EVDS',bop[[nm]]),'Trailing sum of 12 complete months; Shiny signs; divide by 1000')
}
trade <- c(TRADE_EXPORT='TP.IHRACATBEC.9999',TRADE_IMPORT='TP.ITHALATBEC.9999')
t <- fetch(trade,'01-01-2013',5,'external trade')
t$TRADE_BALANCE <- t$TRADE_EXPORT-t$TRADE_IMPORT
for(nm in c(names(trade),'TRADE_BALANCE')) {
  x <- t[[nm]]; src <- paste('CBRT EVDS',if(nm %in% names(trade)) trade[[nm]] else paste(trade,collapse=' minus '))
  add(nm,t$date,x/1e6,nm,'EXTERNAL_TRADE','Mensual','USD bn',src,'Shiny scaling: assumed USD thousand divided by 1e6; balance exports minus imports')
  add(paste0(nm,'_12M'),t$date,roll(x,12,sum)/1e6,paste(nm,'12-month sum'),'EXTERNAL_TRADE','Mensual','USD bn',src,'Trailing 12-month sum; divide by 1e6')
}
# GDP module runs before atomic snapshot export.
source('R/national_accounts.R', encoding='UTF-8')
update_gdp()
# Export exact original Turkey snapshot structure, with explicit weekly extension.
# No existing internal/example snapshot is copied automatically.
path <- 'site/drive_ingestion/snapshots/turkey.json'
dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(list(origen=list(name='R EVDS and TurkStat pipeline',updated_at=format(Sys.time(),tz='UTC',usetz=TRUE)),series=series),paste0(path,'.tmp'),auto_unbox=TRUE,digits=NA,pretty=FALSE,na='null')
stopifnot(file.rename(paste0(path,'.tmp'),path)); message(length(series),' series exported.')
