# Return official SA index levels, not growth rates. Based on macro_db_data.R.
fetch_tuik_cpi <- function() {
  source('R/tuik_extractor.R', local=TRUE, encoding='UTF-8')
  extractor <- TUIKExtractor$new(lang='tr')
  themes <- extractor$themes()
  price <- themes[grepl('Fiyat', themes$name, ignore.case=TRUE),,drop=FALSE]
  if(nrow(price)!=1L) stop('TurkStat price theme missing or ambiguous')
  files <- extractor$search_files('Mevsim etkisinden', within=price)
  if(!nrow(files)) stop('TurkStat SA workbook not found')
  # Original Shiny selects first match. Require exactly one with expected sheet.
  candidates <- which(vapply(seq_len(nrow(files)), function(i) {
    'mevsim_etki' %in% extractor$workbook_sheets(files[i,,drop=FALSE])
  }, logical(1)))
  if(length(candidates)!=1L) stop('TurkStat SA workbook missing or ambiguous; inspect search results')
  df <- extractor$sheet_df(files[candidates,,drop=FALSE], 'mevsim_etki')
  # Preserve original transpose, V6:V34 selection, third-column deletion,
  # header row 3 and final series positions. Fail on changed layout.
  a <- as.data.frame(base::t(df), stringsAsFactors=FALSE)
  if(ncol(a)<34L || nrow(a)<4L) stop('Unexpected TurkStat workbook dimensions')
  a <- a[,6:34,drop=FALSE]; a <- a[,-3,drop=FALSE]
  a[3,1] <- 'year'; names(a) <- as.character(unlist(a[3,,drop=FALSE],use.names=FALSE))
  if(!identical(names(a)[1:2],c('year','Coverage'))) stop('TurkStat date headers changed')
  a <- a[-(1:3),,drop=FALSE]
  month <- vapply(a$Coverage,function(x) {
    if(is.na(x)) return(NA_integer_)
    m <- parse_month_value(trimws(sub('\\s*\\(r\\)\\s*$','',as.character(x))))
    if(is.null(m)) NA_integer_ else as.integer(m)
  },integer(1))
  year <- suppressWarnings(as.integer(as.character(a$year)))
  # Only discard trailing/non-observation rows; reject incomplete date rows.
  idx <- which(!is.na(month) & !is.na(year) & year>=1900 & year<=as.integer(format(Sys.Date(),'%Y')))
  if(!length(idx) || any(diff(idx)!=1L)) stop('TurkStat dates missing or interrupted')
  if(any(!is.na(month) & !(seq_len(nrow(a)) %in% idx))) stop('TurkStat month without valid year')
  date <- as.Date(sprintf('%04d-%02d-01',year[idx],month[idx]))
  if(anyDuplicated(date) || is.unsorted(date) || any(date>Sys.Date())) stop('Invalid TurkStat dates')
  # Shiny relocates date, removes year/Coverage, then selects 1,2,4,5,6,22.
  positions <- c(3,5,6,7,23)
  message('TurkStat selected headers: ',paste(names(a)[positions],collapse=' | '))
  out <- data.frame(date=date)
  for(i in seq_along(positions)) {
    x <- as.character(a[idx,positions[i]])
    v <- suppressWarnings(as.numeric(x))
    if(any(!is.na(x) & nzchar(trimws(x)) & !is.finite(v)) || any(v<=0,na.rm=TRUE) || sum(is.finite(v))<4L) stop('Invalid TurkStat index levels: ',names(a)[positions[i]])
    out[[c('CPI','Core_B','Core_C','Goods','Services')[i]]] <- v
  }
  out
}
