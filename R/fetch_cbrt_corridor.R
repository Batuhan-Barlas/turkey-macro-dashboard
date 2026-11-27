
# Retrieve CBRT overnight borrowing and lending rates
# from the official interest rate decisions table.

fetch_cbrt_corridor <- function() {

  url <- paste0(
    "https://www.tcmb.gov.tr/wps/wcm/connect/TR/",
    "TCMB+TR/Main+Menu/Temel+Faaliyetler/",
    "Para+Politikasi/Merkez+Bankasi+Faiz+Oranlari/",
    "faiz-oranlari"
  )

  message("CBRT: fetching interest rate corridor")

  tables <- rvest::read_html(url) |>
    rvest::html_table(fill = TRUE)

  required <- c("Tarih", "Borç Alma", "Borç Verme")

  # Identify the relevant HTML table.
  corridor <- NULL

  for (tab in tables) {

    tab <- as.data.frame(tab)
    names(tab) <- trimws(names(tab))

    if (!all(required %in% names(tab)) && nrow(tab) > 0L) {
      names(tab) <- trimws(as.character(unlist(tab[1, ])))
      tab <- tab[-1, , drop = FALSE]
    }

    if (all(required %in% names(tab))) {
      corridor <- tab
      break
    }
  }

  if (is.null(corridor)) {
    stop("CBRT corridor table not found.")
  }

  parse_rate <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub("%", "", x, fixed = TRUE)
    x <- gsub(",", ".", x, fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }

  raw_dates <- trimws(as.character(corridor[["Tarih"]]))
  dates <- as.Date(rep(NA_character_, length(raw_dates)))

  short_year <- grepl(
    "^\\d{2}\\.\\d{2}\\.\\d{2}$", raw_dates
  )

  full_year <- grepl(
    "^\\d{2}\\.\\d{2}\\.\\d{4}$", raw_dates
  )

  dates[short_year] <- as.Date(
    raw_dates[short_year], format = "%d.%m.%y"
  )

  dates[full_year] <- as.Date(
    raw_dates[full_year], format = "%d.%m.%Y"
  )

  a <- data.frame(
    date = dates,
    LOWER_BAND = parse_rate(corridor[["Borç Alma"]]),
    UPPER_BAND = parse_rate(corridor[["Borç Verme"]])
  )

  # Remove non-observation rows.
  a <- a[!is.na(a$date), , drop = FALSE]
  a <- unique(a)
  a <- a[order(a$date), , drop = FALSE]

  # Validate the historical rate decisions.
  stopifnot(
    nrow(a) > 0L,
    !anyDuplicated(a$date),
    all(is.finite(a$LOWER_BAND)),
    all(is.finite(a$UPPER_BAND)),
    all(a$LOWER_BAND <= a$UPPER_BAND),
    all(a$date <= Sys.Date())
  )

  message(
    "CBRT corridor: ", nrow(a),
    " rate decisions retrieved"
  )

  message(
    "CBRT corridor coverage: ",
    min(a$date), " to ", max(a$date)
  )

  message(
    "Latest lower band: ", tail(a$LOWER_BAND, 1),
    "% | Latest upper band: ", tail(a$UPPER_BAND, 1), "%"
  )

  return(a)
}
