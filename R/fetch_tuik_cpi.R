
# Return official seasonally adjusted CPI index levels.
fetch_tuik_cpi <- function() {

  source(
    "R/tuik_extractor.R",
    local = TRUE,
    encoding = "UTF-8"
  )

  extractor <- TUIKExtractor$new(lang = "tr")

  themes <- extractor$themes()

  price <- themes[
    grepl("Fiyat", themes$name, ignore.case = TRUE),
    , drop = FALSE
  ]

  if (nrow(price) != 1L) {
    stop("TurkStat price theme missing or ambiguous")
  }

  files <- extractor$search_files(
    "Mevsim etkisinden",
    within = price
  )

  if (!nrow(files)) {
    stop("TurkStat SA workbook not found")
  }

  # Download each candidate only once.
  # Reuse the same file for sheet discovery and reading.

  workbooks <- list()

  for (i in seq_len(nrow(files))) {

    path <- extractor$download_to_temp(
      files[i, , drop = FALSE]
    )

    on.exit(unlink(path), add = TRUE)

    fmt <- .detect_file_format_from_path(path)

    message(
      "TurkStat candidate ", i,
      ": format=", fmt,
      ", bytes=", file.info(path)$size
    )

    if (!fmt %in% c("xls", "xlsx")) {
      stop(
        "Unexpected TurkStat download format: ",
        fmt
      )
    }

    sheets <- .try_excel_sheets(path, fmt)

    if (is.null(sheets)) {
      stop("Cannot read TurkStat workbook sheets")
    }

    if ("mevsim_etki" %in% sheets) {

      df <- .try_read_excel_df(
        path,
        fmt,
        sheet_name = "mevsim_etki"
      )

      if (is.null(df)) {
        stop("Cannot read mevsim_etki sheet")
      }

      workbooks[[length(workbooks) + 1L]] <- df
    }
  }

  if (length(workbooks) != 1L) {
    stop(
      "TurkStat SA workbook missing or ambiguous"
    )
  }

  df <- workbooks[[1L]]

  message(
    "TurkStat workbook dimensions: ",
    nrow(df), " rows x ",
    ncol(df), " columns"
  )

  # Preserve original Shiny data transformation.

  a <- as.data.frame(
    base::t(df),
    stringsAsFactors = FALSE
  )

  if (ncol(a) < 34L || nrow(a) < 4L) {
    stop("Unexpected TurkStat workbook dimensions")
  }

  a <- a[, 6:34, drop = FALSE]
  a <- a[, -3, drop = FALSE]

  a[3, 1] <- "year"

  names(a) <- as.character(
    unlist(a[3, , drop = FALSE], use.names = FALSE)
  )

  if (!identical(
    names(a)[1:2],
    c("year", "Coverage")
  )) {
    stop("TurkStat date headers changed")
  }

  a <- a[-(1:3), , drop = FALSE]

  month <- vapply(
    a$Coverage,
    function(x) {

      if (is.na(x)) return(NA_integer_)

      m <- parse_month_value(
        trimws(
          sub(
            "\\s*\\(r\\)\\s*$",
            "",
            as.character(x)
          )
        )
      )

      if (is.null(m)) {
        NA_integer_
      } else {
        as.integer(m)
      }
    },
    integer(1)
  )

  year <- suppressWarnings(
    as.integer(as.character(a$year))
  )

  idx <- which(
    !is.na(month) &
    !is.na(year) &
    year >= 1900 &
    year <= as.integer(format(Sys.Date(), "%Y"))
  )

  if (!length(idx) || any(diff(idx) != 1L)) {
    stop("TurkStat dates missing or interrupted")
  }

  if (any(
    !is.na(month) &
    !(seq_len(nrow(a)) %in% idx)
  )) {
    stop("TurkStat month without valid year")
  }

  date <- as.Date(
    sprintf(
      "%04d-%02d-01",
      year[idx],
      month[idx]
    )
  )

  if (
    anyDuplicated(date) ||
    is.unsorted(date) ||
    any(date > Sys.Date())
  ) {
    stop("Invalid TurkStat dates")
  }

  # Preserve original series positions.

  positions <- c(3, 5, 6, 7, 23)

  message(
    "TurkStat selected headers: ",
    paste(names(a)[positions], collapse = " | ")
  )

  out <- data.frame(date = date)

  series_names <- c(
    "CPI",
    "Core_B",
    "Core_C",
    "Goods",
    "Services"
  )

  for (i in seq_along(positions)) {

    x <- as.character(a[idx, positions[i]])

    v <- suppressWarnings(as.numeric(x))

    if (
      any(
        !is.na(x) &
        nzchar(trimws(x)) &
        !is.finite(v)
      ) ||
      any(v <= 0, na.rm = TRUE) ||
      sum(is.finite(v)) < 4L
    ) {
      stop(
        "Invalid TurkStat index levels: ",
        names(a)[positions[i]]
      )
    }

    out[[series_names[i]]] <- v
  }

  message(
    "TurkStat CPI observations: ", nrow(out),
    "; latest date: ", max(out$date)
  )

  out
}
