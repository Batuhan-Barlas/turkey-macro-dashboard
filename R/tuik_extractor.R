BASE <- "https://veriportali.tuik.gov.tr"
TREE_URL <- function(lang) sprintf("%s/api/%s/data/statistical-themes", BASE, lang)

.default_headers <- c(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36",
  "Accept" = "application/json, text/plain, */*",
  "Accept-Language" = "tr-TR,tr;q=0.9,en-US;q=0.8,en;q=0.7",
  "Accept-Encoding" = "gzip, deflate, br, zstd",
  "Referer" = paste0(BASE, "/tr/statistical-themes"),
  "sec-ch-ua" = '"Not:A-Brand";v="99", "Google Chrome";v="145", "Chromium";v="145"',
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = '"Windows"',
  "sec-fetch-dest" = "empty",
  "sec-fetch-mode" = "cors",
  "sec-fetch-site" = "same-origin",
  "x-requested-with" = "XMLHttpRequest",
  "x-client-referrer" = "https://www.tuik.gov.tr/",
  "Connection" = "keep-alive"
)

.require_packages <- function() {
  pkgs <- c("R6", "httr", "jsonlite", "readxl", "openxlsx")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      sprintf(
        "Missing required packages: %s. Install them with install.packages(c(%s)).",
        paste(missing, collapse = ", "),
        paste(sprintf('"%s"', unique(missing)), collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

`%notin%` <- function(x, table) !(x %in% table)

.norm <- function(text) {
  x <- as.character(text %||% "")
  x <- trimws(x)
  x <- tolower(x)
  gsub("\\s+", " ", x, perl = TRUE)
}

.as_char <- function(x) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) "" else as.character(x[[1]])
}

.node_to_row <- function(node) {
  path <- node[["_path"]] %||% node[["name"]] %||% ""
  data.frame(
    id = .as_char(node[["id"]]),
    name = .as_char(node[["name"]]),
    icon = .as_char(node[["icon"]]),
    url = .as_char(node[["url"]]),
    n_children = length(node[["children"]] %||% list()),
    path = path,
    full_path = path,
    stringsAsFactors = FALSE
  )
}

.flatten_tree <- function(nodes, path = character(), result = list()) {
  if (is.null(nodes) || length(nodes) == 0) return(result)
  for (node in nodes) {
    if (!is.list(node)) next
    enriched <- node
    name <- node[["name"]] %||% node[["title"]] %||% node[["label"]] %||% ""
    enriched[["name"]] <- name
    enriched[["_path"]] <- if (nzchar(name)) paste(c(path, name), collapse = " > ") else paste(path, collapse = " > ")
    result[[length(result) + 1]] <- enriched
    children <- node[["children"]] %||% list()
    if (length(children) > 0) {
      next_path <- if (nzchar(name)) c(path, name) else path
      result <- .flatten_tree(children, next_path, result)
    }
  }
  result
}

.find_node <- function(nodes, target_id) {
  if (is.null(nodes) || length(nodes) == 0) return(NULL)
  for (node in nodes) {
    if (!is.list(node)) next
    if (identical(.as_char(node[["id"]]), .as_char(target_id))) return(node)
    found <- .find_node(node[["children"]] %||% list(), target_id)
    if (!is.null(found)) return(found)
  }
  NULL
}

.read_csv_auto <- function(path) {
  encodings <- c("UTF-8", "UTF-8-BOM", "windows-1254", "latin1")
  for (enc in encodings) {
    out <- tryCatch(
      utils::read.csv(path, fileEncoding = enc, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.null(out)) return(out)
  }
  stop("Could not decode CSV.", call. = FALSE)
}

.promote_header <- function(df) {
  if (nrow(df) == 0) return(df)
  sample_rows <- min(10L, nrow(df))
  best_row <- 1L
  best_score <- -1L
  
  for (i in seq_len(sample_rows)) {
    row <- df[i, , drop = FALSE]
    vals <- as.list(row[1, , drop = TRUE])
    non_empty <- sum(!vapply(vals, function(v) is.na(v) || identical(trimws(as.character(v)), ""), logical(1)))
    textual <- sum(vapply(vals, function(v) is.character(v) && !is.na(v) && nzchar(trimws(v)), logical(1)))
    score <- non_empty + textual
    if (score > best_score) {
      best_score <- score
      best_row <- i
    }
  }
  
  headers <- vapply(seq_along(df), function(j) {
    v <- df[best_row, j][[1]]
    if (is.na(v) || trimws(as.character(v)) == "") paste0("col_", j - 1L) else trimws(as.character(v))
  }, character(1))
  
  out <- df[(best_row + 1L):nrow(df), , drop = FALSE]
  if (nrow(out) == 0) return(out)
  names(out) <- headers
  out <- out[rowSums(!is.na(out) & trimws(as.character(as.matrix(out))) != "") > 0, , drop = FALSE]
  keep_cols <- vapply(out, function(col) any(!is.na(col) & trimws(as.character(col)) != ""), logical(1))
  out[, keep_cols, drop = FALSE]
}

.find_best_column <- function(df, keywords, fallback_numeric = FALSE) {
  if (ncol(df) == 0) return(NULL)
  norm_cols <- stats::setNames(vapply(names(df), .norm, character(1)), names(df))
  
  for (kw in keywords) {
    nkw <- .norm(kw)
    for (col in names(norm_cols)) {
      if (nzchar(nkw) && grepl(nkw, norm_cols[[col]], fixed = TRUE)) return(col)
    }
  }
  
  for (kw in keywords) {
    nkw <- .norm(kw)
    for (col in names(df)) {
      ser <- vapply(df[[col]], function(x) .norm(as.character(x)), character(1))
      if (any(grepl(nkw, ser, fixed = TRUE), na.rm = TRUE)) return(col)
    }
  }
  
  if (fallback_numeric) {
    ratios <- vapply(names(df), function(col) {
      ser <- gsub("\\.", "", as.character(df[[col]]))
      ser <- gsub(",", ".", ser, fixed = TRUE)
      mean(!is.na(suppressWarnings(as.numeric(ser))))
    }, numeric(1))
    best <- names(which.max(ratios))
    if (length(best) == 1 && ratios[[best]] > 0.4) return(best)
  }
  
  NULL
}

.excel_cell <- function(row_idx, col_idx) {
  n <- as.integer(col_idx)
  letters <- character()
  while (n > 0L) {
    rem <- (n - 1L) %% 26L
    letters <- c(intToUtf8(65L + rem), letters)
    n <- (n - 1L) %/% 26L
  }
  paste0(paste(letters, collapse = ""), row_idx)
}

excel_cell_to_idx <- function(cell_ref) {
  m <- regexec("^([A-Za-z]+)(\\d+)$", trimws(cell_ref))
  parts <- regmatches(trimws(cell_ref), m)[[1]]
  if (length(parts) == 0) stop(sprintf("Invalid cell reference: %s", cell_ref), call. = FALSE)
  
  col_letters <- toupper(parts[2])
  row_num <- as.integer(parts[3])
  
  col_idx <- 0L
  chars <- strsplit(col_letters, "")[[1]]
  for (ch in chars) {
    col_idx <- col_idx * 26L + (utf8ToInt(ch) - utf8ToInt("A") + 1L)
  }
  
  c(row_idx = row_num - 1L, col_idx = col_idx - 1L)
}

parse_month_value <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NULL)
  }
  
  s <- trimws(as.character(x))
  if (!nzchar(s)) {
    return(NULL)
  }
  
  num_val <- suppressWarnings(as.numeric(s))
  if (!is.na(num_val)) {
    m <- as.integer(num_val)
    if (m >= 1 && m <= 12) {
      return(m)
    }
  }
  
  s_norm <- tolower(enc2utf8(s))
  s_norm <- iconv(s_norm, from = "UTF-8", to = "ASCII//TRANSLIT")
  if (is.na(s_norm)) {
    s_norm <- tolower(s)
  }
  s_norm <- gsub("[^a-z]", "", s_norm)
  
  month_map <- c(
    "ocak" = 1, "oca" = 1, "january" = 1, "jan" = 1,
    "subat" = 2, "sub" = 2, "february" = 2, "feb" = 2,
    "mart" = 3, "mar" = 3,
    "nisan" = 4, "nis" = 4, "april" = 4, "apr" = 4,
    "mayis" = 5, "may" = 5,
    "haziran" = 6, "haz" = 6, "june" = 6, "jun" = 6,
    "temmuz" = 7, "tem" = 7, "july" = 7, "jul" = 7,
    "agustos" = 8, "agu" = 8, "august" = 8, "aug" = 8,
    "eylul" = 9, "eyl" = 9, "september" = 9, "sep" = 9,
    "ekim" = 10, "eki" = 10, "october" = 10, "oct" = 10,
    "kasim" = 11, "kas" = 11, "november" = 11, "nov" = 11,
    "aralik" = 12, "ara" = 12, "december" = 12, "dec" = 12
  )
  
  if (s_norm %in% names(month_map)) {
    return(unname(month_map[[s_norm]]))
  }
  
  return(NULL)
}

.to_numeric_tr <- function(val) {
  if (length(val) == 0 || is.null(val) || is.na(val) || trimws(as.character(val[[1]])) == "") return(NA)
  s <- trimws(as.character(val[[1]]))
  out <- suppressWarnings(as.numeric(s))
  if (!is.na(out)) return(out)
  val[[1]]
}


.detect_file_format_from_path <- function(path) {

  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  raw_head <- readBin(
    con,
    what = "raw",
    n = 512L
  )

  # Print binary signature for diagnostic purposes.
  message(
    "File signature: ",
    paste(
      format(head(raw_head, 8L)),
      collapse = " "
    )
  )

  # XLSX: ZIP container signature.
  if (
    length(raw_head) >= 4L &&
    identical(
      raw_head[1:4],
      as.raw(c(0x50, 0x4B, 0x03, 0x04))
    )
  ) {
    return("xlsx")
  }

  # XLS: OLE compound file signature.
  if (
    length(raw_head) >= 8L &&
    identical(
      raw_head[1:8],
      as.raw(c(
        0xD0, 0xCF, 0x11, 0xE0,
        0xA1, 0xB1, 0x1A, 0xE1
      ))
    )
  ) {
    return("xls")
  }

  # Check whether the server returned HTML.
  text_head <- tryCatch(
    rawToChar(raw_head),
    error = function(e) ""
  )

  text_head_lc <- tolower(text_head)

  if (
    grepl("<html", text_head_lc, fixed = TRUE) ||
    grepl("<!doctype", text_head_lc, fixed = TRUE)
  ) {
    return("html")
  }

  # Fallback: try opening the file directly.
  if (!is.null(.try_excel_sheets(path, "xlsx"))) {
    return("xlsx")
  }

  if (!is.null(.try_excel_sheets(path, "xls"))) {
    return("xls")
  }

  "unknown"
}


extract_series_from_starts_consecutive_dates <- function(
  df,
  start_cells,
  series_names = NULL,
  year_col = "A",
  month_col = "B",
  stop_at_all_blank_rows = TRUE,
  max_rows = NULL
) {
  if (is.character(start_cells) && length(start_cells) == 1L) start_cells <- list(start_cells)
  if (is.character(start_cells)) start_cells <- as.list(start_cells)
  
  if (is.null(series_names)) series_names <- unlist(start_cells)
  if (length(series_names) != length(start_cells)) {
    stop("series_names must have same length as start_cells", call. = FALSE)
  }
  
  start_positions <- lapply(start_cells, excel_cell_to_idx)
  start_rows <- unique(vapply(start_positions, function(x) x[["row_idx"]], integer(1)))
  if (length(start_rows) != 1L) {
    stop("All start_cells must be on the same row, e.g. C8, K8, S8", call. = FALSE)
  }
  start_row <- start_rows[[1]]
  
  year_idx <- excel_cell_to_idx(sprintf("%s%d", year_col, start_row + 1L))[["col_idx"]]
  month_idx <- excel_cell_to_idx(sprintf("%s%d", month_col, start_row + 1L))[["col_idx"]]
  
  first_year_val <- if (year_idx + 1L <= ncol(df)) df[start_row + 1L, year_idx + 1L][[1]] else NA
  first_month_val <- if (month_idx + 1L <= ncol(df)) df[start_row + 1L, month_idx + 1L][[1]] else NA
  
  start_year <- suppressWarnings(as.integer(as.numeric(as.character(first_year_val))))
  if (is.na(start_year)) {
    stop(sprintf("Could not parse start year from %s%d: %s", year_col, start_row + 1L, as.character(first_year_val)), call. = FALSE)
  }
  
  start_month <- parse_month_value(first_month_val)
  if (is.null(start_month) || is.na(start_month)) {
    stop(sprintf("Could not parse start month from %s%d: %s", month_col, start_row + 1L, as.character(first_month_val)), call. = FALSE)
  }
  
  start_date <- as.Date(sprintf("%04d-%02d-01", start_year, start_month))
  last_row <- if (is.null(max_rows)) nrow(df) else min(nrow(df), start_row + max_rows)
  row_idx <- seq.int(start_row + 1L, last_row)
  
  rows <- vector("list", length(row_idx))
  out_i <- 0L
  
  for (r in row_idx) {
    all_blank <- TRUE
    clean_row <- list(date = seq.Date(start_date, by = "month", length.out = (r - start_row))[[r - start_row]])
    
    for (i in seq_along(start_positions)) {
      c_idx <- start_positions[[i]][["col_idx"]] + 1L
      val <- if (c_idx <= ncol(df)) df[r, c_idx][[1]] else NA
      clean_row[[series_names[[i]]]] <- .to_numeric_tr(val)
      if (!(is.null(val) || length(val) == 0 || is.na(val) || trimws(as.character(val)) == "")) {
        all_blank <- FALSE
      }
    }
    
    if (stop_at_all_blank_rows && all_blank) break
    
    out_i <- out_i + 1L
    rows[[out_i]] <- clean_row
  }
  
  if (out_i == 0L) return(data.frame())
  out <- do.call(rbind, lapply(rows[seq_len(out_i)], as.data.frame, stringsAsFactors = FALSE))
  out <- out[order(out$date), , drop = FALSE]
  rownames(out) <- NULL
  out
}

extract_series_long_consecutive_dates <- function(
  df,
  start_cells,
  series_names = NULL,
  year_col = "A",
  month_col = "B",
  stop_at_all_blank_rows = TRUE,
  max_rows = NULL
) {
  wide <- extract_series_from_starts_consecutive_dates(
    df = df,
    start_cells = start_cells,
    series_names = series_names,
    year_col = year_col,
    month_col = month_col,
    stop_at_all_blank_rows = stop_at_all_blank_rows,
    max_rows = max_rows
  )
  
  if (nrow(wide) == 0) return(wide)
  
  series_cols <- setdiff(names(wide), "date")
  out <- data.frame(
    date = rep(wide$date, times = length(series_cols)),
    series = rep(series_cols, each = nrow(wide)),
    value = unlist(wide[series_cols], use.names = FALSE),
    stringsAsFactors = FALSE
  )
  out[order(out$series, out$date), , drop = FALSE]
}

.download_tuik_file_win <- function(url, ext = "xls") {
  if (!grepl("^http", url)) {
    url <- paste0(BASE, url)
  }
  
  path <- tempfile(fileext = paste0(".", ext))
  
  utils::download.file(
    url = url,
    destfile = path,
    mode = "wb",
    method = if (.Platform$OS.type == "windows") "wininet" else "libcurl"
  )
  
  path
}

.copy_with_ext <- function(src, ext) {
  dst <- tempfile(fileext = paste0(".", ext))
  ok <- file.copy(src, dst, overwrite = TRUE)
  if (!ok) stop(sprintf("Failed to copy temp file as .%s", ext), call. = FALSE)
  dst
}

.try_excel_sheets <- function(path, ext) {
  p <- .copy_with_ext(path, ext)
  on.exit(unlink(p), add = TRUE)
  tryCatch(
    readxl::excel_sheets(p),
    error = function(e) NULL
  )
}

.try_read_excel_df <- function(path, ext, sheet_name = 1) {
  p <- .copy_with_ext(path, ext)
  on.exit(unlink(p), add = TRUE)
  tryCatch(
    as.data.frame(
      readxl::read_excel(p, sheet = sheet_name, col_names = FALSE),
      stringsAsFactors = FALSE
    ),
    error = function(e) NULL
  )
}

.try_load_workbook <- function(path, ext) {
  p <- .copy_with_ext(path, ext)
  on.exit(unlink(p), add = TRUE)
  tryCatch(
    openxlsx::loadWorkbook(p),
    error = function(e) NULL
  )
}

.try_read_csv_df <- function(path) {
  tryCatch(
    as.data.frame(.read_csv_auto(path), stringsAsFactors = FALSE),
    error = function(e) NULL
  )
}

TUIKExtractor <- R6::R6Class(
  "TUIKExtractor",
  public = list(
    lang = NULL,
    .tree = NULL,
    .flat = NULL,
    .handle = NULL,
    
    initialize = function(lang = "tr") {
      .require_packages()
      self$lang <- lang
      self$.handle <- httr::handle(BASE)
      invisible(self)
    },
    
    .load_tree = function() {
      if (!is.null(self$.tree)) return(invisible(NULL))
      
      httr::GET(
        url = paste0(BASE, "/tr/statistical-themes"),
        httr::add_headers(
          Accept = "text/html",
          `sec-fetch-dest` = "document",
          `sec-fetch-mode` = "navigate",
          `sec-fetch-site` = "none"
        ),
        handle = self$.handle,
        httr::timeout(20)
      )
      
      resp <- httr::GET(
        url = TREE_URL(self$lang),
        httr::add_headers(.headers = .default_headers),
        handle = self$.handle,
        httr::timeout(30)
      )
      httr::stop_for_status(resp)
      data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
      self$.tree <- data[["data"]]
      self$.flat <- .flatten_tree(self$.tree, path = character(), result = list())
      invisible(NULL)
    },
    
    themes = function() {
      self$.load_tree()
      rows <- lapply(self$.tree, function(n) data.frame(
        id = .as_char(n[["id"]]),
        name = .as_char(n[["name"]]),
        n_children = length(n[["children"]] %||% list()),
        path = .as_char(n[["_path"]] %||% n[["name"]] %||% ""),
        full_path = .as_char(n[["_path"]] %||% n[["name"]] %||% ""),
        stringsAsFactors = FALSE
      ))
      do.call(rbind, rows)
    },
    
    categories = function() self$themes(),
    
    children = function(row) {
      self$.load_tree()
      row_id <- if (is.data.frame(row)) row$id[[1]] else row[["id"]]
      node <- .find_node(self$.tree, row_id)
      if (is.null(node)) stop(sprintf("Node id '%s' not found in tree.", row_id), call. = FALSE)
      kids <- node[["children"]] %||% list()
      if (length(kids) == 0) return(data.frame())
      do.call(rbind, lapply(kids, .node_to_row))
    },
    
    descendants = function(row, include_self = FALSE, files_only = FALSE, folders_only = FALSE, max_depth = NULL) {
      self$.load_tree()
      row_id <- if (is.data.frame(row)) row$id[[1]] else row[["id"]]
      node <- .find_node(self$.tree, row_id)
      if (is.null(node)) stop(sprintf("Node id '%s' not found in tree.", row_id), call. = FALSE)
      
      rows <- list()
      visit <- function(n, depth) {
        if (!is.null(max_depth) && depth > max_depth) return(invisible(NULL))
        if (depth > 0 || include_self) {
          icon <- .as_char(n[["icon"]])
          keep <- TRUE
          if (files_only) keep <- identical(icon, "istab")
          if (folders_only) keep <- identical(icon, "folder")
          if (keep) {
            r <- .node_to_row(n)
            r$depth <- depth
            r$path <- .as_char(n[["_path"]])
            r$full_path <- .as_char(n[["_path"]])
            rows[[length(rows) + 1L]] <<- r
          }
        }
        for (child in n[["children"]] %||% list()) visit(child, depth + 1L)
      }
      
      visit(node, 0L)
      if (length(rows) == 0) data.frame() else do.call(rbind, rows)
    },
    
    folder_contents = function(row = NULL, recursive = FALSE, files_only = FALSE, folders_only = FALSE, max_depth = NULL) {
      self$.load_tree()
      if (is.null(row)) {
        df <- do.call(rbind, lapply(self$.tree, function(n) {
          r <- .node_to_row(n)
          r$depth <- 0L
          r$path <- .as_char(n[["_path"]] %||% n[["name"]] %||% "")
          r$full_path <- r$path
          r
        }))
      } else if (recursive) {
        df <- self$descendants(row, include_self = FALSE, files_only = files_only, folders_only = folders_only, max_depth = max_depth)
        return(df)
      } else {
        df <- self$children(row)
      }
      
      if (nrow(df) == 0) return(df)
      if (files_only) df <- df[df$icon == "istab", , drop = FALSE]
      if (folders_only) df <- df[df$icon == "folder", , drop = FALSE]
      rownames(df) <- NULL
      df
    },
    
    search = function(keyword, files_only = FALSE, within = NULL) {
      self$.load_tree()
      kw <- .norm(keyword)
      pool <- self$.flat
      
      if (!is.null(within)) {
        node_id <- if (is.data.frame(within)) within$id[[1]] else within[["id"]]
        node <- .find_node(self$.tree, node_id)
        if (is.null(node)) stop(sprintf("Node id '%s' not found in tree.", node_id), call. = FALSE)
        allowed_prefix <- .as_char(node[["_path"]] %||% node[["name"]] %||% "")
        pool <- Filter(function(n) startsWith(.as_char(n[["_path"]]), allowed_prefix), pool)
      }
      
      hits <- Filter(function(n) grepl(kw, .norm(.as_char(n[["name"]])), fixed = TRUE), pool)
      if (files_only) hits <- Filter(function(n) identical(.as_char(n[["icon"]]), "istab"), hits)
      if (length(hits) == 0) return(data.frame())
      
      rows <- lapply(hits, function(n) {
        r <- .node_to_row(n)
        r$path <- .as_char(n[["_path"]])
        r$full_path <- r$path
        r
      })
      do.call(rbind, rows)
    },
    
    search_files = function(keyword, within = NULL) {
      self$search(keyword, files_only = TRUE, within = within)
    },
    
    download_to_temp_win = function(file_row, ext = "bin") {
      url <- .as_char(file_row$url)
      if (!nzchar(url)) stop("file_row$url is empty.", call. = FALSE)
      
      if (!grepl("^http", url)) {
        url <- paste0(BASE, url)
      }
      
      path <- tempfile(fileext = paste0(".", ext))
      
      utils::download.file(
        url = url,
        destfile = path,
        mode = "wb",
        method = if (.Platform$OS.type == "windows") "wininet" else "libcurl"
      )
      
      path
    },
    
    download_to_temp = function(file_row) {
      self$download_to_temp_win(file_row)
    },
    
    download_bytes = function(file_row) {
      url <- .as_char(file_row$url)
      if (!nzchar(url)) stop("file_row$url is empty.", call. = FALSE)
      if (!grepl("^http", url)) url <- paste0(BASE, url)
      
      try(
        httr::GET(
          paste0(BASE, "/tr/statistical-themes"),
          httr::add_headers(
            "Accept" = "text/html",
            "User-Agent" = "Mozilla/5.0",
            "Referer" = "https://veriportali.tuik.gov.tr/tr/statistical-themes"
          ),
          handle = self$.handle,
          httr::timeout(20)
        ),
        silent = TRUE
      )
      
      resp <- httr::GET(
        url,
        httr::add_headers(
          "User-Agent" = "Mozilla/5.0",
          "Referer" = "https://veriportali.tuik.gov.tr/tr/statistical-themes"
        ),
        handle = self$.handle,
        httr::timeout(60)
      )
      
      httr::stop_for_status(resp)
      
      list(
        content = httr::content(resp, as = "raw"),
        content_type = httr::headers(resp)[["content-type"]] %||% ""
      )
    },
    
    .detect_excel_format = function(content, content_type = "") {
      n <- min(200L, length(content))
      head_raw <- content[seq_len(n)]
      
      ct <- tolower(content_type %||% "")
      
      if (length(content) >= 4 &&
          identical(as.integer(content[1:4]), c(0x50, 0x4B, 0x03, 0x04))) {
        return("xlsx")
      }
      
      if (length(content) >= 4 &&
          identical(as.integer(content[1:4]), c(0xD0, 0xCF, 0x11, 0xE0))) {
        return("xls")
      }
      
      text_head <- tryCatch(
        rawToChar(head_raw, multiple = FALSE),
        error = function(e) ""
      )
      text_head_lc <- tolower(text_head)
      
      if (grepl("<html", text_head_lc, fixed = TRUE) ||
          grepl("<!doctype", text_head_lc, fixed = TRUE)) {
        return("html")
      }
      
      if (grepl("csv", ct, fixed = TRUE)) {
        return("csv")
      }
      
      if (grepl("excel", ct, fixed = TRUE) ||
          grepl("spreadsheet", ct, fixed = TRUE)) {
        return("excel")
      }
      
      return("unknown")
    },
    
    .write_temp_file = function(content, ext) {
      path <- tempfile(fileext = paste0(".", ext))
      con <- file(path, "wb")
      on.exit(close(con), add = TRUE)
      writeBin(content, con)
      path
    },
    
    workbook_sheets = function(file_row) {
      path <- self$download_to_temp(file_row)
      on.exit(unlink(path), add = TRUE)
      
      sheets_xlsx <- .try_excel_sheets(path, "xlsx")
      if (!is.null(sheets_xlsx)) return(sheets_xlsx)
      
      sheets_xls <- .try_excel_sheets(path, "xls")
      if (!is.null(sheets_xls)) return(sheets_xls)
      
      csv_df <- .try_read_csv_df(path)
      if (!is.null(csv_df)) return("Sheet1")
      
      stop("Downloaded file could not be opened as XLSX, XLS, or CSV.", call. = FALSE)
    },
    
    sheet_df = function(file_row, sheet_name = 1) {
      path <- self$download_to_temp(file_row)
      on.exit(unlink(path), add = TRUE)
      
      out_xlsx <- .try_read_excel_df(path, "xlsx", sheet_name = sheet_name)
      if (!is.null(out_xlsx)) return(out_xlsx)
      
      out_xls <- .try_read_excel_df(path, "xls", sheet_name = sheet_name)
      if (!is.null(out_xls)) return(out_xls)
      
      out_csv <- .try_read_csv_df(path)
      if (!is.null(out_csv)) return(out_csv)
      
      stop("Downloaded file could not be read as XLSX, XLS, or CSV.", call. = FALSE)
    },
    
    download = function(file_row, sheet_name = 1, ...) {
      invisible(list(...))
      self$sheet_df(file_row, sheet_name = sheet_name)
    },
    
    workbook = function(file_row, data_only = TRUE) {
      invisible(data_only)
      
      path <- self$download_to_temp(file_row)
      on.exit(unlink(path), add = TRUE)
      
      wb_xlsx <- .try_load_workbook(path, "xlsx")
      if (!is.null(wb_xlsx)) return(wb_xlsx)
      
      wb_xls <- .try_load_workbook(path, "xls")
      if (!is.null(wb_xls)) {
        stop("File appears to be XLS. openxlsx cannot reliably open XLS files. Use workbook_sheets(), sheet_df(), find_series(), or extract_time_series() instead.", call. = FALSE)
      }
      
      csv_df <- .try_read_csv_df(path)
      if (!is.null(csv_df)) {
        stop("File appears to be CSV, not an Excel workbook. Use sheet_df() instead.", call. = FALSE)
      }
      
      stop("Downloaded file could not be opened as XLSX/XLS/CSV.", call. = FALSE)
    },
    
    find_series = function(file_row, keyword, sheet_name = NULL, look_in_formulas = FALSE) {
      invisible(look_in_formulas)
      target <- .norm(keyword)
      sheets <- if (is.null(sheet_name)) self$workbook_sheets(file_row) else sheet_name
      matches <- list()
      
      for (ws_name in sheets) {
        df <- self$sheet_df(file_row, ws_name)
        if (nrow(df) == 0 || ncol(df) == 0) next
        for (r in seq_len(nrow(df))) {
          for (c in seq_len(ncol(df))) {
            value <- df[r, c][[1]]
            if (is.null(value) || length(value) == 0 || is.na(value)) next
            if (grepl(target, .norm(as.character(value)), fixed = TRUE)) {
              matches[[length(matches) + 1L]] <- data.frame(
                sheet = ws_name,
                cell = .excel_cell(r, c),
                row = r,
                column = c,
                value = as.character(value),
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
      
      if (length(matches) == 0) data.frame() else do.call(rbind, matches)
    },
    
    extract_series_table = function(file_row, keyword, sheet_name = NULL, pad_top = 1, pad_bottom = 200, pad_left = 0, pad_right = 20, drop_empty_rows = TRUE, drop_empty_cols = TRUE) {
      matches <- self$find_series(file_row, keyword, sheet_name = sheet_name)
      if (nrow(matches) == 0) stop(sprintf("No cell containing '%s' was found.", keyword), call. = FALSE)
      best <- matches[1, , drop = FALSE]
      raw <- self$sheet_df(file_row, best$sheet[[1]])
      
      r0 <- max(best$row[[1]] - pad_top, 1L)
      r1 <- min(best$row[[1]] + pad_bottom, nrow(raw))
      c0 <- max(best$column[[1]] - pad_left, 1L)
      c1 <- min(best$column[[1]] + pad_right, ncol(raw))
      
      out <- raw[r0:r1, c0:c1, drop = FALSE]
      names(out) <- as.character(seq_len(ncol(out)) - 1L)
      
      if (drop_empty_rows) {
        keep_rows <- rowSums(!is.na(out) & trimws(as.character(as.matrix(out))) != "") > 0
        out <- out[keep_rows, , drop = FALSE]
      }
      if (drop_empty_cols) {
        keep_cols <- vapply(out, function(col) any(!is.na(col) & trimws(as.character(col)) != ""), logical(1))
        out <- out[, keep_cols, drop = FALSE]
      }
      rownames(out) <- NULL
      out
    },
    
    extract_time_series = function(file_row, keyword, date_col_keywords = NULL, value_col_keywords = NULL, sheet_name = NULL, search_window_rows = 300, search_window_cols = 20) {
      if (is.null(date_col_keywords)) date_col_keywords <- c("tarih", "date", "ay", "month", "yıl", "year", "dönem", "period")
      if (is.null(value_col_keywords)) value_col_keywords <- c(keyword)
      
      block <- self$extract_series_table(
        file_row = file_row,
        keyword = keyword,
        sheet_name = sheet_name,
        pad_top = 3,
        pad_bottom = search_window_rows,
        pad_left = 2,
        pad_right = search_window_cols
      )
      
      cleaned <- .promote_header(as.data.frame(block, stringsAsFactors = FALSE))
      date_col <- .find_best_column(cleaned, date_col_keywords)
      value_col <- .find_best_column(cleaned, value_col_keywords, fallback_numeric = TRUE)
      
      if (is.null(date_col) || is.null(value_col)) {
        stop("Could not infer date/value columns automatically. Use find_series() and extract_series_table() first.", call. = FALSE)
      }
      
      ts <- cleaned[, c(date_col, value_col), drop = FALSE]
      names(ts) <- c("date", "value")
      ts$date <- trimws(as.character(ts$date))
      val_chr <- gsub("\\.", "", as.character(ts$value))
      val_chr <- gsub(",", ".", val_chr, fixed = TRUE)
      ts$value <- suppressWarnings(as.numeric(val_chr))
      ts <- ts[!is.na(ts$value) & nzchar(ts$date), , drop = FALSE]
      rownames(ts) <- NULL
      ts
    },
    
    extract_series_from_starts_consecutive_dates = function(file_row, sheet_name, start_cells, series_names = NULL, year_col = "A", month_col = "B", stop_at_all_blank_rows = TRUE, max_rows = NULL) {
      df <- self$sheet_df(file_row, sheet_name)
      extract_series_from_starts_consecutive_dates(
        df = df,
        start_cells = start_cells,
        series_names = series_names,
        year_col = year_col,
        month_col = month_col,
        stop_at_all_blank_rows = stop_at_all_blank_rows,
        max_rows = max_rows
      )
    },
    
    extract_series_long_consecutive_dates = function(file_row, sheet_name, start_cells, series_names = NULL, year_col = "A", month_col = "B", stop_at_all_blank_rows = TRUE, max_rows = NULL) {
      df <- self$sheet_df(file_row, sheet_name)
      extract_series_long_consecutive_dates(
        df = df,
        start_cells = start_cells,
        series_names = series_names,
        year_col = year_col,
        month_col = month_col,
        stop_at_all_blank_rows = stop_at_all_blank_rows,
        max_rows = max_rows
      )
    }
  )
)
