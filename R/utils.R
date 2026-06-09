# Internal helpers ------------------------------------------------------------

# Base endpoint of the NCHU "CWB Historical Weather Data Downloader".
.tww_base_url <- function() {
  "https://mycolab.pp.nchu.edu.tw/historical_weather/index.php"
}

# Default URL for the CODiS station list. This is the JSON feed that powers the
# station picker on https://codis.cwa.gov.tw/StationData : a single GET returns
# every station (局屬站 / 農業站 / 自動站 / 雨量站 ...) grouped by attribute,
# each with id, Chinese name, lon/lat, altitude, county, set-up / decommission
# dates, etc.
.tww_station_list_url <- function() {
  "https://codis.cwa.gov.tw/api/station_list"
}

# Previous source (CWA detailed station map, STMap.json). Kept so callers can
# still opt into it via `get_stations(url = .tww_stmap_url())`.
.tww_stmap_url <- function() {
  "https://www.cwa.gov.tw/Data/js/Observe/OSM/C/STMap.json"
}

# A browser-like User-Agent. The CODiS / CWA hosts reject requests carrying
# R's default libcurl agent (HTTP 403 / empty body), which is why reading the
# station list straight off the URL with `jsonlite::fromJSON(url)` fails. We
# therefore download with this UA first, then parse from the local file.
.tww_user_agent <- function() {
  paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
         "AppleWebKit/537.36 (KHTML, like Gecko) ",
         "Chrome/124.0.0.0 Safari/537.36")
}

# Download `url` to a temp file (binary-safe) and return the path. Sets a
# browser User-Agent and a generous timeout. Tries the libcurl method with an
# explicit header set first; if that is unavailable it falls back to the
# default method with the UA supplied via `options(HTTPUserAgent=)`.
.tww_fetch_to_tempfile <- function(url, fileext = ".json", quiet = TRUE) {
  tmp     <- tempfile(fileext = fileext)
  old_to  <- getOption("timeout")
  old_ua  <- getOption("HTTPUserAgent")
  on.exit(options(timeout = old_to, HTTPUserAgent = old_ua), add = TRUE)
  options(timeout = max(old_to, 300), HTTPUserAgent = .tww_user_agent())

  do_dl <- function(extra) {
    do.call(utils::download.file,
            c(list(url = url, destfile = tmp, mode = "wb", quiet = quiet),
              extra))
  }

  tryCatch(
    do_dl(list(method  = "libcurl",
               headers = c("User-Agent" = .tww_user_agent(),
                           "Accept" = "application/json, text/plain, */*"))),
    error = function(e1) {
      # libcurl method or `headers=` not available: retry with defaults.
      tryCatch(
        do_dl(list()),
        error = function(e2) {
          stop("Request failed for:\n  ", url, "\n  ",
               conditionMessage(e2), call. = FALSE)
        }
      )
    }
  )

  if (!file.exists(tmp) || file.size(tmp) == 0L) {
    stop("Empty response from server for URL:\n  ", url, call. = FALSE)
  }
  tmp
}

# Normalise a date-ish input into the YYYYMMDD string the API expects.
# Accepts Date, POSIXt, or character/numeric in YYYYMMDD or YYYY-MM-DD form.
.tww_as_yyyymmdd <- function(x, arg = "date") {
  if (length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a single, non-missing date.", arg), call. = FALSE)
  }
  if (inherits(x, c("Date", "POSIXt"))) {
    return(format(as.Date(x), "%Y%m%d"))
  }
  s <- gsub("[^0-9]", "", as.character(x))
  if (!grepl("^[0-9]{8}$", s)) {
    stop(sprintf("`%s` must be a Date or a YYYYMMDD / YYYY-MM-DD string, got '%s'.",
                 arg, x), call. = FALSE)
  }
  # validate it is a real calendar date
  d <- as.Date(s, format = "%Y%m%d")
  if (is.na(d)) {
    stop(sprintf("`%s` is not a valid calendar date: '%s'.", arg, x), call. = FALSE)
  }
  s
}

# Build the full request URL.
.tww_build_url <- function(station_id, start, end, type) {
  type <- match.arg(type, c("hourly", "daily", "monthly"))
  ids <- paste(trimws(as.character(station_id)), collapse = ",")
  url <- sprintf("%s?station_id=%s&startdate=%s&enddate=%s",
                 .tww_base_url(), utils::URLencode(ids), start, end)
  # hourly is the server default; only append type when not hourly so that
  # behaviour matches the website exactly.
  if (type != "hourly") {
    url <- paste0(url, "&type=", type)
  }
  url
}

# Download the raw response (CSV or ZIP) to a temp file and return its path.
.tww_download <- function(url, quiet = TRUE) {
  tmp <- tempfile(fileext = ".bin")
  old <- getOption("timeout")
  on.exit(options(timeout = old), add = TRUE)
  options(timeout = max(old, 300))
  res <- tryCatch(
    utils::download.file(url, destfile = tmp, mode = "wb", quiet = quiet),
    error = function(e) {
      stop("Request failed: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (!file.exists(tmp) || file.size(tmp) == 0L) {
    stop("Empty response from server for URL:\n  ", url, call. = FALSE)
  }
  tmp
}

# Is this file a ZIP archive? (multiple station ids -> zip)
.tww_is_zip <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  magic <- readBin(con, what = "raw", n = 4L)
  length(magic) == 4L &&
    identical(magic[1:2], as.raw(c(0x50, 0x4B)))   # "PK"
}

# Read one CWA CSV file (UTF-8 with BOM) into a data.frame, cleaning sentinels.
.tww_read_csv <- function(path, na_codes, clean) {
  df <- utils::read.csv(
    path,
    header = TRUE,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM",
    na.strings = c("", "NA")
  )
  # The first column is the observation time; rename it to `obs_time` and put it
  # in a consistent ISO (YYYY-MM-DD / YYYY-MM) shape.
  if (ncol(df) >= 1L) {
    names(df)[1] <- "obs_time"
    df$obs_time <- .tww_iso_obs_time(df$obs_time)
  }
  if (isTRUE(clean)) {
    df <- .tww_clean(df, na_codes)
  }
  df
}

# Normalise observation-time strings to ISO. A bare YYYYMMDD becomes
# YYYY-MM-DD and a bare YYYYMM becomes YYYY-MM; values that already carry
# separators (e.g. "2024-01-01 01:00:00") are left untouched. This keeps the
# date format consistent across get_weather(), get_township_weather() and
# get_region_weather().
.tww_iso_obs_time <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  s <- trimws(x)
  d8 <- grepl("^[0-9]{8}$", s)
  if (any(d8)) {
    x[d8] <- format(as.Date(s[d8], format = "%Y%m%d"), "%Y-%m-%d")
  }
  d6 <- grepl("^[0-9]{6}$", s)
  if (any(d6)) {
    x[d6] <- paste0(substr(s[d6], 1L, 4L), "-", substr(s[d6], 5L, 6L))
  }
  x
}

# Convert known CODiS missing-value sentinels to NA in numeric-looking columns.
#
# The feed often writes missing cells as the literal text "NA" (sometimes padded,
# e.g. " NA ", or as other tokens like "X" / "--"). read.csv's `na.strings` only
# catches an exact, untrimmed "NA", so those slip through as character values.
# A column that mixes real numbers with such tokens then fails the
# "is it numeric?" test below and is left as character — which makes it invisible
# to the aggregator (it only sums/averages numeric columns), so whole variables
# come back empty. We therefore blank out NA-like tokens (case- and
# whitespace-insensitive) *before* the numeric coercion, so the column converts
# cleanly to numeric with genuine NAs.
.tww_na_tokens <- function() {
  c("", "na", "n/a", "null", "nan", "none", "nil", "--", "-", "...", ".", "x")
}

.tww_clean <- function(df, na_codes) {
  tokens <- .tww_na_tokens()
  cols <- setdiff(names(df), "obs_time")
  for (nm in cols) {
    v <- df[[nm]]
    if (is.character(v)) {
      vt <- trimws(v)
      vt[tolower(vt) %in% tokens] <- NA_character_
      vn <- suppressWarnings(as.numeric(vt))
      # Coerce to numeric when every non-missing entry parsed as a number
      # (i.e. as.numeric only introduced NAs where the value was already blank).
      parse_fail <- is.na(vn) & !is.na(vt)
      v <- if (!any(parse_fail)) vn else vt
    }
    if (is.numeric(v) && length(na_codes)) {
      v[v %in% na_codes] <- NA
    }
    df[[nm]] <- v
  }
  df
}

# Default CODiS missing-value codes seen in this feed.
.tww_default_na_codes <- function() {
  c(-9991, -9996, -9997, -9998, -9999, -99.8, -99.7)
}

# Pull the station id out of a per-station filename inside the zip, e.g.
# "466920_20190816-20190913.csv" -> "466920".
.tww_id_from_name <- function(fname) {
  base <- basename(fname)
  m <- regmatches(base, regexpr("^[A-Za-z0-9]+", base))
  if (length(m) == 0L || m == "") base else m
}
