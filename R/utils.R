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
  # The first column is the observation time; rename it to `obs_time`.
  if (ncol(df) >= 1L) {
    names(df)[1] <- "obs_time"
  }
  if (isTRUE(clean)) {
    df <- .tww_clean(df, na_codes)
  }
  df
}

# Convert known CODiS missing-value sentinels to NA in numeric-looking columns.
.tww_clean <- function(df, na_codes) {
  cols <- setdiff(names(df), "obs_time")
  for (nm in cols) {
    v <- df[[nm]]
    if (is.character(v)) {
      vn <- suppressWarnings(as.numeric(v))
      # only coerce when the column is genuinely numeric (ignoring NAs)
      if (all(is.na(vn) == is.na(v))) v <- vn
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
