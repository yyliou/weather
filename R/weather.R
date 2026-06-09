#' Download station weather observations
#'
#' Wraps the NCHU "CWB Historical Weather Data Downloader". You give it one or
#' more station ids and a date range, and it returns the observation time
#' series as a single data frame.
#'
#' Behind the scenes a single station id is served as a CSV, while multiple ids
#' are served as a ZIP of one CSV per station; this function handles both and
#' always returns one combined, long-format table with a `station_id` column.
#'
#' @param station_id Character or numeric vector of CWA station ids, e.g.
#'   `"467490"` or `c("466920", "466930")`.
#' @param start,end Start and end dates (inclusive). Either `Date`/`POSIXt`
#'   objects or strings in `YYYYMMDD` / `YYYY-MM-DD` form. Per the data source,
#'   `end` cannot be later than yesterday; the server truncates it if so.
#' @param type Observation interval: `"hourly"` (default), `"daily"` or
#'   `"monthly"`. Daily and monthly responses contain extra summary columns
#'   (max/min/means). For `"monthly"`, `end` is automatically extended to the
#'   last day of its month, because the source returns a month's record only
#'   once the window reaches the month end (a sub-month window comes back empty).
#' @param clean Logical. If `TRUE` (default), coerce value columns to numeric
#'   and replace known CODiS missing-value sentinels with `NA`.
#' @param na_codes Numeric vector of sentinel values to treat as missing when
#'   `clean = TRUE`.
#' @param quiet Logical, passed to the downloader; `FALSE` shows a progress bar.
#' @param max_ids Maximum number of station ids to request per HTTP call. The
#'   endpoint passes ids in the query string, so asking for hundreds at once
#'   overflows the server's URL limit and fails with "cannot open URL". Larger
#'   id sets are split into chunks of this size and the responses are bound back
#'   together. Default `100`.
#'
#' @return A data frame with a leading `station_id` column, an `obs_time`
#'   column (the original first column of the feed), and one column per measured
#'   variable. Column names keep the provider's original Chinese/English labels.
#'
#' @examples
#' \dontrun{
#' # One station, hourly
#' w <- get_weather("467490", "2024-01-01", "2024-01-07")
#'
#' # Several stations, daily
#' wd <- get_weather(c("466920", "466930"), "2024-01-01", "2024-01-31",
#'                   type = "daily")
#' }
#' @export
get_weather <- function(station_id,
                        start,
                        end,
                        type = c("hourly", "daily", "monthly"),
                        clean = TRUE,
                        na_codes = .tww_default_na_codes(),
                        quiet = TRUE,
                        max_ids = 100L) {
  type <- match.arg(type)
  if (length(station_id) == 0L) {
    stop("`station_id` must contain at least one id.", call. = FALSE)
  }
  ids <- trimws(as.character(station_id))
  ids <- ids[nzchar(ids) & !is.na(ids)]
  if (length(ids) == 0L) {
    stop("`station_id` has no usable ids after trimming.", call. = FALSE)
  }

  start <- .tww_as_yyyymmdd(start, "start")
  end   <- .tww_as_yyyymmdd(end, "end")
  if (end < start) {
    stop("`end` (", end, ") is before `start` (", start, ").", call. = FALSE)
  }

  # The monthly endpoint only emits a month's record once the window reaches the
  # end of that month: a short window like 20240101-20240107 returns an empty
  # body, which silently propagates as all-NA. Extend `end` to its month end.
  if (type == "monthly") {
    me <- .tww_month_end(end)
    if (me > end) {
      if (!isTRUE(quiet)) {
        message("monthly: extended end date ", end, " -> ", me,
                " so the month's record is returned.")
      }
      end <- me
    }
  }

  # The endpoint carries the station ids in the query string, so a request for
  # hundreds of stations (e.g. every township's stations) overflows the server's
  # URL length limit and fails with "cannot open URL". Split large id sets into
  # chunks of `max_ids` and bind the per-chunk responses back together.
  max_ids <- max(1L, as.integer(max_ids))
  chunks  <- split(ids, ceiling(seq_along(ids) / max_ids))

  fetch_chunk <- function(cids) {
    url  <- .tww_build_url(cids, start, end, type)
    path <- .tww_download(url, quiet = quiet)
    on.exit(unlink(path), add = TRUE)
    if (.tww_is_zip(path)) {
      .tww_read_zip(path, cids, na_codes, clean)
    } else {
      df <- .tww_read_csv(path, na_codes, clean)
      if (nrow(df) == 0L) return(data.frame())   # no data in window -> skip
      id <- if (length(cids) == 1L) cids else NA_character_
      cbind(station_id = id, df, stringsAsFactors = FALSE)
    }
  }

  out <- .tww_rbind_fill(lapply(chunks, fetch_chunk))

  # Stations with no data in the requested window (e.g. decommissioned ones) are
  # quietly dropped above rather than aborting the whole request; report which.
  got <- if (nrow(out)) unique(as.character(out$station_id)) else character(0)
  missing_ids <- setdiff(ids, got)
  if (length(missing_ids) && length(ids) > 1L && !isTRUE(quiet)) {
    message("No data in window for station(s), skipped: ",
            paste(missing_ids, collapse = ", "))
  }
  if (nrow(out) == 0L) {
    warning("No data returned for any requested station in this window.",
            call. = FALSE)
  }

  attr(out, "type")  <- type
  # Store the window as Date objects so the format is consistent across the
  # package (station_panel() also carries Date attributes).
  attr(out, "start") <- as.Date(start, format = "%Y%m%d")
  attr(out, "end")   <- as.Date(end, format = "%Y%m%d")
  out
}

# Read a multi-station ZIP into a single long data frame.
.tww_read_zip <- function(path, ids, na_codes, clean) {
  exdir <- tempfile("tww_zip")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)

  files <- utils::unzip(path, exdir = exdir)
  files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  if (length(files) == 0L) {
    stop("ZIP response contained no CSV files.", call. = FALSE)
  }

  parts <- lapply(files, function(f) {
    df <- .tww_read_csv(f, na_codes, clean)
    if (nrow(df) == 0L) return(NULL)   # station had no data in window -> skip
    cbind(station_id = .tww_id_from_name(f), df, stringsAsFactors = FALSE)
  })

  .tww_rbind_fill(parts)
}

# Bind data frames with possibly differing column sets (union of columns).
.tww_rbind_fill <- function(parts) {
  parts <- Filter(function(d) !is.null(d) && nrow(d) > 0L, parts)
  if (length(parts) == 0L) return(data.frame())
  all_cols <- Reduce(union, lapply(parts, names))
  parts <- lapply(parts, function(d) {
    miss <- setdiff(all_cols, names(d))
    for (m in miss) d[[m]] <- NA
    d[all_cols]
  })
  do.call(rbind, parts)
}
