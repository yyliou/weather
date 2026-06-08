#' Download CWA weather station metadata
#'
#' Fetches the Central Weather Administration "detailed station info" feed
#' (`STMap.json`) and returns it as a tidy data frame. This is the companion
#' lookup table for [get_weather()]: it gives you each station's id, name and
#' longitude/latitude, which is also what [get_township_weather()] uses to map
#' stations to townships.
#'
#' @param url Source URL for the station JSON. Defaults to the official CWA
#'   `STMap.json`.
#' @param active_only Logical. If `TRUE` (default) and the feed exposes a
#'   status/active flag, only currently operating stations are returned. If the
#'   feed has no such flag this argument is ignored.
#' @param raw Logical. If `TRUE`, return the raw parsed JSON (a data frame with
#'   the provider's original column names) instead of the normalised table.
#'
#' @return A data frame. With `raw = FALSE` (default) it contains at least the
#'   columns `station_id`, `name`, `lon`, `lat`, and, when available,
#'   `altitude`, `county`, `address` and `active`, followed by any remaining
#'   original columns.
#'
#' @examples
#' \dontrun{
#' st <- get_stations()
#' head(st[, c("station_id", "name", "county", "lon", "lat")])
#' }
#' @export
get_stations <- function(url = NULL,
                         active_only = TRUE,
                         raw = FALSE) {
  if (is.null(url)) url <- .tww_stmap_url()

  dat <- tryCatch(
    jsonlite::fromJSON(url, simplifyDataFrame = TRUE),
    error = function(e) {
      stop("Could not read station metadata from:\n  ", url,
           "\n  ", conditionMessage(e), call. = FALSE)
    }
  )

  # The feed is sometimes wrapped (e.g. a named list with one data-frame slot).
  if (is.list(dat) && !is.data.frame(dat)) {
    is_df <- vapply(dat, is.data.frame, logical(1))
    if (any(is_df)) {
      dat <- dat[[which(is_df)[1]]]
    } else {
      dat <- as.data.frame(dat, stringsAsFactors = FALSE)
    }
  }
  if (!is.data.frame(dat) || nrow(dat) == 0L) {
    stop("Station metadata feed returned no rows.", call. = FALSE)
  }
  if (isTRUE(raw)) return(dat)

  .tww_normalise_stations(dat, active_only = active_only)
}

# Map the provider's columns onto a stable, documented schema. Column names in
# CWA feeds have varied over time, so detection is done case-insensitively
# against a set of candidate names.
.tww_normalise_stations <- function(dat, active_only) {
  nm <- names(dat)
  low <- tolower(nm)

  pick <- function(cands) {
    for (c in cands) {
      hit <- which(low == c)
      if (length(hit)) return(nm[hit[1]])
    }
    NA_character_
  }

  col_id   <- pick(c("id", "stationid", "station_id", "stno", "stid", "stationidc"))
  col_name <- pick(c("name", "stationname", "locationname", "cname", "station_name"))
  col_lat  <- pick(c("lat", "latitude", "stationlatitude", "y"))
  col_lon  <- pick(c("lon", "lng", "longitude", "stationlongitude", "x"))
  col_alt  <- pick(c("alt", "altitude", "height", "elevation", "stationaltitude"))
  col_cty  <- pick(c("county", "countyname", "city", "area"))
  col_addr <- pick(c("address", "location", "addr"))
  col_act  <- pick(c("active", "status", "statusfg", "state"))

  if (is.na(col_id) || is.na(col_lat) || is.na(col_lon)) {
    stop("Could not locate id / latitude / longitude columns in the station ",
         "feed. Re-run with `raw = TRUE` to inspect the original columns: ",
         paste(nm, collapse = ", "), call. = FALSE)
  }

  num <- function(x) suppressWarnings(as.numeric(x))

  out <- data.frame(
    station_id = as.character(dat[[col_id]]),
    name       = if (!is.na(col_name)) as.character(dat[[col_name]]) else NA_character_,
    lon        = num(dat[[col_lon]]),
    lat        = num(dat[[col_lat]]),
    stringsAsFactors = FALSE
  )
  if (!is.na(col_alt))  out$altitude <- num(dat[[col_alt]])
  if (!is.na(col_cty))  out$county   <- as.character(dat[[col_cty]])
  if (!is.na(col_addr)) out$address  <- as.character(dat[[col_addr]])

  active <- NULL
  if (!is.na(col_act)) {
    raw_act <- dat[[col_act]]
    active <- if (is.logical(raw_act)) {
      raw_act
    } else {
      # treat common "on/working/現存/1/true" markers as active
      a <- tolower(as.character(raw_act))
      !(a %in% c("0", "false", "off", "n", "no", "撤站", "已撤銷", "stop"))
    }
    out$active <- active
  }

  # append any columns we did not explicitly map, for completeness
  mapped <- c(col_id, col_name, col_lon, col_lat, col_alt, col_cty, col_addr, col_act)
  extra  <- setdiff(nm, mapped[!is.na(mapped)])
  for (e in extra) out[[e]] <- dat[[e]]

  if (isTRUE(active_only) && !is.null(active)) {
    out <- out[!is.na(out$active) & out$active, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}
