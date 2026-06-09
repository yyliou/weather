#' Download CWA weather station metadata
#'
#' Fetches the CODiS station list that powers the station picker on
#' <https://codis.cwa.gov.tw/StationData> and returns it as a tidy data frame.
#' This is the companion lookup table for [get_weather()]: it gives you each
#' station's id, Chinese name and longitude/latitude, which is also what
#' [get_region_weather()] interpolates from.
#'
#' The CODiS feed groups stations by attribute (`cwb` 局屬氣象站, `agr`
#' 農業氣象站, automatic and rainfall stations, ...). This function flattens all
#' groups into one table and keeps the attribute in an `attribute` column. A
#' station is treated as currently operating when it has no decommission date
#' (`stationEndDate`); `active_only = TRUE` keeps only those.
#'
#' @param url Source URL for the station JSON. Defaults to the CODiS
#'   `station_list` endpoint. You can pass another compatible feed (e.g. the old
#'   CWA `STMap.json` via `.tww_stmap_url()`); the normaliser auto-detects the
#'   common id / name / lon / lat columns either way.
#' @param active_only Logical. If `TRUE` (default), only currently operating
#'   stations (those without a decommission date / active flag) are returned.
#' @param raw Logical. If `TRUE`, return the raw flattened table with the
#'   provider's original column names (including `log`, `extend.mainPic`, ...)
#'   instead of the normalised table.
#'
#' @return A data frame. With `raw = FALSE` (default) it contains at least
#'   `station_id`, `name`, `lon`, `lat`, and, when available, `name_en`,
#'   `altitude`, `county`, `town`, `address`, `area`, `attribute`,
#'   `start_date`, `end_date`, `remark` and `active`. Provider noise columns
#'   (e.g. `log`, `extend.mainPic`) are dropped unless `raw = TRUE`.
#'
#' @examples
#' \dontrun{
#' st <- get_stations()
#' head(st[, c("station_id", "name", "county", "lon", "lat")])
#'
#' # include decommissioned stations too
#' all_st <- get_stations(active_only = FALSE)
#' }
#' @export
get_stations <- function(url = NULL,
                         active_only = TRUE,
                         raw = FALSE) {
  if (is.null(url)) url <- .tww_station_list_url()

  # Remote URLs are downloaded with a browser User-Agent first (the CODiS host
  # rejects R's default agent); local paths are parsed directly. Either way we
  # hand `jsonlite` a file, never the raw URL.
  src <- if (grepl("^https?://", url)) {
    path <- .tww_fetch_to_tempfile(url, fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    path
  } else {
    url
  }

  dat <- tryCatch(
    jsonlite::fromJSON(src, simplifyVector = TRUE,
                       simplifyDataFrame = TRUE, flatten = FALSE),
    error = function(e) {
      stop("Could not parse station list JSON from:\n  ", url,
           "\n  ", conditionMessage(e), call. = FALSE)
    }
  )

  # Surface an explicit API error envelope (code != 200) instead of failing
  # later with a confusing "no id/lat/lon columns" message.
  if (is.list(dat) && !is.data.frame(dat) &&
      !is.null(dat[["code"]]) && !identical(as.integer(dat[["code"]][1]), 200L)) {
    stop("Station list endpoint returned an error (code ", dat[["code"]][1],
         if (!is.null(dat[["message"]])) paste0(": ", dat[["message"]][1]),
         ")\n  ", url, call. = FALSE)
  }

  # CODiS wraps the payload in an envelope: list(code, message, metadata, data).
  # Unwrap to the `data` element when present; otherwise use what we got.
  payload <- if (is.list(dat) && !is.data.frame(dat) && !is.null(dat[["data"]])) {
    dat[["data"]]
  } else {
    dat
  }

  st <- .tww_flatten_station_list(payload)

  if (!is.data.frame(st) || nrow(st) == 0L) {
    stop("Station list endpoint returned no rows:\n  ", url, call. = FALSE)
  }
  if (isTRUE(raw)) return(st)

  .tww_normalise_stations(st, active_only = active_only)
}

# The CODiS `data` payload is a data frame with one row per station *group*:
# a scalar `stationAttribute` and an `item` list-column whose elements are the
# per-group station data frames. Flatten them into a single long table, tagging
# every row with its attribute. A plain (already-flat) feed is passed through.
.tww_flatten_station_list <- function(payload) {
  one_group <- function(df, attr) {
    if (is.null(df)) return(NULL)
    if (!is.data.frame(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
    if (nrow(df) == 0L) return(NULL)
    df[["stationAttribute"]] <- if (length(attr)) attr else NA_character_
    df
  }

  # Shape A: a data frame with a scalar `stationAttribute` column and an
  # `item` list-column (jsonlite's usual simplification).
  if (is.data.frame(payload) && "item" %in% names(payload)) {
    items <- payload[["item"]]
    attrs <- if ("stationAttribute" %in% names(payload)) {
      payload[["stationAttribute"]]
    } else {
      rep(NA_character_, length(items))
    }
    parts <- lapply(seq_along(items),
                    function(i) one_group(items[[i]], attrs[[i]]))
    return(.tww_rbind_fill(parts))
  }

  # Shape B: a plain list of group objects, each list(stationAttribute, item).
  if (is.list(payload) && !is.data.frame(payload) &&
      length(payload) && all(vapply(payload,
        function(g) is.list(g) && !is.null(g[["item"]]), logical(1)))) {
    parts <- lapply(payload,
                    function(g) one_group(g[["item"]], g[["stationAttribute"]]))
    return(.tww_rbind_fill(parts))
  }

  # Otherwise assume it is already a flat station table.
  if (is.data.frame(payload)) return(payload)
  as.data.frame(payload, stringsAsFactors = FALSE)
}

# Map the provider's columns onto a stable, documented schema. Column names in
# CWA/CODiS feeds vary, so detection is case-insensitive against candidate sets.
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

  col_id    <- pick(c("stationid", "id", "station_id", "stno", "stid", "stationidc"))
  col_name  <- pick(c("stationname", "name", "locationname", "cname", "station_name"))
  col_nmeng <- pick(c("stationnameen", "name_en", "ename", "englishname"))
  col_lat   <- pick(c("latitude", "lat", "stationlatitude", "y"))
  col_lon   <- pick(c("longitude", "lon", "lng", "stationlongitude", "x"))
  col_alt   <- pick(c("altitude", "alt", "height", "elevation", "stationaltitude"))
  col_cty   <- pick(c("countyname", "county", "countryname", "city"))
  col_town  <- pick(c("townname", "town", "township", "districtname"))
  col_addr  <- pick(c("address", "location", "addr"))
  col_area  <- pick(c("area"))
  col_attr  <- pick(c("stationattribute", "attribute", "stationtype", "type"))
  col_sdate <- pick(c("stationstartdate", "startdate", "start_date", "datastartdate"))
  col_edate <- pick(c("stationenddate", "enddate", "end_date"))
  col_act   <- pick(c("active", "status", "statusfg", "state", "switch"))
  col_rmk   <- pick(c("webremark", "remark", "note"))

  if (is.na(col_id) || is.na(col_lat) || is.na(col_lon)) {
    stop("Could not locate id / latitude / longitude columns in the station ",
         "feed. Re-run with `raw = TRUE` to inspect the original columns: ",
         paste(nm, collapse = ", "), call. = FALSE)
  }

  num <- function(x) suppressWarnings(as.numeric(x))
  # blank strings -> NA, so empty stationEndDate becomes a clean missing value
  chr <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(trimws(x))] <- NA_character_
    x
  }

  out <- data.frame(
    station_id = as.character(dat[[col_id]]),
    name       = if (!is.na(col_name)) as.character(dat[[col_name]]) else NA_character_,
    lon        = num(dat[[col_lon]]),
    lat        = num(dat[[col_lat]]),
    stringsAsFactors = FALSE
  )
  if (!is.na(col_nmeng)) out$name_en  <- chr(dat[[col_nmeng]])
  if (!is.na(col_alt))  out$altitude  <- num(dat[[col_alt]])
  if (!is.na(col_cty))  out$county    <- chr(dat[[col_cty]])
  if (!is.na(col_town)) out$town      <- chr(dat[[col_town]])
  if (!is.na(col_addr)) out$address   <- chr(dat[[col_addr]])
  if (!is.na(col_area)) out$area      <- chr(dat[[col_area]])
  if (!is.na(col_attr)) out$attribute <- as.character(dat[[col_attr]])
  if (!is.na(col_sdate)) {
    out$start_date <- suppressWarnings(as.Date(chr(dat[[col_sdate]])))
  }
  end_chr <- NULL
  if (!is.na(col_edate)) {
    end_chr <- chr(dat[[col_edate]])
    out$end_date <- suppressWarnings(as.Date(end_chr))
  }
  if (!is.na(col_rmk)) out$remark <- chr(dat[[col_rmk]])

  # Active flag: prefer an explicit status column; otherwise a station is
  # "active" when it carries no decommission (end) date.
  active <- NULL
  if (!is.na(col_act)) {
    raw_act <- dat[[col_act]]
    active <- if (is.logical(raw_act)) {
      raw_act
    } else {
      a <- tolower(as.character(raw_act))
      !(a %in% c("0", "false", "off", "n", "no", "撤站", "已撤銷", "stop"))
    }
  } else if (!is.null(end_chr)) {
    active <- is.na(end_chr)
  }
  if (!is.null(active)) out$active <- active

  if (isTRUE(active_only) && !is.null(active)) {
    out <- out[!is.na(out$active) & out$active, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}
