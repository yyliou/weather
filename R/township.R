#' Default township boundary source
#'
#' The official "鄉鎮市區界線(TWD97經緯度)" shapefile published by the NLSC
#' (內政部國土測繪中心) on data.gov.tw (dataset 7441). It ships as a zipped
#' shapefile with `TOWNNAME` / `COUNTYNAME` attribute columns.
#'
#' The download URL embeds a release date (e.g. `...1140318.zip`) and is updated
#' from time to time. If it 404s, grab the current SHP link from
#' <https://data.gov.tw/dataset/7441> and pass it to [load_tw_townships()].
#' @keywords internal
.TWW_TOWNSHIP_SHP <- paste0(
  "https://www.tgos.tw/tgos/VirtualDir/Product/",
  "3fe61d4a-ca23-4f45-8aca-4a536f40f290/",
  "%E9%84%89%28%E9%8E%AE%E3%80%81%E5%B8%82%E3%80%81%E5%8D%80%29",
  "%E7%95%8C%E7%B7%9A1140318.zip"
)

#' Load Taiwan township boundary polygons
#'
#' Reads a township (鄉鎮市區) boundary layer as an \pkg{sf} object and
#' normalises its town/county name columns. Any source readable by
#' [sf::st_read()] works: a local shapefile/GeoPackage/GeoJSON path, a URL, or a
#' zipped shapefile (local `.zip` path or URL) — zips are downloaded, unpacked
#' and the contained `.shp` is read automatically.
#'
#' The default points at the official NLSC "鄉鎮市區界線(TWD97經緯度)" shapefile
#' on data.gov.tw (dataset 7441). Because that download URL embeds a release date
#' and changes occasionally, you can always fetch the current SHP link from
#' <https://data.gov.tw/dataset/7441> and pass it (or a local copy) via `source`.
#'
#' @param source Path or URL to a township boundary layer, or a zipped
#'   shapefile. Defaults to the data.gov.tw dataset 7441 SHP.
#' @param town_field,county_field Optional explicit column names holding the
#'   township and county names. If `NULL`, they are auto-detected from common
#'   field names (`TOWNNAME`/`TOWN`/`T_Name`, `COUNTYNAME`/`COUNTY`/`C_Name`).
#'
#' @return An \pkg{sf} data frame with (at least) `township`, `county` and a
#'   geometry column, in EPSG:4326.
#' @export
load_tw_townships <- function(source = NULL,
                              town_field = NULL,
                              county_field = NULL) {
  .tww_need_sf()
  if (is.null(source)) source <- .TWW_TOWNSHIP_SHP
  poly <- tryCatch(
    .tww_read_boundary(source),
    error = function(e) {
      stop("Could not read township boundaries from:\n  ", source,
           "\n  ", conditionMessage(e),
           "\nDownload the current SHP from https://data.gov.tw/dataset/7441 ",
           "and pass its path/URL via `source=`.",
           call. = FALSE)
    }
  )
  .tww_standardise_boundaries(poly, town_field, county_field)
}

# Read any supported source into an sf object. Zipped shapefiles (local or
# remote) are downloaded as needed, unpacked to a temp dir, and the first .shp
# inside is read; everything else is handed straight to sf::st_read().
.tww_read_boundary <- function(source) {
  is_url <- grepl("^(https?|ftp)://", source, ignore.case = TRUE)
  is_zip <- grepl("\\.zip($|\\?)", source, ignore.case = TRUE)
  if (!is_zip) {
    return(sf::st_read(source, quiet = TRUE))
  }
  local_zip <- source
  if (is_url) {
    local_zip <- tempfile(fileext = ".zip")
    old <- options(timeout = max(600, getOption("timeout", 60)))
    on.exit(options(old), add = TRUE)
    utils::download.file(source, local_zip, mode = "wb", quiet = TRUE)
  }
  exdir <- tempfile("tww_shp_")
  dir.create(exdir)

  # The official NLSC zip bundles a Big5/CP950-named .xlsx ("清冊") alongside the
  # shapefile. R's internal unzip writes entry names as raw bytes, so on UTF-8
  # filesystems (macOS APFS, modern Linux) creating that file fails with
  # "Illegal byte sequence", aborting the whole extraction before the .shp is
  # reached. Extract only the shapefile's component files (which carry plain
  # ASCII names) and skip everything else, so the offending entry is never
  # touched. Fall back to a full extraction if no obvious shp parts are listed.
  shp_exts <- "\\.(shp|shx|dbf|prj|cpg|sbn|sbx|qix|fix|aih|ain|atx|qpj)$"
  entries <- tryCatch(utils::unzip(local_zip, list = TRUE)$Name,
                      error = function(e) character(0))
  # Match on raw bytes: the bundled .xlsx has an invalid-in-UTF-8 Big5 name that
  # would otherwise trigger "unable to translate to a wide string" warnings (and
  # an NA in the match vector). ASCII shapefile extensions still match fine.
  hit <- grepl(shp_exts, entries, ignore.case = TRUE, useBytes = TRUE)
  wanted <- entries[!is.na(hit) & hit]
  if (length(wanted)) {
    utils::unzip(local_zip, files = wanted, exdir = exdir)
  } else {
    utils::unzip(local_zip, exdir = exdir)
  }

  shp <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE,
                    recursive = TRUE)
  if (length(shp) == 0L) {
    stop("No .shp file found inside the zip.", call. = FALSE)
  }
  sf::st_read(.tww_pick_township_shp(shp), quiet = TRUE)
}

# A zip can hold more than one shapefile (the NLSC bundle ships the island-wide
# township layer "TOWN_MOI_*.shp" plus per-area extras such as "majia.shp").
# Prefer the layer whose name starts with "town"; otherwise fall back to the
# first one alphabetically.
.tww_pick_township_shp <- function(shp) {
  if (length(shp) == 1L) return(shp)
  base <- tolower(basename(shp))
  town <- shp[grepl("^town", base)]
  if (length(town)) return(town[1])
  shp[1]
}

.tww_standardise_boundaries <- function(poly, town_field, county_field) {
  nm <- names(poly)
  low <- tolower(nm)
  pick <- function(explicit, cands) {
    if (!is.null(explicit)) {
      if (!explicit %in% nm) stop("Field '", explicit, "' not in boundary layer.",
                                  call. = FALSE)
      return(explicit)
    }
    for (c in cands) {
      hit <- which(low == c)
      if (length(hit)) return(nm[hit[1]])
    }
    NA_character_
  }
  tf <- pick(town_field,
             c("townname", "town", "t_name", "townshipname", "name", "t_uname"))
  cf <- pick(county_field,
             c("countyname", "county", "c_name", "county_name", "cn"))
  if (is.na(tf)) {
    stop("Could not find a township-name column in the boundary layer. ",
         "Columns present: ", paste(nm, collapse = ", "),
         ". Pass `town_field=`.", call. = FALSE)
  }
  poly$township <- as.character(poly[[tf]])
  poly$county   <- if (!is.na(cf)) as.character(poly[[cf]]) else NA_character_

  # everything in lon/lat for joining with station points
  if (is.na(sf::st_crs(poly))) {
    sf::st_crs(poly) <- 4326
  } else {
    poly <- sf::st_transform(poly, 4326)
  }
  poly[, c("township", "county", attr(poly, "sf_column"))]
}

#' Assign each station to a township by its coordinates
#'
#' Reverse-geocodes stations: turns each station's longitude/latitude into a
#' point and finds the township polygon that contains it.
#'
#' @param stations A data frame with `station_id`, `lon`, `lat` columns
#'   (e.g. from [get_stations()]).
#' @param boundaries An \pkg{sf} township layer from [load_tw_townships()], or a
#'   path/URL accepted by it.
#'
#' @return `stations` with added `township` and `county` columns. Stations that
#'   fall outside every polygon get `NA`.
#' @export
assign_township <- function(stations, boundaries) {
  .tww_need_sf()
  req <- c("station_id", "lon", "lat")
  if (!all(req %in% names(stations))) {
    stop("`stations` must have columns: ", paste(req, collapse = ", "),
         call. = FALSE)
  }
  if (!inherits(boundaries, "sf")) {
    boundaries <- load_tw_townships(source = boundaries)
  } else {
    boundaries <- .tww_standardise_boundaries(boundaries, NULL, NULL)
  }

  ok <- !is.na(stations$lon) & !is.na(stations$lat)
  stations$township <- NA_character_
  stations$county_geo <- NA_character_
  if (!any(ok)) return(stations)

  pts <- sf::st_as_sf(stations[ok, , drop = FALSE],
                      coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  suppressWarnings(sf::st_agr(boundaries) <- "constant")
  joined <- sf::st_join(pts, boundaries, join = sf::st_within, left = TRUE)
  # st_join can duplicate a point if polygons overlap; keep the first match
  joined <- joined[!duplicated(joined$station_id), , drop = FALSE]

  idx <- match(stations$station_id[ok], joined$station_id)
  stations$township[ok]  <- joined$township[idx]
  stations$county_geo[ok] <- joined$county[idx]
  stations
}

#' Aggregate station observations up to township level
#'
#' The end-to-end helper for question three: it figures out which stations sit
#' in which township, downloads their observations, and aggregates the stations
#' within each township for every time step. Rainfall columns are **summed**;
#' all other numeric variables are **averaged** (this default matches common
#' meteorological practice and your stated preference).
#'
#' @param start,end,type Passed to [get_weather()].
#' @param boundaries Township boundaries: an \pkg{sf} object from
#'   [load_tw_townships()], or a path/URL it can read. Required for the
#'   coordinate-based lookup.
#' @param townships Optional character vector of township names to keep
#'   (e.g. `c("北屯區", "信義區")`). `NULL` aggregates every township that has
#'   at least one station.
#' @param stations Optional pre-fetched station table (from [get_stations()]).
#'   If `NULL`, it is downloaded.
#' @param rain_pattern Regex identifying rainfall columns to sum. Default
#'   matches the feed's `降水`/`雨量`/`Precipitation`/`rain` labels.
#' @param agg_fun Named list overriding the statistic for specific columns,
#'   e.g. `list("日照時數(hour)" = sum)`.
#' @param clean,na_codes,quiet Passed to [get_weather()].
#'
#' @return A data frame with `county`, `township`, `obs_time`, one column per
#'   aggregated variable, and `n_stations` (number of stations contributing to
#'   each township/time row). The ids of the contributing stations are kept in
#'   the `stations` attribute.
#'
#' @examples
#' \dontrun{
#' bnd <- load_tw_townships()                      # or a local shapefile
#' tw  <- get_township_weather(
#'   start = "2024-01-01", end = "2024-01-07", type = "daily",
#'   boundaries = bnd, townships = c("北屯區", "西屯區")
#' )
#' }
#' @export
get_township_weather <- function(start, end,
                                 type = c("hourly", "daily", "monthly"),
                                 boundaries,
                                 townships = NULL,
                                 stations = NULL,
                                 rain_pattern = "降水|雨量|precip|rain",
                                 agg_fun = list(),
                                 clean = TRUE,
                                 na_codes = .tww_default_na_codes(),
                                 quiet = TRUE) {
  type <- match.arg(type)
  .tww_need_sf()
  if (missing(boundaries) || is.null(boundaries)) {
    stop("`boundaries` is required to reverse-geocode stations to townships. ",
         "Use load_tw_townships() or pass a shapefile/GeoJSON path.",
         call. = FALSE)
  }

  if (is.null(stations)) stations <- get_stations()
  stations <- assign_township(stations, boundaries)

  # keep stations that landed in a township (optionally only requested ones)
  keep <- !is.na(stations$township)
  if (!is.null(townships)) keep <- keep & stations$township %in% townships
  stations <- stations[keep, , drop = FALSE]
  if (nrow(stations) == 0L) {
    stop("No stations fall inside the requested township(s).", call. = FALSE)
  }

  obs <- get_weather(stations$station_id, start, end, type = type,
                     clean = clean, na_codes = na_codes, quiet = quiet)

  # attach township / county to every observation row
  m <- match(obs$station_id, stations$station_id)
  obs$township <- stations$township[m]
  obs$county   <- stations$county_geo[m]
  # fall back to metadata county where geo-county is missing
  if (all(is.na(obs$county)) && "county" %in% names(stations)) {
    obs$county <- stations$county[m]
  }
  if (is.null(obs$county)) obs$county <- NA_character_
  obs$county[is.na(obs$county)] <- ""

  out <- .tww_aggregate_township(obs, rain_pattern, agg_fun)
  attr(out, "stations") <- stats::setNames(stations$station_id, stations$name)
  attr(out, "type") <- type
  out
}

# Group by county + township + obs_time and reduce numeric columns.
.tww_aggregate_township <- function(obs, rain_pattern, agg_fun) {
  group_cols <- c("county", "township", "obs_time")
  meta_cols  <- c(group_cols, "station_id")
  value_cols <- setdiff(names(obs), meta_cols)
  num_cols   <- value_cols[vapply(obs[value_cols], is.numeric, logical(1))]
  if (length(num_cols) == 0L) {
    stop("No numeric value columns to aggregate.", call. = FALSE)
  }

  is_rain <- grepl(rain_pattern, num_cols, ignore.case = TRUE)
  fun_for <- function(col) {
    if (!is.null(agg_fun[[col]])) return(match.fun(agg_fun[[col]]))
    if (is_rain[match(col, num_cols)]) sum else mean
  }

  key <- do.call(paste, c(obs[group_cols], sep = "\r"))
  idx <- split(seq_len(nrow(obs)), key, drop = TRUE)

  rows <- lapply(idx, function(ii) {
    sub <- obs[ii, , drop = FALSE]
    vals <- lapply(num_cols, function(col) {
      f <- fun_for(col)
      v <- sub[[col]]
      r <- suppressWarnings(f(v, na.rm = TRUE))
      if (is.nan(r) || is.infinite(r)) NA_real_ else r
    })
    names(vals) <- num_cols
    cols <- c(
      list(
        county   = sub$county[1],
        township = sub$township[1],
        obs_time = sub$obs_time[1]
      ),
      vals,
      list(n_stations = length(unique(sub$station_id)))
    )
    do.call(data.frame, c(cols,
                          list(check.names = FALSE, stringsAsFactors = FALSE)))
  })

  out <- do.call(rbind, rows)
  out <- out[order(out$county, out$township, out$obs_time), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.tww_need_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for township operations. ",
         "Install it with install.packages('sf').", call. = FALSE)
  }
}
