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

# A zip can hold more than one shapefile: the NLSC bundle ships the island-wide
# township layer "TOWN_MOI_<date>.shp" alongside partial extras such as
# "Town_Majia_Sanhe.shp" (both start with "town", so a name prefix isn't enough).
# Prefer the official "TOWN_MOI_*" layer; if that's absent, fall back to whichever
# layer has the most features (the full township layer has ~368, far more than a
# partial extra), and only then to the first file alphabetically.
.tww_pick_township_shp <- function(shp) {
  if (length(shp) == 1L) return(shp)
  base <- tolower(basename(shp))
  moi <- shp[grepl("^town_moi", base)]
  if (length(moi)) return(moi[1])
  counts <- vapply(shp, function(s) {
    n <- tryCatch(sum(sf::st_layers(s)$features, na.rm = TRUE),
                  error = function(e) NA_real_)
    if (is.na(n)) -1 else n
  }, numeric(1))
  if (any(counts >= 0)) shp[which.max(counts)] else shp[1]
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
  # "township"/"county" are listed first so re-standardising an already
  # standardised layer (e.g. the sf returned by load_tw_townships()) is
  # idempotent instead of failing to find a name column.
  tf <- pick(town_field,
             c("township", "townname", "town", "t_name", "townshipname",
               "name", "t_uname"))
  cf <- pick(county_field,
             c("county", "countyname", "c_name", "county_name", "cn"))
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
  # `stations` may already carry `township`/`county` columns (we just seeded
  # `township`, and get_stations() supplies `county`). Rename the boundary
  # attributes so st_join doesn't suffix the clashing names to .x/.y, which
  # would leave joined$township/$county NULL.
  geom <- attr(boundaries, "sf_column")
  bnd <- boundaries[, c("township", "county", geom)]
  names(bnd)[match(c("township", "county"), names(bnd))] <-
    c(".bnd_township", ".bnd_county")
  suppressWarnings(sf::st_agr(bnd) <- "constant")
  joined <- sf::st_join(pts, bnd, join = sf::st_within, left = TRUE)
  # st_join can duplicate a point if polygons overlap; keep the first match
  joined <- joined[!duplicated(joined$station_id), , drop = FALSE]

  idx <- match(stations$station_id[ok], joined$station_id)
  stations$township[ok]   <- joined$.bnd_township[idx]
  stations$county_geo[ok]  <- joined$.bnd_county[idx]
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
#' Townships are identified by **county + township together** (`county` +
#' `townships`), because district names are not unique across Taiwan — e.g.
#' 中山區 exists in both 臺北市 and 基隆市, and 西區 in 臺中市, 嘉義市 and
#' 臺南市. The traditional/simplified 臺/台 forms are treated as equal.
#'
#' When a township has **no valid value** for a given time step and variable —
#' either because no station falls inside its polygon, or because every
#' in-township station reports `NA` there — the value is filled from the
#' `k_nearest` stations closest to that township's centroid (the "question two"
#' fallback). This guarantees one row per requested township per time step,
#' even for districts that contain no station of their own.
#'
#' @param start,end,type Passed to [get_weather()].
#' @param boundaries Township boundaries: an \pkg{sf} object from
#'   [load_tw_townships()], or a path/URL it can read. Required for the
#'   coordinate-based lookup.
#' @param county Single county/city name the `townships` belong to
#'   (e.g. `"臺中市"`). `NULL` matches `townships` on district name alone across
#'   every county (kept for convenience, but ambiguous for non-unique names).
#' @param townships Optional character vector of district names to keep
#'   (e.g. `c("北屯區", "西屯區")`). `NULL` aggregates every township that has
#'   at least one station.
#' @param k_nearest Number of nearest stations (by distance to the township
#'   centroid) used to fill time steps/variables that have no valid in-township
#'   value. Default `3`.
#' @param stations Optional pre-fetched station table (from [get_stations()]).
#'   If `NULL`, it is downloaded.
#' @param rain_pattern Regex identifying rainfall columns to sum. Default
#'   matches the feed's `降水`/`雨量`/`Precipitation`/`rain` labels.
#' @param agg_fun Named list overriding the statistic for specific columns,
#'   e.g. `list("日照時數(hour)" = sum)`.
#' @param clean,na_codes,quiet Passed to [get_weather()].
#'
#' @return A data frame with `county`, `township`, `obs_time`, one column per
#'   aggregated variable, `n_stations` (number of in-township stations
#'   contributing to that row) and `used_fallback` (`TRUE` when at least one
#'   variable in the row was filled from the nearest-station pool). The ids of
#'   the in-township stations are kept in the `stations` attribute.
#'
#' @examples
#' \dontrun{
#' bnd <- load_tw_townships()                      # or a local shapefile
#' tw  <- get_township_weather(
#'   start = "2024-01-01", end = "2024-01-07", type = "daily",
#'   boundaries = bnd, county = "臺中市",
#'   townships = c("北屯區", "西屯區")
#' )
#' }
#' @export
get_township_weather <- function(start, end,
                                 type = c("hourly", "daily", "monthly"),
                                 boundaries,
                                 county = NULL,
                                 townships = NULL,
                                 k_nearest = 3,
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
  # standardise once so we can both reverse-geocode and read region centroids
  if (!inherits(boundaries, "sf")) {
    boundaries <- load_tw_townships(source = boundaries)
  } else {
    boundaries <- .tww_standardise_boundaries(boundaries, NULL, NULL)
  }

  if (is.null(stations)) stations <- get_stations()
  stations <- assign_township(stations, boundaries)

  # which (county, township) regions must we return one row per time step for?
  regions <- .tww_target_regions(boundaries, stations, county, townships)
  if (nrow(regions) == 0L) {
    stop("No matching township(s) for the requested county/townships.",
         call. = FALSE)
  }
  # attach in-township station ids and the nearest-k fallback pool per region
  regions <- .tww_attach_station_pools(regions, boundaries, stations, k_nearest)

  # download once for every station we might use (in-township + fallback pools)
  need_ids <- unique(unlist(c(regions$in_ids, regions$near_ids)))
  need_ids <- need_ids[!is.na(need_ids) & nzchar(need_ids)]
  if (length(need_ids) == 0L) {
    stop("No stations available for the requested township(s).", call. = FALSE)
  }
  obs <- get_weather(need_ids, start, end, type = type,
                     clean = clean, na_codes = na_codes, quiet = quiet)

  out <- .tww_aggregate_regions(obs, regions, rain_pattern, agg_fun)

  used <- unique(unlist(regions$in_ids))
  used <- used[!is.na(used) & nzchar(used)]
  attr(out, "stations") <-
    stats::setNames(used, stations$name[match(used, stations$station_id)])
  attr(out, "type") <- type
  out
}

# Normalise a Chinese place name for matching: fold 台 -> 臺 and trim. This lets
# users type either form (台中市 / 臺中市) and still hit the official boundary
# names, which use 臺.
.tww_norm_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- NA_character_
  trimws(gsub("台", "臺", x))   # 台 -> 臺
}

# Resolve the set of (county, township) regions to return, using the canonical
# display names from the boundary layer. With `townships = NULL` we take every
# (county, township) that has at least one assigned station (optionally limited
# to `county`); otherwise we look each requested district up in the boundaries.
.tww_target_regions <- function(boundaries, stations, county, townships) {
  bnd <- data.frame(
    county_n      = .tww_norm_name(boundaries$county),
    township_n    = .tww_norm_name(boundaries$township),
    county_disp   = as.character(boundaries$county),
    township_disp = as.character(boundaries$township),
    stringsAsFactors = FALSE
  )
  bnd <- unique(bnd[!is.na(bnd$township_n) & nzchar(bnd$township_n), ,
                    drop = FALSE])
  cty_n <- if (!is.null(county)) .tww_norm_name(county)[1] else NULL

  if (is.null(townships)) {
    df <- data.frame(county_n   = .tww_norm_name(stations$county_geo),
                     township_n = .tww_norm_name(stations$township),
                     stringsAsFactors = FALSE)
    df <- df[!is.na(df$township_n) & nzchar(df$township_n), , drop = FALSE]
    if (!is.null(cty_n)) df <- df[df$county_n %in% cty_n, , drop = FALSE]
    df <- unique(df)
    if (nrow(df) == 0L) {
      return(data.frame(county = character(0), township = character(0),
                        stringsAsFactors = FALSE))
    }
    m <- match(paste(df$county_n, df$township_n, sep = "\r"),
               paste(bnd$county_n, bnd$township_n, sep = "\r"))
    return(unique(data.frame(
      county   = ifelse(is.na(m), df$county_n,   bnd$county_disp[m]),
      township = ifelse(is.na(m), df$township_n, bnd$township_disp[m]),
      stringsAsFactors = FALSE)))
  }

  tship_n <- .tww_norm_name(townships)
  parts <- lapply(tship_n, function(t) {
    hit <- bnd[bnd$township_n == t, , drop = FALSE]
    if (!is.null(cty_n)) hit <- hit[hit$county_n == cty_n, , drop = FALSE]
    if (nrow(hit) == 0L) {
      warning("Township not found in boundaries: ",
              if (!is.null(county)) paste0(county, " "), t, call. = FALSE)
      return(NULL)
    }
    unique(data.frame(county = hit$county_disp, township = hit$township_disp,
                      stringsAsFactors = FALSE))
  })
  out <- do.call(rbind, parts)
  if (is.null(out)) {
    return(data.frame(county = character(0), township = character(0),
                      stringsAsFactors = FALSE))
  }
  unique(out)
}

# For each region attach: `in_ids` (stations whose assigned county+township
# match the region) and `near_ids` (the k stations nearest the region's
# centroid, used as the missing-value fallback).
.tww_attach_station_pools <- function(regions, boundaries, stations, k) {
  k <- max(1L, as.integer(k))
  has_xy <- !is.na(stations$lon) & !is.na(stations$lat)
  spts <- if (any(has_xy)) {
    sf::st_as_sf(stations[has_xy, , drop = FALSE],
                 coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  } else {
    NULL
  }

  bc <- .tww_norm_name(boundaries$county)
  bt <- .tww_norm_name(boundaries$township)
  st_cty  <- .tww_norm_name(stations$county_geo)
  st_town <- .tww_norm_name(stations$township)

  in_ids   <- vector("list", nrow(regions))
  near_ids <- vector("list", nrow(regions))
  for (i in seq_len(nrow(regions))) {
    rc <- .tww_norm_name(regions$county[i])
    rt <- .tww_norm_name(regions$township[i])
    cty_free <- is.na(rc) || !nzchar(rc)

    sel_in <- !is.na(st_town) & st_town == rt &
      (cty_free | (!is.na(st_cty) & st_cty == rc))
    in_ids[[i]] <- stations$station_id[sel_in]

    near_ids[[i]] <- character(0)
    sel_b <- !is.na(bt) & bt == rt & (cty_free | (!is.na(bc) & bc == rc))
    if (any(sel_b) && !is.null(spts)) {
      poly <- suppressWarnings(
        sf::st_union(sf::st_geometry(boundaries[sel_b, , drop = FALSE])))
      cen  <- suppressWarnings(sf::st_centroid(poly))
      d    <- suppressWarnings(as.numeric(sf::st_distance(cen, spts)))
      near_ids[[i]] <- spts$station_id[utils::head(order(d), k)]
    }
  }
  regions$in_ids   <- in_ids
  regions$near_ids <- near_ids
  regions
}

# Aggregate observations to one row per region per time step. Within a cell
# (region x obs_time x variable) the in-township stations are reduced first
# (rain summed, the rest averaged); if that yields no valid value the nearest-k
# pool is used instead and `used_fallback` is flagged.
.tww_aggregate_regions <- function(obs, regions, rain_pattern, agg_fun) {
  meta_cols  <- c("station_id", "obs_time")
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
  reduce_cell <- function(rows, col) {
    if (length(rows) == 0L) return(NA_real_)
    r <- suppressWarnings(fun_for(col)(obs[[col]][rows], na.rm = TRUE))
    if (length(r) != 1L || is.na(r) || is.nan(r) || is.infinite(r)) {
      NA_real_
    } else {
      r
    }
  }

  times <- unique(obs$obs_time)
  rows  <- list()
  for (i in seq_len(nrow(regions))) {
    in_ids   <- regions$in_ids[[i]]
    near_ids <- regions$near_ids[[i]]
    for (tv in times) {
      at_t <- !is.na(obs$obs_time) & obs$obs_time == tv
      in_rows   <- which(at_t & obs$station_id %in% in_ids)
      near_rows <- which(at_t & obs$station_id %in% near_ids)

      vals <- vector("list", length(num_cols)); names(vals) <- num_cols
      fb <- FALSE
      for (col in num_cols) {
        v <- reduce_cell(in_rows, col)
        if (is.na(v)) {
          v <- reduce_cell(near_rows, col)
          if (!is.na(v)) fb <- TRUE
        }
        vals[[col]] <- v
      }
      cols <- c(
        list(county = regions$county[i], township = regions$township[i],
             obs_time = tv),
        vals,
        list(n_stations = length(unique(obs$station_id[in_rows])),
             used_fallback = fb))
      rows[[length(rows) + 1L]] <-
        do.call(data.frame,
                c(cols, list(check.names = FALSE, stringsAsFactors = FALSE)))
    }
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$county, out$township, out$obs_time), , drop = FALSE]
  rownames(out) <- NULL
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
