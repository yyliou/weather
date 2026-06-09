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
#'   geometry column, in EPSG:4326. When the source carries a township-code
#'   field (`TOWNID` / `TOWNCODE`), it is kept as `townid` — the key
#'   [get_township_weather()] selects and aggregates on.
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
  # The official township code get_township_weather() selects and aggregates on.
  # On the NLSC layer two code columns coexist: TOWNCODE is the 8-digit
  # administrative code (e.g. "66000040" = 臺中市北屯區) while TOWNID is a short
  # internal id (e.g. "T07"). The docs, examples and the `townid=` filter all use
  # the 8-digit code, so prefer TOWNCODE and only fall back to TOWNID when no
  # 8-digit code column exists.
  idf <- pick(NULL,
              c("towncode", "town_code", "towncode_", "townid", "town_id",
                "t_id", "code"))
  if (is.na(tf)) {
    stop("Could not find a township-name column in the boundary layer. ",
         "Columns present: ", paste(nm, collapse = ", "),
         ". Pass `town_field=`.", call. = FALSE)
  }
  poly$township <- as.character(poly[[tf]])
  poly$county   <- if (!is.na(cf)) as.character(poly[[cf]]) else NA_character_
  if (!is.na(idf)) poly$townid <- as.character(poly[[idf]])

  # everything in lon/lat for joining with station points
  if (is.na(sf::st_crs(poly))) {
    sf::st_crs(poly) <- 4326
  } else {
    poly <- sf::st_transform(poly, 4326)
  }
  keep <- c("township", "county", if (!is.na(idf)) "townid",
            attr(poly, "sf_column"))
  poly[, keep]
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
#' @return `stations` with added `township`, `county_geo` and (when the boundary
#'   layer carries a township code) `townid` columns. Stations that fall outside
#'   every polygon get `NA`.
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

  has_townid <- "townid" %in% names(boundaries)

  ok <- !is.na(stations$lon) & !is.na(stations$lat)
  stations$township   <- NA_character_
  stations$county_geo <- NA_character_
  if (has_townid) stations$townid <- NA_character_
  if (!any(ok)) return(stations)

  pts <- sf::st_as_sf(stations[ok, , drop = FALSE],
                      coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  # `stations` may already carry `township`/`county` columns (we just seeded
  # `township`, and get_stations() supplies `county`). Rename the boundary
  # attributes so st_join doesn't suffix the clashing names to .x/.y, which
  # would leave joined$township/$county NULL.
  geom <- attr(boundaries, "sf_column")
  src_cols  <- c("township", "county", if (has_townid) "townid")
  dest_cols <- c(".bnd_township", ".bnd_county", if (has_townid) ".bnd_townid")
  bnd <- boundaries[, c(src_cols, geom)]
  names(bnd)[match(src_cols, names(bnd))] <- dest_cols
  suppressWarnings(sf::st_agr(bnd) <- "constant")
  joined <- sf::st_join(pts, bnd, join = sf::st_within, left = TRUE)
  # st_join can duplicate a point if polygons overlap; keep the first match
  joined <- joined[!duplicated(joined$station_id), , drop = FALSE]

  idx <- match(stations$station_id[ok], joined$station_id)
  stations$township[ok]   <- joined$.bnd_township[idx]
  stations$county_geo[ok]  <- joined$.bnd_county[idx]
  if (has_townid) stations$townid[ok] <- joined$.bnd_townid[idx]
  stations
}

#' Aggregate station observations up to township level
#'
#' The end-to-end township helper: it figures out which stations sit in which
#' township, downloads their observations, and aggregates the stations within
#' each township for every time step. Rainfall columns are **summed**; all other
#' numeric variables are **averaged**.
#'
#' Townships are identified by their official township code, `townid` (the
#' `TOWNID` / `TOWNCODE` field on the NLSC layer). It is a single, unique key, so
#' it sidesteps the fact that district *names* repeat across Taiwan (中山區 is in
#' both 臺北市 and 基隆市). The boundary layer you pass via `boundaries` must
#' therefore carry a township-code column; [load_tw_townships()] keeps it
#' automatically.
#'
#' When a township has **no valid value** for a given time step and variable —
#' either because no station falls inside its polygon, or because every
#' in-township station reports `NA` there — the value is filled from the nearest
#' stations to that township's centroid that **actually report a non-`NA` value**
#' for that variable at that time. The `k_nearest` closest such stations are
#' averaged (rainfall summed); stations that are `NA` there are skipped over, so
#' an all-`NA` township is not left empty. This guarantees one complete,
#' balanced row per requested township per time step — even for districts that
#' contain no station of their own, or whose own stations never report the
#' requested variable (e.g. rain-only gauges).
#'
#' @param start,end,type Passed to [get_weather()].
#' @param boundaries Township boundaries: an \pkg{sf} object from
#'   [load_tw_townships()], or a path/URL it can read. Must include a township
#'   code column (`TOWNID` / `TOWNCODE`, kept as `townid`).
#' @param townid Optional character vector of township codes to keep
#'   (e.g. `c("66000040", "66000050")`). `NULL` (default) aggregates every
#'   township that has at least one station. Replaces the old
#'   `county` / `townships` name-based selection.
#' @param k_nearest Number of nearest stations **with a valid value** to average
#'   (rain summed) when filling a time step/variable that has no in-township
#'   value. Default `10`.
#' @param pool_size Number of nearest stations (by distance to the township
#'   centroid) whose observations are downloaded to search for those non-`NA`
#'   values. Must be `>= k_nearest`; the larger it is, the more likely every
#'   cell can be filled. `NULL` (default) uses `max(30, 3 * k_nearest)`.
#' @param stations Optional pre-fetched station table (from [get_stations()]).
#'   If `NULL`, it is downloaded.
#' @param rain_pattern Regex identifying rainfall columns to sum. Default
#'   matches the feed's `降水`/`雨量`/`Precipitation`/`rain` labels.
#' @param agg_fun Named list overriding the statistic for specific columns,
#'   e.g. `list("日照時數(hour)" = sum)`.
#' @param clean,na_codes,quiet Passed to [get_weather()].
#'
#' @return A data frame with `townid`, `county`, `township`, `obs_time`, one
#'   column per aggregated variable, `n_stations` (number of in-township stations
#'   contributing to that row) and `used_fallback` (`TRUE` when at least one
#'   variable in the row was filled from the nearest-station pool). The ids of
#'   the in-township stations are kept in the `stations` attribute.
#'
#' @examples
#' \dontrun{
#' bnd <- load_tw_townships()                      # or a local shapefile
#' # every township that has a station:
#' tw_all <- get_township_weather(
#'   start = "2024-01-01", end = "2024-01-07", type = "daily", boundaries = bnd
#' )
#' # just a couple of townships, selected by their townid codes:
#' tw <- get_township_weather(
#'   start = "2024-01-01", end = "2024-01-07", type = "daily",
#'   boundaries = bnd, townid = c("66000040", "66000050")
#' )
#' }
#' @export
get_township_weather <- function(start, end,
                                 type = c("hourly", "daily", "monthly"),
                                 boundaries,
                                 townid = NULL,
                                 k_nearest = 10,
                                 pool_size = NULL,
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
  if (!"townid" %in% names(boundaries)) {
    stop("The boundary layer has no township-code column (TOWNID / TOWNCODE). ",
         "get_township_weather() selects and keys on `townid`; pass a layer ",
         "that includes one, or use get_region_weather() with a custom ",
         "`id_field`.", call. = FALSE)
  }

  if (is.null(stations)) stations <- get_stations()
  stations <- assign_township(stations, boundaries)

  # townid -> county / township display labels, carried onto the output.
  lab <- data.frame(
    townid   = as.character(boundaries$townid),
    county   = as.character(boundaries$county),
    township = as.character(boundaries$township),
    stringsAsFactors = FALSE
  )
  lab <- lab[!is.na(lab$townid) & nzchar(lab$townid), , drop = FALSE]
  lab <- lab[!duplicated(lab$townid), , drop = FALSE]

  # which townids must we return one row per time step for?
  all_ids <- if (is.null(townid)) {
    unique(stations$townid[!is.na(stations$townid) & nzchar(stations$townid)])
  } else {
    want <- as.character(townid)
    miss <- setdiff(want, lab$townid)
    if (length(miss)) {
      warning("townid not found in boundaries: ", paste(miss, collapse = ", "),
              call. = FALSE)
    }
    want[want %in% lab$townid]
  }
  all_ids <- all_ids[!is.na(all_ids) & nzchar(all_ids)]
  if (length(all_ids) == 0L) {
    stop("No matching township(s) for the requested `townid`.", call. = FALSE)
  }
  regions <- merge(data.frame(townid = all_ids, stringsAsFactors = FALSE),
                   lab, by = "townid", all.x = TRUE, sort = FALSE)
  regions <- regions[, c("townid", "county", "township"), drop = FALSE]

  # attach in-township station ids and the nearest-station fallback pool per
  # region. The pool is a generous, distance-ordered candidate set; the cell-wise
  # aggregation walks it to find `k_nearest` stations that actually have a value.
  pool <- if (is.null(pool_size)) {
    max(30L, 3L * as.integer(k_nearest))
  } else {
    max(as.integer(pool_size), as.integer(k_nearest))
  }
  regions <- .tww_attach_region_pools(regions, boundaries, stations, pool,
                                      key = "townid")

  # download once for every station we might use (in-township + fallback pools)
  need_ids <- unique(unlist(c(regions$in_ids, regions$near_ids)))
  need_ids <- need_ids[!is.na(need_ids) & nzchar(need_ids)]
  if (length(need_ids) == 0L) {
    stop("No stations available for the requested township(s).", call. = FALSE)
  }
  obs <- get_weather(need_ids, start, end, type = type,
                     clean = clean, na_codes = na_codes, quiet = quiet)

  out <- .tww_aggregate_regions(obs, regions, rain_pattern, agg_fun,
                                k = k_nearest)

  used <- unique(unlist(regions$in_ids))
  used <- used[!is.na(used) & nzchar(used)]
  attr(out, "stations") <-
    stats::setNames(used, stations$name[match(used, stations$station_id)])
  attr(out, "type") <- type
  out
}

#' Aggregate station observations over an arbitrary shapefile
#'
#' A general-purpose sibling of [get_township_weather()]: instead of the
#' official township layer, you supply **your own** boundary polygons (any
#' source [sf::st_read()] can read — a shapefile, GeoPackage, GeoJSON, a zipped
#' shapefile, local or URL — or an already-loaded \pkg{sf} object) and name the
#' column that identifies each region. Stations are reverse-geocoded into your
#' polygons and aggregated per region for every time step, with exactly the same
#' balanced-panel guarantee: rainfall is summed, other variables averaged, and
#' any region/time/variable with no in-region value is filled from the nearest
#' stations that actually report a value there (see [get_township_weather()]).
#'
#' @param start,end,type Passed to [get_weather()].
#' @param shp Your boundary polygons: an \pkg{sf} object, or a path/URL to a
#'   shapefile / GeoPackage / GeoJSON / zipped shapefile.
#' @param id_field Name of the column in `shp` that identifies each region
#'   (e.g. `"VILLNAME"`, `"site"`, `"basin_id"`). Its values become the `region`
#'   column of the output. Polygons sharing an `id_field` value are treated as
#'   one region (their geometries are unioned for the centroid lookup).
#' @param regions Optional character vector of `id_field` values to keep. `NULL`
#'   (default) aggregates every region in `shp`.
#' @param k_nearest Number of nearest stations **with a valid value** to average
#'   (rain summed) when filling a time step/variable that has no in-region
#'   value. Default `10`.
#' @param pool_size Number of nearest stations (by distance to a region's
#'   centroid) whose observations are downloaded to search for those non-`NA`
#'   values. Must be `>= k_nearest`. `NULL` (default) uses
#'   `max(30, 3 * k_nearest)`.
#' @param stations Optional pre-fetched station table (from [get_stations()]).
#'   If `NULL`, it is downloaded.
#' @param rain_pattern,agg_fun,clean,na_codes,quiet As in
#'   [get_township_weather()].
#'
#' @return A data frame with `region`, `obs_time`, one column per aggregated
#'   variable, `n_stations` (in-region stations contributing) and
#'   `used_fallback`. The ids of the in-region stations are kept in the
#'   `stations` attribute.
#'
#' @seealso [get_township_weather()], [load_tw_townships()]
#'
#' @examples
#' \dontrun{
#' # Aggregate to whatever polygons you have, keyed by one column.
#' rw <- get_region_weather(
#'   start = "2024-01-01", end = "2024-01-07", type = "daily",
#'   shp = "my_regions.shp", id_field = "site_name"
#' )
#' }
#' @export
get_region_weather <- function(start, end,
                               type = c("hourly", "daily", "monthly"),
                               shp,
                               id_field,
                               regions = NULL,
                               k_nearest = 10,
                               pool_size = NULL,
                               stations = NULL,
                               rain_pattern = "降水|雨量|precip|rain",
                               agg_fun = list(),
                               clean = TRUE,
                               na_codes = .tww_default_na_codes(),
                               quiet = TRUE) {
  type <- match.arg(type)
  .tww_need_sf()
  if (missing(shp) || is.null(shp)) {
    stop("`shp` is required: pass an sf object or a path/URL to a shapefile.",
         call. = FALSE)
  }
  if (missing(id_field) || is.null(id_field) ||
      !is.character(id_field) || length(id_field) != 1L) {
    stop("`id_field` must be a single column name identifying each region.",
         call. = FALSE)
  }
  boundaries <- .tww_standardise_region(shp, id_field)

  if (is.null(stations)) stations <- get_stations()
  stations <- .tww_assign_region(stations, boundaries)

  all_regions <- unique(boundaries$region)
  all_regions <- all_regions[!is.na(all_regions) & nzchar(all_regions)]
  if (!is.null(regions)) {
    want <- as.character(regions)
    miss <- setdiff(want, all_regions)
    if (length(miss)) {
      warning("Region(s) not found in `shp`: ", paste(miss, collapse = ", "),
              call. = FALSE)
    }
    all_regions <- all_regions[all_regions %in% want]
  }
  if (length(all_regions) == 0L) {
    stop("No matching region(s) in `shp`.", call. = FALSE)
  }
  reg_df <- data.frame(region = all_regions, stringsAsFactors = FALSE)

  pool <- if (is.null(pool_size)) {
    max(30L, 3L * as.integer(k_nearest))
  } else {
    max(as.integer(pool_size), as.integer(k_nearest))
  }
  reg_df <- .tww_attach_region_pools(reg_df, boundaries, stations, pool)

  need_ids <- unique(unlist(c(reg_df$in_ids, reg_df$near_ids)))
  need_ids <- need_ids[!is.na(need_ids) & nzchar(need_ids)]
  if (length(need_ids) == 0L) {
    stop("No stations available for the requested region(s).", call. = FALSE)
  }
  obs <- get_weather(need_ids, start, end, type = type,
                     clean = clean, na_codes = na_codes, quiet = quiet)

  out <- .tww_aggregate_regions(obs, reg_df, rain_pattern, agg_fun,
                                k = k_nearest)

  used <- unique(unlist(reg_df$in_ids))
  used <- used[!is.na(used) & nzchar(used)]
  attr(out, "stations") <-
    stats::setNames(used, stations$name[match(used, stations$station_id)])
  attr(out, "type") <- type
  out
}

# Standardise an arbitrary boundary source into an sf with a `region` column
# (from `id_field`) in EPSG:4326.
.tww_standardise_region <- function(shp, id_field) {
  .tww_need_sf()
  poly <- if (inherits(shp, "sf")) shp else .tww_read_boundary(shp)
  geom <- attr(poly, "sf_column")
  if (!id_field %in% names(poly)) {
    stop("`id_field` '", id_field, "' is not a column in `shp`. ",
         "Columns present: ",
         paste(setdiff(names(poly), geom), collapse = ", "), call. = FALSE)
  }
  poly$region <- as.character(poly[[id_field]])
  if (is.na(sf::st_crs(poly))) {
    sf::st_crs(poly) <- 4326
  } else {
    poly <- sf::st_transform(poly, 4326)
  }
  poly[, c("region", attr(poly, "sf_column"))]
}

# Reverse-geocode stations into the `region` polygons (point-in-polygon).
.tww_assign_region <- function(stations, boundaries) {
  .tww_need_sf()
  req <- c("station_id", "lon", "lat")
  if (!all(req %in% names(stations))) {
    stop("`stations` must have columns: ", paste(req, collapse = ", "),
         call. = FALSE)
  }
  ok <- !is.na(stations$lon) & !is.na(stations$lat)
  stations$region <- NA_character_
  if (!any(ok)) return(stations)

  pts <- sf::st_as_sf(stations[ok, , drop = FALSE],
                      coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  geom <- attr(boundaries, "sf_column")
  bnd <- boundaries[, c("region", geom)]
  names(bnd)[match("region", names(bnd))] <- ".bnd_region"
  suppressWarnings(sf::st_agr(bnd) <- "constant")
  joined <- sf::st_join(pts, bnd, join = sf::st_within, left = TRUE)
  joined <- joined[!duplicated(joined$station_id), , drop = FALSE]

  idx <- match(stations$station_id[ok], joined$station_id)
  stations$region[ok] <- joined$.bnd_region[idx]
  stations
}

# For each region attach `in_ids` (stations whose assigned `key` matches the
# region) and `near_ids` (the `pool_size` stations nearest the region's
# centroid, ordered by distance, used as the missing-value fallback pool). The
# aggregation walks `near_ids` in distance order to find stations that actually
# have a non-NA value for the cell being filled. `key` is the column (in both
# `regions` and `stations`/`boundaries`) that identifies a region: `"townid"`
# for get_township_weather(), `"region"` for get_region_weather().
.tww_attach_region_pools <- function(regions, boundaries, stations, pool_size,
                                     key = "region") {
  pool_size <- max(1L, as.integer(pool_size))
  has_xy <- !is.na(stations$lon) & !is.na(stations$lat)
  spts <- if (any(has_xy)) {
    sf::st_as_sf(stations[has_xy, , drop = FALSE],
                 coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  } else {
    NULL
  }
  br   <- as.character(boundaries[[key]])
  skey <- as.character(stations[[key]])

  in_ids   <- vector("list", nrow(regions))
  near_ids <- vector("list", nrow(regions))
  for (i in seq_len(nrow(regions))) {
    rr <- as.character(regions[[key]][i])
    in_ids[[i]] <- stations$station_id[!is.na(skey) & skey == rr]

    near_ids[[i]] <- character(0)
    sel_b <- !is.na(br) & br == rr
    if (any(sel_b) && !is.null(spts)) {
      poly <- suppressWarnings(
        sf::st_union(sf::st_geometry(boundaries[sel_b, , drop = FALSE])))
      cen  <- suppressWarnings(sf::st_centroid(poly))
      d    <- suppressWarnings(as.numeric(sf::st_distance(cen, spts)))
      near_ids[[i]] <- spts$station_id[utils::head(order(d), pool_size)]
    }
  }
  regions$in_ids   <- in_ids
  regions$near_ids <- near_ids
  regions
}

# Aggregate observations to one row per region per time step. Every column of
# `regions` other than the `in_ids` / `near_ids` list-columns is treated as an
# identifier and copied onto the output (so this serves both the township case,
# keyed on county+township, and the generic case, keyed on a single `region`).
#
# Within a cell (region x obs_time x variable) the in-region stations are reduced
# first (rain summed, the rest averaged). If that yields no valid value, the
# distance-ordered `near_ids` pool is walked, skipping stations that are NA there,
# until up to `k` stations with a real value are collected; those are reduced and
# `used_fallback` is flagged. This is what keeps the panel balanced and free of
# missing values even when a region's own stations report nothing.
.tww_aggregate_regions <- function(obs, regions, rain_pattern, agg_fun, k = 10) {
  id_cols    <- setdiff(names(regions), c("in_ids", "near_ids"))
  meta_cols  <- c("station_id", "obs_time")
  value_cols <- setdiff(names(obs), meta_cols)
  num_cols   <- value_cols[vapply(obs[value_cols], is.numeric, logical(1))]
  if (length(num_cols) == 0L) {
    stop("No numeric value columns to aggregate.", call. = FALSE)
  }
  k <- max(1L, as.integer(k))
  is_rain <- grepl(rain_pattern, num_cols, ignore.case = TRUE)
  fun_for <- function(col) {
    if (!is.null(agg_fun[[col]])) return(match.fun(agg_fun[[col]]))
    if (is_rain[match(col, num_cols)]) sum else mean
  }
  finalize <- function(r) {
    if (length(r) != 1L || is.na(r) || is.nan(r) || is.infinite(r)) {
      NA_real_
    } else {
      r
    }
  }
  # Guard the row assembly: every value handed to data.frame() must be exactly
  # length 1, otherwise data.frame() aborts with "arguments imply differing
  # number of rows" (a length-0 cell -> "1, 0"). reduce_cell()/reduce_fallback()
  # already aim for scalars, but a degenerate column or an unmatched id label can
  # still surface an empty value; coerce anything non-scalar to a single value.
  scalar1 <- function(x) {
    if (is.null(x) || length(x) == 0L) return(NA)
    if (length(x) > 1L) return(x[[1L]])
    x
  }
  # Reduce all in-region rows for a column.
  reduce_cell <- function(rows, col) {
    if (length(rows) == 0L) return(NA_real_)
    finalize(suppressWarnings(fun_for(col)(obs[[col]][rows], na.rm = TRUE)))
  }
  # Walk the distance-ordered fallback pool, taking one non-NA value per station
  # until `k` stations contribute, then reduce them.
  reduce_fallback <- function(ordered_ids, at_t, col) {
    if (length(ordered_ids) == 0L) return(NA_real_)
    vals <- numeric(0)
    for (sid in ordered_ids) {
      rows <- which(at_t & obs$station_id == sid)
      if (length(rows) == 0L) next
      v <- obs[[col]][rows]
      v <- v[!is.na(v) & !is.nan(v) & is.finite(v)]
      if (length(v) == 0L) next
      vals <- c(vals, v[1L])          # this station's value at this time
      if (length(vals) >= k) break
    }
    if (length(vals) == 0L) return(NA_real_)
    finalize(suppressWarnings(fun_for(col)(vals, na.rm = TRUE)))
  }

  times <- unique(obs$obs_time)
  rows  <- list()
  for (i in seq_len(nrow(regions))) {
    in_ids   <- regions$in_ids[[i]]
    near_ids <- regions$near_ids[[i]]
    id_vals  <- stats::setNames(
      lapply(id_cols, function(cc) regions[[cc]][i]), id_cols)
    for (tv in times) {
      at_t    <- !is.na(obs$obs_time) & obs$obs_time == tv
      in_rows <- which(at_t & obs$station_id %in% in_ids)

      vals <- vector("list", length(num_cols)); names(vals) <- num_cols
      fb <- FALSE
      for (col in num_cols) {
        v <- reduce_cell(in_rows, col)
        if (is.na(v)) {
          v <- reduce_fallback(near_ids, at_t, col)
          if (!is.na(v)) fb <- TRUE
        }
        vals[[col]] <- v
      }
      cols <- c(
        lapply(id_vals, scalar1),
        list(obs_time = scalar1(tv)),
        lapply(vals, scalar1),
        list(n_stations = as.integer(length(unique(obs$station_id[in_rows]))),
             used_fallback = isTRUE(fb)))
      rows[[length(rows) + 1L]] <-
        do.call(data.frame,
                c(cols, list(check.names = FALSE, stringsAsFactors = FALSE)))
    }
  }
  out <- do.call(rbind, rows)
  ord <- do.call(order, c(lapply(id_cols, function(cc) out[[cc]]),
                          list(out$obs_time)))
  out <- out[ord, , drop = FALSE]
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
