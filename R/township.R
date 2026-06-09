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

#' Average weather to township level, interpolating only the gaps
#'
#' The end-to-end township helper, in two stages:
#'
#' \enumerate{
#'   \item **Average** the stations that fall inside each township (rainfall
#'     included: a mean of the in-township gauges) for every time step and
#'     variable. This is the cheap, common case.
#'   \item **Fill** only the cells still missing — a township with no station of
#'     its own, or a variable its own stations never report (e.g. pressure in a
#'     rain-gauge-only district) — by inverse-distance weighting (IDW) of the
#'     `k_nearest` surrounding stations that do report it:
#'     \deqn{v = \frac{\sum_i w_i x_i}{\sum_i w_i}, \qquad w_i = 1 / d_i^{power}}
#'     where \eqn{d_i} is the great-circle distance from the township's
#'     representative point to station \eqn{i}.
#' }
#'
#' Only the in-township stations **plus a nearest pool** (`pool_size`) are
#' downloaded — not every station in the country — so a request for a few
#' townships is fast. The result is still a complete, gap-free panel: one row per
#' township per time step.
#'
#' Townships are identified by their official township code, `townid` (the
#' `TOWNID` / `TOWNCODE` field on the NLSC layer): a single unique key that
#' sidesteps repeated district *names* (中山區 is in both 臺北市 and 基隆市). The
#' `boundaries` layer must therefore carry a township-code column;
#' [load_tw_townships()] keeps it automatically.
#'
#' @param start,end,type Passed to [get_weather()]. For `type = "monthly"` the
#'   end date is automatically extended to the end of its month, since the
#'   source only returns a month's record once the window reaches it.
#' @param boundaries Township boundaries: an \pkg{sf} object from
#'   [load_tw_townships()], or a path/URL it can read. Must include a township
#'   code column (`TOWNID` / `TOWNCODE`, kept as `townid`).
#' @param townid Optional character vector of township codes to keep
#'   (e.g. `c("66000040", "66000050")`). `NULL` (default) returns every township
#'   in `boundaries`.
#' @param power IDW distance exponent used when filling gaps. Higher values give
#'   nearer stations more relative weight. Default `2`.
#' @param k_nearest Number of nearest stations blended when filling a gap cell.
#'   Default `8`.
#' @param max_dist Optional cap (in **kilometres**) on how far a gap-filling
#'   station may be from the township. Stations beyond it are ignored; a cell
#'   with no station within range stays `NA`. `NULL` (default) imposes no cap.
#' @param pool_size Number of nearest stations (per township) downloaded as the
#'   gap-fill pool, on top of the in-township stations. `NULL` (default) uses
#'   `max(30, 3 * k_nearest)`. Larger values fill more reliably but download
#'   more.
#' @param stations Optional pre-fetched station table (from [get_stations()]).
#'   If `NULL`, it is downloaded.
#' @param obs Optional pre-downloaded observations (a [get_weather()] result) to
#'   reuse instead of downloading. When supplied, no network call is made and the
#'   download/`pool_size` step is skipped — average and gap-fill run directly on
#'   `obs`. This is the fast path for repeated runs or many townships: download
#'   once for all stations, save it (e.g. with `saveRDS()`), and pass it back
#'   here. Use the same `stations` table you built `obs` from so coordinates
#'   line up.
#' @param clean,na_codes,quiet Passed to [get_weather()].
#'
#' @return A data frame with `townid`, `county`, `township`, `obs_time`, one
#'   column per variable, `n_stations` (in-township stations feeding the row) and
#'   `used_fallback` (`TRUE` when any cell in the row was filled by IDW rather
#'   than averaged). The IDW power is kept in the `power` attribute and the
#'   in-township station ids in `stations`.
#'
#' @examples
#' \dontrun{
#' bnd <- load_tw_townships()                      # or a local shapefile
#' # every township:
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
                                 power = 2,
                                 k_nearest = 8,
                                 max_dist = NULL,
                                 pool_size = NULL,
                                 stations = NULL,
                                 obs = NULL,
                                 clean = TRUE,
                                 na_codes = .tww_default_na_codes(),
                                 quiet = TRUE) {
  type <- match.arg(type)
  .tww_need_sf()
  if (missing(boundaries) || is.null(boundaries)) {
    stop("`boundaries` is required to locate each township. ",
         "Use load_tw_townships() or pass a shapefile/GeoJSON path.",
         call. = FALSE)
  }
  if (!inherits(boundaries, "sf")) {
    boundaries <- load_tw_townships(source = boundaries)
  } else {
    boundaries <- .tww_standardise_boundaries(boundaries, NULL, NULL)
  }
  if (!"townid" %in% names(boundaries)) {
    stop("The boundary layer has no township-code column (TOWNID / TOWNCODE). ",
         "get_township_weather() keys on `townid`; pass a layer that includes ",
         "one, or use get_region_weather() with a custom `id_field`.",
         call. = FALSE)
  }

  if (is.null(stations)) stations <- get_stations()
  stations <- stations[is.finite(stations$lon) & is.finite(stations$lat), ,
                       drop = FALSE]
  if (nrow(stations) == 0L) {
    stop("No stations with usable coordinates to interpolate from.",
         call. = FALSE)
  }
  # which township each station sits in (drives the in-township average)
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
    lab$townid
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

  # one representative interior point per requested township
  targets <- .tww_region_points(boundaries, key = "townid", ids = all_ids)
  targets <- merge(targets, lab, by = "townid", all.x = TRUE, sort = FALSE)
  targets <- targets[, c("townid", "county", "township", "lon", "lat"),
                     drop = FALSE]

  # stations inside each township feed the in-township average
  in_ids <- lapply(targets$townid, function(id)
    stations$station_id[!is.na(stations$townid) & stations$townid == id])

  if (is.null(obs)) {
    # download only the in-township stations plus a nearest pool to fill gaps
    pool <- if (is.null(pool_size)) {
      max(30L, 3L * as.integer(k_nearest))
    } else {
      max(as.integer(pool_size), as.integer(k_nearest))
    }
    near_ids <- .tww_nearest_ids(targets, stations, pool)
    need_ids <- unique(c(unlist(in_ids), unlist(near_ids)))
    need_ids <- need_ids[!is.na(need_ids) & nzchar(need_ids)]
    if (length(need_ids) == 0L) {
      stop("No stations available for the requested township(s).", call. = FALSE)
    }
    obs <- get_weather(need_ids, start, end, type = type,
                       clean = clean, na_codes = na_codes, quiet = quiet)
  } else {
    obs <- .tww_check_obs(obs)
  }

  out <- .tww_reduce_regions(
    obs, targets, stations,
    id_cols   = c("townid", "county", "township"),
    in_ids    = in_ids,
    power     = power, k_nearest = k_nearest, max_dist = max_dist)

  used <- unique(unlist(in_ids))
  used <- used[!is.na(used) & nzchar(used)]
  attr(out, "stations") <- stats::setNames(
    used, stations$name[match(used, stations$station_id)])
  attr(out, "type")  <- type
  attr(out, "power") <- power
  out
}

#' Interpolate weather over an arbitrary shapefile
#'
#' A general-purpose sibling of [get_township_weather()]: instead of the
#' official township layer, you supply **your own** boundary polygons (any
#' source [sf::st_read()] can read — a shapefile, GeoPackage, GeoJSON, a zipped
#' shapefile, local or URL — or an already-loaded \pkg{sf} object) and name the
#' column that identifies each region. Each region is averaged from the stations
#' inside it, and only the remaining gaps are filled by inverse-distance
#' weighting of nearby stations — exactly as in [get_township_weather()]
#' (rainfall included), downloading only the in-region stations plus a nearest
#' pool.
#'
#' @param start,end,type Passed to [get_weather()]. For `type = "monthly"` the
#'   end date is extended to the end of its month (see [get_township_weather()]).
#' @param shp Your boundary polygons: an \pkg{sf} object, or a path/URL to a
#'   shapefile / GeoPackage / GeoJSON / zipped shapefile.
#' @param id_field Name of the column in `shp` that identifies each region
#'   (e.g. `"VILLNAME"`, `"site"`, `"basin_id"`). Its values become the `region`
#'   column of the output. Polygons sharing an `id_field` value are treated as
#'   one region (their geometries are unioned for the point lookup).
#' @param regions Optional character vector of `id_field` values to keep. `NULL`
#'   (default) returns every region in `shp`.
#' @param power,k_nearest,max_dist,pool_size Averaging / gap-fill settings, as in
#'   [get_township_weather()].
#' @param stations Optional pre-fetched station table (from [get_stations()]).
#'   If `NULL`, it is downloaded.
#' @param obs Optional pre-downloaded observations (a [get_weather()] result) to
#'   reuse instead of downloading, as in [get_township_weather()].
#' @param clean,na_codes,quiet Passed to [get_weather()].
#'
#' @return A data frame with `region`, `obs_time`, one column per variable,
#'   `n_stations` (in-region stations feeding the row) and `used_fallback`
#'   (`TRUE` when any cell was filled by IDW). The power is kept in the `power`
#'   attribute and the in-region station ids in `stations`.
#'
#' @seealso [get_township_weather()], [load_tw_townships()]
#'
#' @examples
#' \dontrun{
#' # Interpolate to whatever polygons you have, keyed by one column.
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
                               power = 2,
                               k_nearest = 8,
                               max_dist = NULL,
                               pool_size = NULL,
                               stations = NULL,
                               obs = NULL,
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
  stations <- stations[is.finite(stations$lon) & is.finite(stations$lat), ,
                       drop = FALSE]
  if (nrow(stations) == 0L) {
    stop("No stations with usable coordinates to interpolate from.",
         call. = FALSE)
  }
  # which region each station sits in (drives the in-region average)
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

  targets <- .tww_region_points(boundaries, key = "region", ids = all_regions)

  # stations inside each region feed the in-region average
  in_ids <- lapply(targets$region, function(id)
    stations$station_id[!is.na(stations$region) & stations$region == id])

  if (is.null(obs)) {
    pool <- if (is.null(pool_size)) {
      max(30L, 3L * as.integer(k_nearest))
    } else {
      max(as.integer(pool_size), as.integer(k_nearest))
    }
    near_ids <- .tww_nearest_ids(targets, stations, pool)
    need_ids <- unique(c(unlist(in_ids), unlist(near_ids)))
    need_ids <- need_ids[!is.na(need_ids) & nzchar(need_ids)]
    if (length(need_ids) == 0L) {
      stop("No stations available for the requested region(s).", call. = FALSE)
    }
    obs <- get_weather(need_ids, start, end, type = type,
                       clean = clean, na_codes = na_codes, quiet = quiet)
  } else {
    obs <- .tww_check_obs(obs)
  }

  out <- .tww_reduce_regions(
    obs, targets, stations,
    id_cols   = "region",
    in_ids    = in_ids,
    power     = power, k_nearest = k_nearest, max_dist = max_dist)

  used <- unique(unlist(in_ids))
  used <- used[!is.na(used) & nzchar(used)]
  attr(out, "stations") <- stats::setNames(
    used, stations$name[match(used, stations$station_id)])
  attr(out, "type")  <- type
  attr(out, "power") <- power
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

# One representative interior point per region. Polygons that share a `key`
# value are unioned first, then reduced to a single point guaranteed to lie
# inside the region (st_point_on_surface), which is the target the surrounding
# stations are interpolated to. Returns a data frame with the `key` column plus
# numeric `lon` / `lat`.
.tww_region_points <- function(boundaries, key, ids = NULL) {
  .tww_need_sf()
  kv <- as.character(boundaries[[key]])
  if (is.null(ids)) {
    ids <- unique(kv[!is.na(kv) & nzchar(kv)])
  } else {
    ids <- as.character(ids)
  }
  lon <- rep(NA_real_, length(ids))
  lat <- rep(NA_real_, length(ids))
  for (i in seq_along(ids)) {
    sel <- !is.na(kv) & kv == ids[i]
    if (!any(sel)) next
    g  <- suppressWarnings(sf::st_union(
      sf::st_geometry(boundaries[sel, , drop = FALSE])))
    pt <- suppressWarnings(sf::st_point_on_surface(g))
    xy <- sf::st_coordinates(pt)
    lon[i] <- xy[1, 1]
    lat[i] <- xy[1, 2]
  }
  out <- data.frame(lon = lon, lat = lat,
                    stringsAsFactors = FALSE, check.names = FALSE)
  out[[key]] <- ids
  out[, c(key, "lon", "lat"), drop = FALSE]
}

# The `pool_size` station ids nearest each target point (great-circle), used as
# the gap-fill pool: only these (plus the in-region stations) are downloaded,
# instead of every station in the country. Returns a list aligned with the rows
# of `targets`.
.tww_nearest_ids <- function(targets, stations, pool_size) {
  pool_size <- max(1L, as.integer(pool_size))
  sid  <- as.character(stations$station_id)
  slon <- stations$lon
  slat <- stations$lat
  lapply(seq_len(nrow(targets)), function(r) {
    d <- .tww_haversine_km(targets$lon[r], targets$lat[r], slon, slat)
    sid[utils::head(order(d), pool_size)]
  })
}

# Reduce station observations to one row per region per time step in two stages:
#  1. AVERAGE the stations that fall inside the region (`in_ids`) - the cheap,
#     common case (rainfall included: a mean of the in-region gauges).
#  2. Only for cells still missing (a region with no station, or a variable its
#     own stations never report) FILL by inverse-distance weighting of the
#     `k_nearest` stations that do report it: v = sum w_i x_i / sum w_i with
#     w_i = 1 / d_i^power, d_i the great-circle km to the region's point. The
#     distance matrix and weights are built lazily, so variables fully covered
#     by in-region stations cost nothing beyond the average.
# Everything is matrix products vectorised over time, touching only the
# downloaded stations, so it stays fast.
#
# `targets`  : data.frame with `id_cols` plus numeric `lon`, `lat`.
# `stations` : data.frame with `station_id`, `lon`, `lat` (the downloaded set).
# `in_ids`   : list (aligned with `targets` rows) of in-region station ids.
.tww_reduce_regions <- function(obs, targets, stations, id_cols, in_ids,
                                power = 2, k_nearest = 8, max_dist = NULL) {
  meta_cols  <- c("station_id", "obs_time")
  value_cols <- setdiff(names(obs), meta_cols)
  num_cols   <- value_cols[vapply(obs[value_cols], is.numeric, logical(1))]
  times <- sort(unique(obs$obs_time[!is.na(obs$obs_time)]))
  nreg  <- nrow(targets)
  nt    <- length(times)

  # Stable, zero-row schema when there is nothing to reduce.
  if (nreg == 0L || nt == 0L || length(num_cols) == 0L) {
    empty <- c(
      stats::setNames(lapply(id_cols, function(.) character(0)), id_cols),
      list(obs_time = character(0)),
      stats::setNames(lapply(num_cols, function(.) numeric(0)), num_cols),
      list(n_stations = integer(0), used_fallback = logical(0)))
    return(do.call(data.frame,
                   c(empty, list(check.names = FALSE, stringsAsFactors = FALSE))))
  }

  # Station universe: stations that appear in obs *and* have coordinates.
  sid_obs <- unique(as.character(obs$station_id))
  smatch  <- match(sid_obs, as.character(stations$station_id))
  keep    <- !is.na(smatch)
  sid     <- sid_obs[keep]
  slon    <- stations$lon[smatch[keep]]
  slat    <- stations$lat[smatch[keep]]
  ns      <- length(sid)
  if (ns == 0L) {
    stop("Could not match any downloaded observation back to a station in the ",
         "station table (so no coordinates are available to reduce). This ",
         "usually means the multi-station download keyed its files in an ",
         "unexpected way. Downloaded ids seen: ",
         paste(utils::head(sid_obs, 5), collapse = ", "),
         if (length(sid_obs) > 5) ", ..." else "",
         call. = FALSE)
  }

  s_idx <- match(as.character(obs$station_id), sid)   # NA for coordless stations
  t_idx <- match(obs$obs_time, times)
  cell  <- (t_idx - 1L) * ns + s_idx                  # column-major into ns x nt

  # Membership matrix A (nreg x ns): 1 where a station is inside the region.
  A <- matrix(0, nreg, ns)
  for (i in seq_len(nreg)) {
    hit <- which(sid %in% as.character(in_ids[[i]]))
    if (length(hit)) A[i, hit] <- 1
  }

  zero <- 1e-9
  kk   <- max(1L, as.integer(k_nearest))
  D    <- NULL                                        # built lazily on first gap
  build_W <- function(cols) {
    if (is.null(D)) {
      D <<- matrix(NA_real_, nreg, ns)
      for (r in seq_len(nreg)) {
        D[r, ] <<- .tww_haversine_km(targets$lon[r], targets$lat[r], slon, slat)
      }
    }
    W <- matrix(0, nreg, length(cols))
    if (!length(cols)) return(W)
    for (r in seq_len(nreg)) {
      d <- D[r, cols]
      if (!is.null(max_dist)) d[d > max_dist] <- Inf
      ord <- order(d)[seq_len(min(kk, length(d)))]
      ord <- ord[is.finite(d[ord])]
      if (!length(ord)) next
      dd <- d[ord]
      if (any(dd <= zero)) {
        W[r, ord[dd <= zero]] <- 1                    # target sits on a station
      } else {
        W[r, ord] <- 1 / dd^power
      }
    }
    W
  }

  # n_stations: in-region stations reporting any value at each time step.
  row_has <- rep(FALSE, nrow(obs))
  for (col in num_cols) row_has <- row_has | is.finite(obs[[col]])
  present_any <- matrix(0, ns, nt)
  ok_any <- !is.na(cell) & row_has
  present_any[cell[ok_any]] <- 1
  n_mat <- A %*% present_any

  out_val <- stats::setNames(vector("list", length(num_cols)), num_cols)
  fb_mat  <- matrix(FALSE, nreg, nt)
  for (col in num_cols) {
    M <- matrix(NA_real_, ns, nt)
    ok <- !is.na(cell) & is.finite(obs[[col]])
    M[cell[ok]] <- obs[[col]][ok]
    present <- matrix(0, ns, nt)
    present[is.finite(M)] <- 1
    Mfill <- M
    Mfill[!is.finite(Mfill)] <- 0

    # stage 1: in-region average (equal weights)
    val <- (A %*% Mfill) / (A %*% present)
    val[!is.finite(val)] <- NA_real_

    # stage 2: fill remaining gaps by IDW (only when there are any)
    gap <- is.na(val)
    if (any(gap)) {
      has_data <- which(rowSums(present) > 0)
      if (length(has_data)) {
        W   <- build_W(has_data)
        idw <- (W %*% Mfill[has_data, , drop = FALSE]) /
               (W %*% present[has_data, , drop = FALSE])
        idw[!is.finite(idw)] <- NA_real_
        fill <- gap & !is.na(idw)
        val[fill] <- idw[fill]
        fb_mat <- fb_mat | fill
      }
    }
    out_val[[col]] <- as.vector(t(val))               # region slow, time fast
  }

  out_id <- lapply(id_cols, function(cc) rep(as.character(targets[[cc]]),
                                             each = nt))
  names(out_id) <- id_cols
  out_time   <- rep(times, times = nreg)
  n_stations <- as.integer(round(as.vector(t(n_mat))))

  out <- do.call(data.frame,
                 c(out_id, list(obs_time = out_time), out_val,
                   list(n_stations = n_stations,
                        used_fallback = as.vector(t(fb_mat))),
                   list(check.names = FALSE, stringsAsFactors = FALSE)))
  ord <- do.call(order, c(lapply(id_cols, function(cc) out[[cc]]),
                          list(out$obs_time)))
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}


# Validate a user-supplied `obs` table (from get_weather()) reused to skip the
# download. It only needs the keys the reducer joins on; value columns are
# whatever was downloaded.
.tww_check_obs <- function(obs) {
  if (!is.data.frame(obs) ||
      !all(c("station_id", "obs_time") %in% names(obs))) {
    stop("`obs` must be a data frame from get_weather() (with `station_id` and ",
         "`obs_time` columns).", call. = FALSE)
  }
  obs
}

.tww_need_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for township operations. ",
         "Install it with install.packages('sf').", call. = FALSE)
  }
}
