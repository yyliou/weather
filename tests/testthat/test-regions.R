# Tests for the county+township resolution and the nearest-station fallback
# (questions 1-3). These exercise the pure helpers and only touch `sf` for the
# centroid-based pool, so they run without any network access.

test_that(".tww_norm_name folds 台 -> 臺 and trims", {
  expect_equal(.tww_norm_name(c(" 台中市 ", "臺北市", NA)),
               c("臺中市", "臺北市", NA_character_))
})

test_that("county + township resolves a non-unique district unambiguously", {
  bnd <- data.frame(
    county   = c("臺中市", "臺中市", "臺北市", "基隆市"),
    township = c("北屯區", "西屯區", "中山區", "中山區"),
    stringsAsFactors = FALSE
  )
  # with a county, only that county's 中山區 comes back
  one <- .tww_target_regions(bnd, stations = NULL,
                             county = "臺北市", townships = "中山區")
  expect_equal(nrow(one), 1L)
  expect_equal(one$county, "臺北市")

  # 台 (simplified) still matches the official 臺 boundary names
  tc <- .tww_target_regions(bnd, stations = NULL,
                            county = "台中市",
                            townships = c("北屯區", "西屯區"))
  expect_equal(sort(tc$township), c("北屯區", "西屯區"))
  expect_true(all(tc$county == "臺中市"))

  # without a county, an ambiguous name expands to every matching county
  both <- .tww_target_regions(bnd, stations = NULL,
                              county = NULL, townships = "中山區")
  expect_equal(nrow(both), 2L)
})

test_that("aggregation falls back to nearest stations per cell", {
  regions <- data.frame(
    county   = c("臺中市", "臺中市"),
    township = c("北屯區", "西屯區"),
    stringsAsFactors = FALSE
  )
  regions$in_ids   <- list(c("A"), character(0))   # 西屯 has no station of its own
  regions$near_ids <- list(c("A", "B"), c("A", "B"))

  obs <- data.frame(
    station_id = c("A", "B", "A", "B"),
    obs_time   = c("d1", "d1", "d2", "d2"),
    `氣溫(℃)`    = c(18, 22, NA, 20),    # A's temp missing on d2
    `降水量(mm)` = c(1, 5, 0, 4),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  out <- .tww_aggregate_regions(obs, regions, "降水|雨量|precip|rain", list())

  # one row per region per day -> the "should be 7 each" case, here 2 x 2 = 4
  expect_equal(nrow(out), 4L)

  bt1 <- out[out$township == "北屯區" & out$obs_time == "d1", ]
  expect_equal(bt1[["氣溫(℃)"]], 18)        # A only, in-township
  expect_false(bt1$used_fallback)

  bt2 <- out[out$township == "北屯區" & out$obs_time == "d2", ]
  expect_equal(bt2[["氣溫(℃)"]], 20)        # A is NA -> nearest pool (B = 20)
  expect_true(bt2$used_fallback)
  expect_equal(bt2[["降水量(mm)"]], 0)      # rain present in-township, no fallback

  xt1 <- out[out$township == "西屯區" & out$obs_time == "d1", ]
  expect_equal(xt1[["氣溫(℃)"]], 20)        # mean(18, 22) from nearest pool
  expect_equal(xt1[["降水量(mm)"]], 6)      # sum(1, 5)
  expect_equal(xt1$n_stations, 0L)
  expect_true(xt1$used_fallback)
})

test_that("nearest-k pool is keyed on the township centroid", {
  testthat::skip_if_not_installed("sf")

  # two unit-square townships side by side: A sits in the left, B in the right
  sq <- function(x0) sf::st_polygon(list(rbind(
    c(x0, 0), c(x0 + 1, 0), c(x0 + 1, 1), c(x0, 1), c(x0, 0))))
  bnd <- sf::st_sf(
    county   = c("臺中市", "臺中市"),
    township = c("左區", "右區"),
    geometry = sf::st_sfc(sq(0), sq(2), crs = 4326)
  )
  stations <- data.frame(
    station_id = c("A", "B"),
    name       = c("a", "b"),
    lon        = c(0.5, 2.5), lat = c(0.5, 0.5),
    county_geo = c("臺中市", "臺中市"),
    township   = c("左區", "右區"),
    stringsAsFactors = FALSE
  )
  regions <- .tww_target_regions(bnd, stations, county = "臺中市",
                                 townships = c("左區", "右區"))
  regions <- .tww_attach_station_pools(regions, bnd, stations, pool_size = 1)

  left  <- which(regions$township == "左區")
  right <- which(regions$township == "右區")
  expect_equal(regions$near_ids[[left]],  "A")   # nearest to left centroid
  expect_equal(regions$near_ids[[right]], "B")   # nearest to right centroid
  expect_equal(regions$in_ids[[left]],  "A")
})

test_that("fallback walks past NA stations to find non-NA values", {
  # A region with no station of its own; the two nearest stations are NA and
  # only the third-nearest reports a value. The walk must skip the NAs.
  regions <- data.frame(region = "R", stringsAsFactors = FALSE)
  regions$in_ids   <- list(character(0))
  regions$near_ids <- list(c("A", "B", "C"))     # distance order

  obs <- data.frame(
    station_id = c("A", "B", "C"),
    obs_time   = c("d1", "d1", "d1"),
    temp       = c(NA, NA, 25),                   # only the farthest has a value
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- .tww_aggregate_regions(obs, regions, "rain", list(), k = 10)
  expect_equal(out$region, "R")
  expect_equal(out$temp, 25)
  expect_true(out$used_fallback)
})

test_that("k_nearest limits how many fallback stations are averaged", {
  regions <- data.frame(region = "R", stringsAsFactors = FALSE)
  regions$in_ids   <- list(character(0))
  regions$near_ids <- list(c("A", "B", "C"))
  obs <- data.frame(
    station_id = c("A", "B", "C"),
    obs_time   = "d1",
    temp       = c(10, 20, 30),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  k2 <- .tww_aggregate_regions(obs, regions, "rain", list(), k = 2)
  expect_equal(k2$temp, 15)      # mean(10, 20): only the two nearest
  k_all <- .tww_aggregate_regions(obs, regions, "rain", list(), k = 10)
  expect_equal(k_all$temp, 20)   # mean(10, 20, 30)
})

test_that("get_region_weather helpers standardise, assign and pool", {
  testthat::skip_if_not_installed("sf")

  sq <- function(x0) sf::st_polygon(list(rbind(
    c(x0, 0), c(x0 + 1, 0), c(x0 + 1, 1), c(x0, 1), c(x0, 0))))
  shp <- sf::st_sf(
    site     = c("left", "right"),
    geometry = sf::st_sfc(sq(0), sq(2), crs = 4326)
  )
  bnd <- .tww_standardise_region(shp, "site")
  expect_true("region" %in% names(bnd))
  expect_setequal(bnd$region, c("left", "right"))

  stations <- data.frame(
    station_id = c("A", "B"),
    name       = c("a", "b"),
    lon = c(0.5, 2.5), lat = c(0.5, 0.5),
    stringsAsFactors = FALSE
  )
  st2 <- .tww_assign_region(stations, bnd)
  expect_equal(st2$region, c("left", "right"))

  reg <- data.frame(region = c("left", "right"), stringsAsFactors = FALSE)
  reg <- .tww_attach_region_pools(reg, bnd, st2, pool_size = 1)
  expect_equal(reg$in_ids[[which(reg$region == "left")]],  "A")
  expect_equal(reg$near_ids[[which(reg$region == "right")]], "B")
})

test_that(".tww_standardise_region errors on a missing id_field", {
  testthat::skip_if_not_installed("sf")
  sq  <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  shp <- sf::st_sf(site = "x", geometry = sf::st_sfc(sq, crs = 4326))
  expect_error(.tww_standardise_region(shp, "nope"), "not a column")
})
