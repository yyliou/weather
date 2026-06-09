# Tests for boundary standardisation, station assignment and region points.
# These touch `sf` only for geometry and run without any network access.

test_that("standardise_boundaries keeps a townid column when present", {
  testthat::skip_if_not_installed("sf")
  sq  <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  shp <- sf::st_sf(
    TOWNNAME   = "北屯區",
    COUNTYNAME = "臺中市",
    TOWNID     = "66000040",
    geometry   = sf::st_sfc(sq, crs = 4326)
  )
  std <- .tww_standardise_boundaries(shp, NULL, NULL)
  expect_true(all(c("township", "county", "townid") %in% names(std)))
  expect_equal(std$townid, "66000040")
})

test_that("assign_township carries through the townid code", {
  testthat::skip_if_not_installed("sf")
  sq <- function(x0) sf::st_polygon(list(rbind(
    c(x0, 0), c(x0 + 1, 0), c(x0 + 1, 1), c(x0, 1), c(x0, 0))))
  bnd <- sf::st_sf(
    TOWNNAME   = c("左區", "右區"),
    COUNTYNAME = c("臺中市", "臺中市"),
    TOWNID     = c("L01", "R01"),
    geometry   = sf::st_sfc(sq(0), sq(2), crs = 4326)
  )
  stations <- data.frame(
    station_id = c("A", "B"),
    name       = c("a", "b"),
    lon = c(0.5, 2.5), lat = c(0.5, 0.5),
    stringsAsFactors = FALSE
  )
  out <- assign_township(stations, bnd)
  expect_true("townid" %in% names(out))
  expect_equal(out$townid, c("L01", "R01"))
  expect_equal(out$township, c("左區", "右區"))
})

test_that("region_points returns one interior point per key", {
  testthat::skip_if_not_installed("sf")
  sq <- function(x0) sf::st_polygon(list(rbind(
    c(x0, 0), c(x0 + 1, 0), c(x0 + 1, 1), c(x0, 1), c(x0, 0))))
  bnd <- sf::st_sf(
    township = c("左區", "右區"),
    county   = c("臺中市", "臺中市"),
    townid   = c("L01", "R01"),
    geometry = sf::st_sfc(sq(0), sq(2), crs = 4326)
  )
  pts <- .tww_region_points(bnd, key = "townid")
  expect_setequal(pts$townid, c("L01", "R01"))
  left  <- pts[pts$townid == "L01", ]
  right <- pts[pts$townid == "R01", ]
  expect_true(left$lon  > 0 && left$lon  < 1)    # inside the left square
  expect_true(right$lon > 2 && right$lon < 3)    # inside the right square
})

test_that("get_region_weather helpers standardise and assign", {
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
})

test_that(".tww_standardise_region errors on a missing id_field", {
  testthat::skip_if_not_installed("sf")
  sq  <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  shp <- sf::st_sf(site = "x", geometry = sf::st_sfc(sq, crs = 4326))
  expect_error(.tww_standardise_region(shp, "nope"), "not a column")
})
