# Tests for the IDW interpolation engine. These exercise the pure numeric core
# (.tww_idw_interpolate / .tww_haversine_km) and need no network or sf.

test_that("haversine distance is symmetric and ~known", {
  # Taipei (121.50, 25.04) to Kaohsiung (120.30, 22.62) is ~290 km.
  d <- .tww_haversine_km(121.50, 25.04, 120.30, 22.62)
  expect_gt(d, 280)
  expect_lt(d, 310)
  expect_equal(.tww_haversine_km(0, 0, 0, 0), 0)
})

test_that("IDW blends the k nearest stations by inverse-distance weight", {
  targets  <- data.frame(townid = "T", lon = 0, lat = 0,
                         stringsAsFactors = FALSE)
  stations <- data.frame(station_id = c("A", "B", "C"),
                         name = c("a", "b", "c"),
                         lon = c(0.1, 0.2, 1.0), lat = c(0, 0, 0),
                         stringsAsFactors = FALSE)
  obs <- data.frame(
    station_id = c("A", "B", "C", "A", "B", "C"),
    obs_time   = c("d1", "d1", "d1", "d2", "d2", "d2"),
    temp       = c(10, 20, 99, NA, 20, 30),   # A missing on d2; C never near
    check.names = FALSE, stringsAsFactors = FALSE)

  out <- .tww_idw_interpolate(obs, targets, stations,
                              id_cols = "townid", power = 2, k_nearest = 2)
  expect_equal(nrow(out), 2L)                       # one row per time step

  d1 <- out[out$obs_time == "d1", ]
  # dB = 2*dA -> weights 1/dA^2 : 1/dB^2 = 4 : 1, C excluded by k = 2
  expect_equal(round(d1$temp, 6), round((4 * 10 + 1 * 20) / 5, 6))

  d2 <- out[out$obs_time == "d2", ]
  expect_equal(d2$temp, 20)                         # A is NA -> only B carries
})

test_that("rainfall is interpolated, not summed", {
  targets  <- data.frame(region = "R", lon = 0, lat = 0,
                         stringsAsFactors = FALSE)
  stations <- data.frame(station_id = c("A", "B"), name = c("a", "b"),
                         lon = c(0.1, 0.1), lat = c(0.1, -0.1),  # equidistant
                         stringsAsFactors = FALSE)
  obs <- data.frame(station_id = c("A", "B"), obs_time = "d1",
                    `降水量(mm)` = c(2, 6),
                    check.names = FALSE, stringsAsFactors = FALSE)
  out <- .tww_idw_interpolate(obs, targets, stations, id_cols = "region",
                              k_nearest = 2)
  expect_equal(out[["降水量(mm)"]], 4)             # mean(2, 6), not sum (8)
})

test_that("a cell with no reporting station is NA; others still filled", {
  targets  <- data.frame(region = "R", lon = 0, lat = 0,
                         stringsAsFactors = FALSE)
  stations <- data.frame(station_id = c("A", "B"), name = c("a", "b"),
                         lon = c(0.1, 0.2), lat = c(0, 0),
                         stringsAsFactors = FALSE)
  obs <- data.frame(
    station_id = c("A", "B"),
    obs_time   = c("d1", "d1"),
    rain       = c(NA, NA),
    temp       = c(15, 25),
    check.names = FALSE, stringsAsFactors = FALSE)
  out <- .tww_idw_interpolate(obs, targets, stations,
                              id_cols = "region", k_nearest = 5)
  expect_true(is.na(out$rain))
  expect_false(is.na(out$temp))
})

test_that("max_dist excludes stations beyond the cap (km)", {
  targets  <- data.frame(region = "R", lon = 0, lat = 0,
                         stringsAsFactors = FALSE)
  # A ~11 km east, B ~111 km east (1 deg lon ~ 111 km at the equator).
  stations <- data.frame(station_id = c("A", "B"), name = c("a", "b"),
                         lon = c(0.1, 1.0), lat = c(0, 0),
                         stringsAsFactors = FALSE)
  obs <- data.frame(station_id = c("A", "B"), obs_time = "d1",
                    temp = c(10, 30),
                    check.names = FALSE, stringsAsFactors = FALSE)
  near <- .tww_idw_interpolate(obs, targets, stations, id_cols = "region",
                               k_nearest = 5, max_dist = 50)
  expect_equal(near$temp, 10)                       # only A within 50 km
  both <- .tww_idw_interpolate(obs, targets, stations, id_cols = "region",
                               k_nearest = 5, max_dist = NULL)
  expect_true(both$temp > 10 && both$temp < 30)     # blend of A and B
})

test_that("a station on the target point is used directly (no Inf weight)", {
  targets  <- data.frame(region = "R", lon = 121.0, lat = 24.0,
                         stringsAsFactors = FALSE)
  stations <- data.frame(station_id = c("A", "B"), name = c("a", "b"),
                         lon = c(121.0, 121.5), lat = c(24.0, 24.0),
                         stringsAsFactors = FALSE)
  obs <- data.frame(station_id = c("A", "B"), obs_time = "d1",
                    temp = c(18, 28),
                    check.names = FALSE, stringsAsFactors = FALSE)
  out <- .tww_idw_interpolate(obs, targets, stations, id_cols = "region",
                              k_nearest = 2)
  expect_equal(out$temp, 18)                        # A coincides with the point
  expect_true(is.finite(out$temp))
})

test_that("empty observations yield a stable zero-row schema", {
  targets <- data.frame(region = "R", lon = 0, lat = 0,
                        stringsAsFactors = FALSE)
  stations <- data.frame(station_id = "A", name = "a", lon = 0.1, lat = 0,
                         stringsAsFactors = FALSE)
  obs <- data.frame(station_id = character(0), obs_time = character(0),
                    temp = numeric(0),
                    check.names = FALSE, stringsAsFactors = FALSE)
  out <- .tww_idw_interpolate(obs, targets, stations, id_cols = "region")
  expect_equal(nrow(out), 0L)
  expect_true(all(c("region", "obs_time", "n_stations") %in% names(out)))
})
