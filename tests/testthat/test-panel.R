# Offline tests for the station operating-status panel. No network needed:
# we hand station_panel() a small metadata frame with known set-up /
# decommission dates and check the three-state classification.

make_stations <- function() {
  data.frame(
    station_id = c("A", "B", "C", "D"),
    name       = c("一直開", "中途撤", "日期未知", "只知撤站日"),
    county     = c("臺北市", "新北市", "桃園市", "新北市"),
    start_date = as.Date(c("1990-01-01", "2010-06-15", NA, NA)),
    end_date   = as.Date(c(NA, "2015-03-20", NA, "2005-12-31")),
    stringsAsFactors = FALSE
  )
}

stat <- function(p, id, period) {
  as.character(p$status[p$station_id == id & p$period == period])
}

test_that("yearly panel has the right shape and levels", {
  p <- station_panel(make_stations(), 20000101, 20241231, by = "year")
  expect_equal(nrow(p), 4L * 25L)                 # 4 stations x 25 years
  expect_equal(levels(p$status), c("Not yet established", "Operating", "Decommissioned"))
  expect_setequal(unique(p$period), as.character(2000:2024))
  expect_s3_class(p$time, "Date")
})

test_that("three states are derived from set-up / decommission dates", {
  p <- station_panel(make_stations(), "2000-01-01", "2024-12-31", by = "year")

  # A: operating the whole window (no end date)
  expect_true(all(p$status[p$station_id == "A"] == "Operating"))

  # B: not set up -> operating -> decommissioned, with overlap rules
  expect_equal(stat(p, "B", "2009"), "Not yet established")
  expect_equal(stat(p, "B", "2010"), "Operating")   # set up mid-2010
  expect_equal(stat(p, "B", "2015"), "Operating")   # decommissioned mid-2015 (overlap)
  expect_equal(stat(p, "B", "2016"), "Decommissioned")

  # C: unknown dates default to operating, never "Not yet established"
  expect_true(all(p$status[p$station_id == "C"] == "Operating"))
  expect_false(any(p$status[p$station_id == "C"] == "Not yet established"))

  # D: missing set-up date, so only operating -> decommissioned
  expect_equal(stat(p, "D", "2005"), "Operating")
  expect_equal(stat(p, "D", "2006"), "Decommissioned")
  expect_false(any(p$status[p$station_id == "D"] == "Not yet established"))
})

test_that("by = 'month' produces month columns with correct count", {
  p <- station_panel(make_stations(), "2020-01-01", "2020-03-31", by = "month")
  expect_setequal(unique(p$period), c("2020-01", "2020-02", "2020-03"))
  expect_equal(nrow(p), 4L * 3L)
})

test_that("metadata without date columns falls back to operating", {
  st <- data.frame(station_id = c("X", "Y"), stringsAsFactors = FALSE)
  p <- station_panel(st, 20200101, 20201231, by = "year")
  expect_true(all(p$status == "Operating"))
})

test_that("bad inputs are rejected", {
  expect_error(station_panel(make_stations(), 20241231, 20200101, by = "year"),
               "before")
  expect_error(station_panel(42, 20200101, 20201231), "data frame")
})

test_that("duration sort orders stations by operating length", {
  p <- station_panel(make_stations(), "2000-01-01", "2024-12-31", by = "year")
  # B operates ~5y, D ~6y, C the full window, A the longest -> shortest first.
  expect_equal(.tww_station_order(p, "duration"), c("B", "D", "C", "A"))
})

test_that("plot_station_panel returns a ggplot when ggplot2 is available", {
  testthat::skip_if_not_installed("ggplot2")
  p  <- station_panel(make_stations(), 20000101, 20241231, by = "year")
  gg <- plot_station_panel(p)
  expect_s3_class(gg, "ggplot")

  # also builds the panel itself when handed raw metadata + a window
  gg2 <- plot_station_panel(make_stations(), start = 20000101, end = 20241231)
  expect_s3_class(gg2, "ggplot")
})
