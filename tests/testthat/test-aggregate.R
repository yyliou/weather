test_that("township aggregation sums rain and averages the rest", {
  obs <- data.frame(
    station_id = c("A", "B", "A", "B"),
    county     = "臺中市",
    township   = "北屯區",
    obs_time   = c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-02"),
    `氣溫(℃)`     = c(18, 20, 16, 18),
    `降水量(mm)`  = c(1, 3, 0, 4),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- .tww_aggregate_township(obs, "降水|雨量|precip|rain", list())

  expect_equal(nrow(out), 2)                       # two days
  d1 <- out[out$obs_time == "2024-01-01", ]
  expect_equal(d1[["氣溫(℃)"]], 19)                # mean(18, 20)
  expect_equal(d1[["降水量(mm)"]], 4)              # sum(1, 3)
  expect_equal(d1$n_stations, 2)
})

test_that("agg_fun overrides the default statistic", {
  obs <- data.frame(
    station_id = c("A", "B"),
    county = "x", township = "y", obs_time = "t",
    `日照時數(hour)` = c(2, 3),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- .tww_aggregate_township(
    obs, "降水|雨量", list("日照時數(hour)" = sum)
  )
  expect_equal(out[["日照時數(hour)"]], 5)
})
