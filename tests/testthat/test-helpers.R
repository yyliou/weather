test_that("date normalisation accepts common forms", {
  expect_equal(.tww_as_yyyymmdd("2024-01-02"), "20240102")
  expect_equal(.tww_as_yyyymmdd(20240102), "20240102")
  expect_equal(.tww_as_yyyymmdd(as.Date("2024-01-02")), "20240102")
  expect_error(.tww_as_yyyymmdd("2024-13-40"))
  expect_error(.tww_as_yyyymmdd(c("20240101", "20240102")))
})

test_that("url is built correctly and omits type for hourly", {
  u <- .tww_build_url("467490", "20240101", "20240107", "hourly")
  expect_false(grepl("type=", u))
  expect_true(grepl("station_id=467490", u))

  ud <- .tww_build_url(c("466920", "466930"), "20240101", "20240107", "daily")
  expect_true(grepl("type=daily", ud))
  expect_true(grepl("466920,466930", utils::URLdecode(ud)))
})

test_that("cleaning converts sentinels to NA", {
  df <- data.frame(
    obs_time = c("2024-01-01 00:00:00", "2024-01-01 01:00:00"),
    temp = c("18.4", "-99.8"),
    rain = c("0.0", "1.5"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- .tww_clean(df, .tww_default_na_codes())
  expect_true(is.numeric(out$temp))
  expect_true(is.na(out$temp[2]))
  expect_equal(out$rain, c(0.0, 1.5))
})

test_that("rbind_fill unions columns", {
  a <- data.frame(station_id = "1", obs_time = "t", x = 1, stringsAsFactors = FALSE)
  b <- data.frame(station_id = "2", obs_time = "t", y = 2, stringsAsFactors = FALSE)
  out <- .tww_rbind_fill(list(a, b))
  expect_setequal(names(out), c("station_id", "obs_time", "x", "y"))
  expect_equal(nrow(out), 2)
})

test_that("station id is parsed from zip member names", {
  expect_equal(.tww_id_from_name("466920_20190816-20190913.csv"), "466920")
  expect_equal(.tww_id_from_name("/tmp/x/467490.csv"), "467490")
})
