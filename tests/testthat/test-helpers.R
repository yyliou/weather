test_that("date normalisation accepts common forms", {
  expect_equal(.tww_as_yyyymmdd("2024-01-02"), "20240102")
  expect_equal(.tww_as_yyyymmdd(20240102), "20240102")
  expect_equal(.tww_as_yyyymmdd(as.Date("2024-01-02")), "20240102")
  expect_error(.tww_as_yyyymmdd("2024-13-40"))
  expect_error(.tww_as_yyyymmdd(c("20240101", "20240102")))
})

test_that("obs_time is normalised to ISO and dashed values are left alone", {
  expect_equal(.tww_iso_obs_time(c("20240101", "20240131")),
               c("2024-01-01", "2024-01-31"))
  expect_equal(.tww_iso_obs_time("202401"), "2024-01")          # monthly
  expect_equal(.tww_iso_obs_time("2024-01-01 01:00:00"),
               "2024-01-01 01:00:00")                            # untouched
  expect_equal(.tww_iso_obs_time("2024-01-01"), "2024-01-01")    # already ISO
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

test_that("cleaning treats literal 'NA' text (and padding) as missing", {
  df <- data.frame(
    obs_time = c("2024-01-01", "2024-01-02", "2024-01-03"),
    temp     = c("18.4", "NA", " NA "),   # literal text, one padded
    pres     = c("1010", "--", "1012"),   # other NA-like token
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- .tww_clean(df, .tww_default_na_codes())
  expect_true(is.numeric(out$temp))       # column coerces despite the "NA" text
  expect_equal(out$temp, c(18.4, NA, NA))
  expect_true(is.numeric(out$pres))
  expect_equal(out$pres, c(1010, NA, 1012))
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

test_that("zip member ids are matched against the requested ids", {
  ids <- c("466920", "467490", "72G600")
  expect_equal(.tww_match_id("467490_20240101-20240107.csv", ids), "467490")
  # code is NOT the leading token (prefix / Chinese name) - the weak point the
  # plain leading-token parse got wrong, leaving nothing to match the station
  expect_equal(.tww_match_id("stationData_466920_2024.csv", ids), "466920")
  expect_equal(.tww_match_id("臺中_467490.csv", ids), "467490")
  expect_equal(.tww_match_id("72G600.csv", ids), "72G600")
  # nothing matches -> falls back to the leading token
  expect_equal(.tww_match_id("weird.csv", ids), "weird")
})

test_that("an in-file station column is detected", {
  df <- data.frame(obs_time = "t", station_id = "466920", temp = 1,
                   check.names = FALSE, stringsAsFactors = FALSE)
  expect_equal(.tww_station_col(df), "station_id")
  plain <- data.frame(obs_time = "t", temp = 1,
                      check.names = FALSE, stringsAsFactors = FALSE)
  expect_true(is.na(.tww_station_col(plain)))
})

test_that(".tww_read_csv parses, normalises obs_time and cleans (fast or base)", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("觀測時間(hour),氣溫(℃),降水量(mm)",
               "20240101,18.4,0.0",
               "20240102,NA,1.5"), f, useBytes = TRUE)
  df <- .tww_read_csv(f, .tww_default_na_codes(), clean = TRUE)
  expect_equal(names(df)[1], "obs_time")
  expect_equal(df$obs_time, c("2024-01-01", "2024-01-02"))
  expect_true(is.numeric(df[["氣溫(℃)"]]))
  expect_equal(df[["氣溫(℃)"]], c(18.4, NA))   # literal "NA" text -> NA
  expect_equal(df[["降水量(mm)"]], c(0.0, 1.5))
})

test_that(".tww_read_csv returns an empty frame for a 'no data' notice", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines("No data available for this station.", f)
  expect_equal(nrow(.tww_read_csv(f, .tww_default_na_codes(), TRUE)), 0L)
})

test_that("fast path and base parser agree on the same input", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("obs,a,b", "20240101,1,2", "20240102,3,X"), f)  # X is an NA token
  expect_equal(.tww_read_csv(f, .tww_default_na_codes(), TRUE),
               .tww_read_csv_base(f, .tww_default_na_codes(), TRUE))
})

test_that("trace precipitation (T / -9991) becomes 0, not NA", {
  df <- data.frame(
    obs_time     = c("2024-01-01", "2024-01-02", "2024-01-03"),
    `降水量(mm)` = c("0.0", "T", "-9991"),
    check.names = FALSE, stringsAsFactors = FALSE)
  out <- .tww_clean(df, .tww_default_na_codes())
  expect_equal(out[["降水量(mm)"]], c(0, 0, 0))
  # but "T" in a non-precipitation column is still treated as missing
  df2 <- data.frame(obs_time = "a", `氣溫(℃)` = "T",
                    check.names = FALSE, stringsAsFactors = FALSE)
  expect_true(is.na(.tww_clean(df2, .tww_default_na_codes())[["氣溫(℃)"]]))
})

test_that("negative rainfall / precip-hours sentinels are cleaned", {
  df <- data.frame(
    obs_time      = c("2024-01-01", "2024-01-02"),
    `降水量(mm)`   = c("0.0", "-9.96"),
    `降水時數(hr)` = c("1.0", "-9.5"),
    check.names = FALSE, stringsAsFactors = FALSE)
  out <- .tww_clean(df, .tww_default_na_codes())
  expect_equal(out[["降水量(mm)"]],   c(0.0, NA))
  expect_equal(out[["降水時數(hr)"]], c(1.0, NA))
})

test_that("large-magnitude negative sentinels are cleaned in any column", {
  df <- data.frame(
    obs_time = c("a", "b", "c"),
    soil     = c("19.5", "-99.5", "-99.95"),   # -99.8, -9991 ... family, any scale
    check.names = FALSE, stringsAsFactors = FALSE)
  out <- .tww_clean(df, .tww_default_na_codes())
  expect_equal(out$soil, c(19.5, NA, NA))
})

test_that("legitimately-negative variables are preserved", {
  df <- data.frame(
    obs_time   = c("a", "b"),
    `氣溫(℃)`   = c("-9.5", "12.0"),   # valid sub-zero mountain temperature
    EvapA      = c("-2.3", "3.1"),     # negative evaporation = it rained (valid)
    check.names = FALSE, stringsAsFactors = FALSE)
  out <- .tww_clean(df, .tww_default_na_codes())
  expect_equal(out[["氣溫(℃)"]], c(-9.5, 12.0))
  expect_equal(out$EvapA, c(-2.3, 3.1))
})

test_that("out-of-range wind directions are cleaned, valid ones kept", {
  df <- data.frame(
    obs_time          = c("a", "b", "c"),
    `風向(360degree)` = c("360.0", "990.0", "0.0"),
    check.names = FALSE, stringsAsFactors = FALSE)
  out <- .tww_clean(df, .tww_default_na_codes())
  expect_equal(out[["風向(360degree)"]], c(360.0, NA, 0.0))
})
