# Offline tests for the CODiS station-list parsing. These mimic the structure
# jsonlite produces for https://codis.cwa.gov.tw/api/station_list (the `data`
# element: one row per attribute group, with an `item` list-column of station
# data frames) so no network is required.

make_payload <- function() {
  payload <- data.frame(
    stationAttribute = c("cwb", "agr"),
    stringsAsFactors = FALSE
  )
  payload$item <- list(
    data.frame(
      stationID        = c("466920", "466880"),
      stationName      = c("臺北", "板橋"),
      altitude         = c(6.3, 9.7),
      longitude        = c(121.514853, 121.442017),
      latitude         = c(25.037658, 24.997647),
      countryName      = c("臺北市", "新北市"),
      address          = c("a", "b"),
      area             = c("北區", "北區"),
      stationStartDate = c("1896-01-01", "1972-03-01"),
      stationEndDate   = c("", "2022-12-31"),    # 466880 decommissioned
      webRemark        = c("", ""),
      stringsAsFactors = FALSE
    ),
    data.frame(
      stationID        = "72C440",
      stationName      = "桃園農改場",
      altitude         = 70,
      longitude        = 121.030583,
      latitude         = 24.950944,
      countryName      = "桃園市",
      address          = "x",
      area             = "北區",
      stationStartDate = "1985-02-01",
      stationEndDate   = "",
      webRemark        = "",
      flow             = "z",                    # extra column unique to group
      stringsAsFactors = FALSE
    )
  )
  payload
}

test_that("station groups are flattened and tagged with attribute", {
  flat <- .tww_flatten_station_list(make_payload())
  expect_equal(nrow(flat), 3L)
  expect_true("stationAttribute" %in% names(flat))
  expect_true("flow" %in% names(flat))          # columns unioned across groups
  expect_setequal(unique(flat$stationAttribute), c("cwb", "agr"))
})

test_that("normalise maps columns and derives active from end date", {
  flat <- .tww_flatten_station_list(make_payload())

  all_st <- .tww_normalise_stations(flat, active_only = FALSE)
  expect_equal(nrow(all_st), 3L)
  expect_true(all(c("station_id", "name", "lon", "lat", "altitude",
                    "county", "area", "attribute", "start_date",
                    "end_date", "active") %in% names(all_st)))
  # county comes from countryName; attribute carried through
  expect_equal(all_st$county[all_st$station_id == "466920"], "臺北市")
  expect_equal(all_st$attribute[all_st$station_id == "72C440"], "agr")
  # active is FALSE exactly for the station with an end date
  expect_false(all_st$active[all_st$station_id == "466880"])
  expect_true(all_st$active[all_st$station_id == "466920"])
  expect_true(is.na(all_st$end_date[all_st$station_id == "466920"]))
})

test_that("active_only drops decommissioned stations", {
  flat <- .tww_flatten_station_list(make_payload())
  active <- .tww_normalise_stations(flat, active_only = TRUE)
  expect_setequal(active$station_id, c("466920", "72C440"))
})
