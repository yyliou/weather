# twweather

Download Taiwan historical weather observations from the NCHU
[*CWB Historical Weather Data Downloader*](https://mycolab.pp.nchu.edu.tw/historical_weather/index.php),
which redistributes Central Weather Administration (CWA / CODiS) station data.

The package gives you three functions:

| Function | Purpose |
|---|---|
| `get_stations()` | 測站基本資料 — station metadata (id, name, lon/lat, county) |
| `get_weather()` | 測量資料 — station observation time series (hourly / daily / monthly) |
| `get_township_weather()` | 加總到鄉鎮 — aggregate stations up to township level by coordinates |

## Install

```r
# install.packages("remotes")
remotes::install_github("yyliou/weather")
```

Core functions need only `jsonlite`. The township function additionally needs
`sf` (`install.packages("sf")`).

The functions carry roxygen comments but the `man/*.Rd` help pages aren't
checked in. To build them (and pass `R CMD check`), run once:

```r
# install.packages("roxygen2")
roxygen2::roxygenise()    # or devtools::document()
```


## 1. Station metadata

```r
library(twweather)

st <- get_stations()
head(st[, c("station_id", "name", "county", "lon", "lat")])
```

Station metadata comes from the CODiS station list
(<https://codis.cwa.gov.tw/StationData>). All station types (`cwb` 局屬氣象站,
`agr` 農業站, automatic and rainfall stations, ...) are flattened into one tidy
table with at least `station_id`, `name`, `lon`, `lat`, plus `altitude`,
`county`, `address`, `area`, `attribute`, `start_date`, `end_date` and
`active`. By default only operating stations are returned; pass
`active_only = FALSE` to include decommissioned ones, or `raw = TRUE` to get the
provider's original columns untouched.

## 2. Measurement data

```r
# one station, hourly
w <- get_weather("467490", "2024-01-01", "2024-01-07")

# several stations, daily (server returns a ZIP; combined automatically)
wd <- get_weather(c("466920", "466930"), 20240101, 20240131, type = "daily")
```

- `start` / `end` accept `Date` objects or `YYYYMMDD` / `YYYY-MM-DD` strings.
  `end` cannot be later than yesterday (the source truncates it).
- `type` is `"hourly"` (default), `"daily"` or `"monthly"`. Daily/monthly carry
  extra max/min/mean columns.
- A leading `station_id` column is always added; the original first column
  (observation time) is renamed `obs_time`.
- With `clean = TRUE` (default), value columns are coerced to numeric and CODiS
  missing-value sentinels (e.g. `-99.8`, `-9999`) become `NA`.

## 3. Township aggregation

Each station is reverse-geocoded — its longitude/latitude is matched to the
township polygon that contains it — then stations in the same township are
aggregated for every time step. **Rainfall is summed; everything else is
averaged.**

```r
# township boundaries as an sf layer (official 鄉鎮市區界線, or any sf source)
bnd <- load_tw_townships()                 # or load_tw_townships("twtowns.shp")

tw <- get_township_weather(
  start = "2024-01-01", end = "2024-01-07", type = "daily",
  boundaries = bnd,
  townships  = c("北屯區", "西屯區")        # omit to do every township
)
```

Output columns: `county`, `township`, `obs_time`, one column per aggregated
variable, and `n_stations` (how many stations fed each row). The contributing
station ids are stored in `attr(tw, "stations")`.

Override the rule per column with `agg_fun`, e.g. sum sunshine hours too:

```r
get_township_weather(..., agg_fun = list("日照時數(hour)" = sum))
```

You can also use the building blocks directly:

```r
st  <- get_stations()
st  <- assign_township(st, bnd)            # adds township / county_geo columns
```

## Notes

- Station metadata comes from the CODiS `station_list` endpoint
  (<https://codis.cwa.gov.tw/api/station_list>); if that endpoint moves, pass
  your own `url=` to `get_stations()`.
- Township boundaries are **not** bundled. `load_tw_townships()` reads any
  `sf`-readable source; for production use, download the official layer from
  data.gov.tw and pass its path.
- Data source & terms: CWA CODiS via
  <https://mycolab.pp.nchu.edu.tw/historical_weather/>.

## License

MIT
