# twweather <img src="man/figures/logo.png" align="right" height="139" alt="twweather logo" />

Download Taiwan historical weather observations from the NCHU
[*CWB Historical Weather Data Downloader*](https://mycolab.pp.nchu.edu.tw/historical_weather/index.php),
which redistributes Central Weather Administration (CWA / CODiS) station data.

The package gives you these functions:

| Function | Purpose |
|---|---|
| `get_stations()` | 測站基本資料 — station metadata (id, name, lon/lat, county) |
| `get_weather()` | 測量資料 — station observation time series (hourly / daily / monthly) |
| `get_township_weather()` | 加總到鄉鎮 — aggregate stations up to township level by coordinates |
| `station_panel()` / `plot_station_panel()` | 營運狀態面板 — panelview-style station operating-status panel (未設站 / 營運中 / 撤銷) over a time window |

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
# township boundaries as an sf layer. The default downloads the official NLSC
# 鄉鎮市區界線(TWD97經緯度) shapefile from data.gov.tw (dataset 7441); zipped
# shapefiles are unpacked automatically. Pass any sf-readable source to override.
bnd <- load_tw_townships()                 # or load_tw_townships("twtowns.shp")

tw <- get_township_weather(
  start = "2024-01-01", end = "2024-01-07", type = "daily",
  boundaries = bnd,
  county     = "臺中市",                    # districts are county + township
  townships  = c("北屯區", "西屯區"),       # omit to do every township
  k_nearest  = 3                            # nearest stations for the fallback
)
```

Townships are keyed on **county + township** because district names repeat
across Taiwan (中山區 is in both 臺北市 and 基隆市). `台`/`臺` are treated as
equal, so `county = "台中市"` works too.

Each requested district gets one row per time step. When a district has **no
valid value** for a given time step / variable — no station inside its polygon,
or every in-township station is `NA` — that cell is filled from the `k_nearest`
stations closest to the district centroid.

Output columns: `county`, `township`, `obs_time`, one column per aggregated
variable, `n_stations` (in-township stations feeding the row) and
`used_fallback` (`TRUE` when any cell came from the nearest-station pool). The
contributing in-township station ids are stored in `attr(tw, "stations")`.

Override the rule per column with `agg_fun`, e.g. sum sunshine hours too:

```r
get_township_weather(..., agg_fun = list("日照時數(hour)" = sum))
```

You can also use the building blocks directly:

```r
st  <- get_stations()
st  <- assign_township(st, bnd)            # adds township / county_geo columns
```

## 4. Station operating-status panel

A [panelview](https://yiqingxu.org/packages/panelView/)-style view of which
stations were operating over a window. Each station's `start_date` (set-up) and
`end_date` (decommission) classify every time step into one of three states —
`未設站` (not yet set up), `營運中` (operating) or `撤銷` (decommissioned) —
so you can see, at a glance, when stations came online and when they were retired.

```r
# build the long status table: one row per station per period
p <- station_panel(start = "1990-01-01", end = "2024-12-31", by = "year")
table(p$status)

# plot it (needs ggplot2; in Suggests)
# install.packages("ggplot2")
plot_station_panel(start = "1990-01-01", end = "2024-12-31", by = "year")
```

- `by` is `"year"` (default), `"month"` or `"day"` — the time resolution of the
  columns. `start` / `end` accept `Date` objects or `YYYYMMDD` / `YYYY-MM-DD`
  strings.
- By default the metadata is fetched with `active_only = FALSE`, so
  decommissioned stations are kept (otherwise `撤銷` could never show up). Pass
  your own metadata to restrict the panel:

```r
st  <- get_stations(active_only = FALSE)
tp  <- st[st$county == "臺北市", ]
plot_station_panel(tp, start = 20000101, end = 20241231, by = "month")
```

- `plot_station_panel()` returns a normal `ggplot`, so you can keep styling it.
  Useful arguments: `sort` (`"start"` / `"id"` / `"name"` / `"none"`),
  `colors` (named vector for the three states), `label_col` (`"station_id"` or
  `"name"`) and `labels` (force the y-axis labels on/off). With many stations
  the y labels are hidden automatically (see `max_labels`).

A station with a missing set-up date is treated as "set up before the window"
and a missing decommission date as "still operating", so stations with unknown
dates default to `營運中` rather than dropping out of the plot.

## Notes

- Station metadata comes from the CODiS `station_list` endpoint
  (<https://codis.cwa.gov.tw/api/station_list>); if that endpoint moves, pass
  your own `url=` to `get_stations()`.
- Township boundaries are **not** bundled. `load_tw_townships()` defaults to the
  official NLSC 鄉鎮市區界線(TWD97經緯度) shapefile on data.gov.tw
  (<https://data.gov.tw/dataset/7441>) and unpacks the zip for you. That download
  URL embeds a release date and changes occasionally — if it 404s, grab the
  current SHP link from the dataset page (or a local copy) and pass it via
  `source=`.
- Data source & terms: CWA CODiS via
  <https://mycolab.pp.nchu.edu.tw/historical_weather/>.

## License

MIT
