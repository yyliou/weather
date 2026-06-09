# twweather <img src="man/figures/logo.png" align="right" height="139" alt="twweather logo" />

Download Taiwan historical weather observations from the NCHU
[*CWB Historical Weather Data Downloader*](https://mycolab.pp.nchu.edu.tw/historical_weather/index.php),
which redistributes Central Weather Administration (CWA / CODiS) station data.

The package gives you these functions:

| Function | Purpose |
|---|---|
| `get_stations()` | 測站基本資料 — station metadata (id, name, lon/lat, county) |
| `station_panel()` / `plot_station_panel()` | 營運狀態面板 — panelview-style station operating-status panel (Not yet established / Operating / Decommissioned) over a time window |
| `get_weather()` | 測量資料 — station observation time series (hourly / daily / monthly) |
| `get_township_weather()` | 內插到鄉鎮 — interpolate weather to township level by inverse-distance weighting (keyed on townid) |
| `get_region_weather()` | 內插到自訂區域 — interpolate weather over your own shapefile, keyed by one id column |

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

## 2. Station operating-status panel

A [panelview](https://yiqingxu.org/packages/panelView/)-style view of which
stations were operating over a window. Each station's `start_date` (set-up) and
`end_date` (decommission) classify every time step into one of three states —
`Not yet established`, `Operating` or `Decommissioned` — so you can see, at a
glance, when stations came online and when they were retired. The labels are in
English so the plot carries no Chinese text.

```r
# build the long status table: one row per station per period
p <- station_panel(start = "1990-01-01", end = "2024-12-31", by = "year")
table(p$status)

# plot it (needs ggplot2; in Suggests)
# install.packages("ggplot2")
plot_station_panel(start = "1990-01-01", end = "2024-12-31", by = "year")
```

![Station operating-status panel](man/figures/station-panel.png)

- `by` is `"year"` (default), `"month"` or `"day"` — the time resolution of the
  columns. `start` / `end` accept `Date` objects or `YYYYMMDD` / `YYYY-MM-DD`
  strings.
- By default the metadata is fetched with `active_only = FALSE`, so
  decommissioned stations are kept (otherwise `Decommissioned` could never show
  up). Pass your own metadata to restrict the panel:

```r
st  <- get_stations(active_only = FALSE)
tp  <- st[st$county == "臺北市", ]
plot_station_panel(tp, start = "2000-01-01", end = "2024-12-31", by = "month")
```

- `plot_station_panel()` returns a normal `ggplot`, so you can keep styling it.
  Useful arguments: `sort` (`"start"` / `"duration"` / `"id"` / `"name"` /
  `"none"` — `"duration"` orders stations by how long they operated, longest at
  the top), `colors` (named vector for the three states), `label_col`
  (`"station_id"` or `"name"`) and `labels` (force the y-axis labels on/off).
  With many stations the y labels are hidden automatically (see `max_labels`).

A station with a missing set-up date is treated as "set up before the window"
and a missing decommission date as "still operating", so stations with unknown
dates default to `Operating` rather than dropping out of the plot.

## 3. Measurement data

```r
# one station, hourly
w <- get_weather("467490", "2024-01-01", "2024-01-07")

# several stations, daily (server returns a ZIP; combined automatically)
wd <- get_weather(c("466920", "466930"), "2024-01-01", "2024-01-31", type = "daily")
```

- `start` / `end` accept `Date` objects or `YYYYMMDD` / `YYYY-MM-DD` strings.
  `end` cannot be later than yesterday (the source truncates it).
- `type` is `"hourly"` (default), `"daily"` or `"monthly"`. Daily/monthly carry
  extra max/min/mean columns.
- A leading `station_id` column is always added; the original first column
  (observation time) is renamed `obs_time` and normalised to ISO format
  (`YYYY-MM-DD`, or `YYYY-MM` for monthly).
- With `clean = TRUE` (default), value columns are coerced to numeric and CODiS
  missing-value sentinels (e.g. `-99.8`, `-9999`, and literal text like `"NA"` /
  `"--"`) become `NA`.

## 4. Interpolate to any polygons

One engine interpolates weather to **whatever polygons you give it**.
`get_region_weather()` is the general function; **`get_township_weather()` is
just a convenience wrapper** for the official township layer (a special case: it
keys on `townid` and adds the `county` / `township` name columns). Same
algorithm, same options (`power` / `k_nearest` / `max_dist` / `pool_size` /
`obs=`).

For every *polygon × time step × variable* the value is the **pure
inverse-distance-weighted (IDW)** mean of the `k_nearest` stations that report
it:

$$
v \;=\; \frac{\sum_i w_i\,x_i}{\sum_i w_i}, \qquad w_i = \frac{1}{d_i^{\,\text{power}}}
$$

where $d_i$ is the great-circle distance from the polygon's representative point
to station $i$. Every variable — rainfall included — is interpolated; nothing is
averaged over a polygon's own stations (a station sitting on the point is used
directly, so nearby stations still dominate). If no station within range reports
the variable, the cell is `NA`. Polygons sharing an `id_field` value are unioned
and treated as one region.

The recommended flow is **two steps** — download once (section 3), then
interpolate — so you can re-run with different polygons or settings without
re-downloading:

```r
# step 1 — download the measurement data once (see section 3)
stations <- get_stations()
obs <- get_weather(stations$station_id, "2024-01-01", "2024-01-07",
                   type = "daily")
# (optional) persist across R sessions:
# saveRDS(list(stations = stations, obs = obs), "cwa_2024w1.rds")
```

### Example A — Taiwan townships

```r
bnd <- load_tw_townships()          # official NLSC 鄉鎮市區界線 (data.gov.tw 7441)

# step 2 — interpolate to every township (re-run freely, no re-download)
tw <- get_township_weather(
  start = "2024-01-01", end = "2024-01-07", type = "daily",
  boundaries = bnd, stations = stations, obs = obs,
  power = 2, k_nearest = 8, max_dist = NULL
)
# townid = c("66000040", "66000050") restricts to a few districts.

# the same thing via the general function (keyed on any column you choose):
tw2 <- get_region_weather(
  start = "2024-01-01", end = "2024-01-07", type = "daily",
  shp = bnd, id_field = "townid",   # or "TOWNNAME", "COUNTYNAME", ...
  stations = stations, obs = obs
)
```

Output: `townid`, `county`, `township`, `obs_time`, one column per interpolated
variable, and `n_stations` (nearby stations reporting at that step). `townid` is
the unambiguous key — district *names* repeat across Taiwan (中山區 is in both
臺北市 and 基隆市), so the wrapper keys on the code.

### Example B — a 200 km² hexagonal grid (main island only)

Make your own polygons and pass them straight in:

```r
library(sf)

# 1. main-island outline: drop the 外島 counties, union, project to metres
bnd  <- load_tw_townships()
main <- bnd[!bnd$county %in% c("澎湖縣", "金門縣", "連江縣"), ]
main <- st_transform(st_union(st_make_valid(main)), 3826)   # TWD97 / TM2 (metres)

# 2. ~200 km² regular hexagons: build, then rescale cellsize to hit the target
mk  <- function(cs) st_sf(geometry = st_make_grid(main, cellsize = cs, square = FALSE))
hex <- mk(15000)
hex <- mk(15000 * sqrt(200e6 / as.numeric(median(st_area(hex)))))  # ≈ 200 km²

# 3. keep only cells covering land, give each a stable id
hex <- hex[lengths(st_intersects(hex, main)) > 0, ]
hex$hex_id <- sprintf("H%04d", seq_len(nrow(hex)))

# 4. interpolate (reuse the cached stations / obs from step 1)
hw <- get_region_weather(
  start = "2024-01-01", end = "2024-01-07", type = "daily",
  shp = hex, id_field = "hex_id",
  stations = stations, obs = obs
)
```

Output: `region` (your `hex_id` values), `obs_time`, one column per interpolated
variable, and `n_stations`. (`get_region_weather()` re-projects `shp` to lon/lat
internally, so any CRS is fine.)

### Notes & speed

If you omit `obs` (and `stations`), the function downloads the nearest
`pool_size` stations to each polygon for you — convenient for a one-off, but the
two-step `obs=` flow is the fast path when you run repeatedly or cover many
polygons. The interpolation itself is fast (vectorised matrix algebra); the cost
is downloading and parsing the station CSVs, so:

- **Faster parsing (automatic).** With the suggested [`data.table`](https://cran.r-project.org/package=data.table)
  installed, CSV parsing uses `data.table::fread`; otherwise a base-R parser is
  used. Just `install.packages("data.table")`.
- **Download once, reuse via `obs=`.** Fetch `obs` once (optionally `saveRDS()`),
  then call either function as many times as you like with `stations=`/`obs=` —
  use the **same `stations`** table you built `obs` from so coordinates line up.

> **monthly note** — for `type = "monthly"` the end date is automatically
> extended to the end of its month, because the source returns a month's record
> only once the window reaches the month end (a sub-month window comes back
> empty — which previously surfaced as all-`NA`).

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
