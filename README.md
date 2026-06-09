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
| `get_township_weather()` | 加總到鄉鎮 — aggregate stations up to township level by coordinates (keyed on townid) |
| `get_region_weather()` | 加總到自訂區域 — aggregate stations over your own shapefile, keyed by one id column |

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

## 4. Township aggregation

Each station is reverse-geocoded — its longitude/latitude is matched to the
township polygon that contains it — then stations in the same township are
aggregated for every time step. **Rainfall is summed; everything else is
averaged.**

```r
# township boundaries as an sf layer. The default downloads the official NLSC
# 鄉鎮市區界線(TWD97經緯度) shapefile from data.gov.tw (dataset 7441); zipped
# shapefiles are unpacked automatically. Pass any sf-readable source to override.
bnd <- load_tw_townships()                 # or load_tw_townships("twtowns.shp")

# every township that has a station:
tw_all <- get_township_weather(
  start = "2024-01-01", end = "2024-01-07", type = "daily", boundaries = bnd
)

# just a couple of townships, selected by their townid codes:
tw <- get_township_weather(
  start = "2024-01-01", end = "2024-01-07", type = "daily",
  boundaries = bnd,
  townid     = c("66000040", "66000050"),   # omit to do every township
  k_nearest  = 10                            # nearest non-NA stations for fallback
)
```

Townships are keyed on **`townid`** — the official township code (`TOWNID` /
`TOWNCODE`) carried through from the boundary layer. A single code is
unambiguous, unlike district *names* which repeat across Taiwan (中山區 is in
both 臺北市 and 基隆市). The boundary layer must therefore include a township-code
column; `load_tw_townships()` keeps it for you.

Each requested township gets one row per time step (a **balanced, gap-free
panel**). When a township has **no valid value** for a given time step /
variable — no station inside its polygon, or every in-township station is `NA`
there — that cell is filled by walking outward from the township centroid and
averaging the `k_nearest` (default 10) closest stations that **actually report a
value** there, skipping any that are `NA`. `pool_size` controls how many nearby
stations are downloaded to search (default `max(30, 3 * k_nearest)`).

Output columns: `townid`, `county`, `township`, `obs_time`, one column per
aggregated variable, `n_stations` (in-township stations feeding the row) and
`used_fallback` (`TRUE` when any cell came from the nearest-station pool). The
contributing in-township station ids are stored in `attr(tw, "stations")`.

Override the rule per column with `agg_fun`, e.g. sum sunshine hours too:

```r
get_township_weather(..., agg_fun = list("日照時數(hour)" = sum))
```

You can also use the building blocks directly:

```r
st  <- get_stations()
st  <- assign_township(st, bnd)            # adds township / county_geo / townid
```

## 5. Aggregation over your own shapefile

`get_region_weather()` is the general-purpose sibling of
`get_township_weather()`: instead of the official township layer, you supply
**your own** polygons (an `sf` object, or a path/URL to a shapefile / GeoPackage
/ GeoJSON / zipped shapefile) and name the column that identifies each region.
Everything else — point-in-polygon assignment, rain-summed/else-averaged
aggregation, and the same balanced-panel nearest non-NA fallback — works exactly
as above.

```r
rw <- get_region_weather(
  start = "2024-01-01", end = "2024-01-07", type = "daily",
  shp      = "my_regions.shp",   # or an sf object you already loaded
  id_field = "site_name",        # the column that names each region
  regions  = NULL,               # optional subset of id_field values to keep
  k_nearest = 10
)
```

Output columns: `region` (your `id_field` values), `obs_time`, one column per
aggregated variable, `n_stations` and `used_fallback`. Polygons that share an
`id_field` value are unioned and treated as a single region.

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
