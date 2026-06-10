# twweather <img src="man/figures/logo.png" align="right" height="139" alt="twweather logo" />

## 1. Overview

`twweather` downloads Taiwan historical weather observations from the NCHU
[*CWB Historical Weather Data Downloader*](https://mycolab.pp.nchu.edu.tw/historical_weather/index.php),
which redistributes Central Weather Administration (CWA / CODiS) station data. It
provides station metadata, station observation time series (hourly / daily /
monthly), a panelview-style station operating-status panel, and interpolation of
station data to arbitrary polygons (e.g. townships) by inverse-distance
weighting.

```r
# install.packages("remotes")
remotes::install_github("yyliou/weather")
```

Core functions need only `jsonlite`. The interpolation and boundary helpers
(`get_region_weather()`, `load_tw_townships()`, `assign_township()`) also need
`sf`. Installing `data.table` speeds up CSV parsing. The `man/*.Rd` help pages
are not checked in; run `roxygen2::roxygenise()` once to build them.

## 2. Functions

| Function | Purpose |
|---|---|
| `get_stations()` | Station metadata (id, name, lon/lat, county, dates). |
| `get_weather()` | Station observation time series (hourly / daily / monthly). |
| `get_region_weather()` | Interpolate weather to any polygons you supply, by IDW. |
| `station_panel()` / `plot_station_panel()` | Build / plot a panelview-style station operating-status panel. |
| `load_tw_townships()` | Download/read the official NLSC township boundary layer as an `sf` object. |
| `assign_township()` | Tag each station with the township polygon it falls in. |

## 3. Arguments

**`get_stations(url, active_only, raw)`** — `active_only = TRUE` (default) keeps
only operating stations; `raw = TRUE` returns the provider's original columns.

**`get_weather(station_id, start, end, type, clean, na_codes, quiet, max_ids)`**

| Argument | Description | Default |
|---|---|---|
| `station_id` | One or more CWA station ids (`"467490"`, `c("466920","466930")`). | required |
| `start`, `end` | Inclusive dates: `Date`/`POSIXt` or `YYYYMMDD` / `YYYY-MM-DD`. `end` cannot be later than yesterday. | required |
| `type` | `"hourly"` (default), `"daily"`, or `"monthly"`. | `"hourly"` |
| `clean` | Coerce values to numeric and map CODiS missing-value sentinels to `NA`. | `TRUE` |
| `na_codes` | Sentinel values treated as missing when `clean = TRUE`. | CODiS codes |
| `max_ids` | Max ids per HTTP call (larger sets are chunked). | `100` |

**`get_region_weather(start, end, type, shp, id_field, regions, power, k_nearest, max_dist, pool_size, dist_from, stations, obs, clean, na_codes, quiet)`**

| Argument | Description | Default |
|---|---|---|
| `shp` | Boundary polygons: an `sf` object or a path/URL to a shapefile/GeoPackage/GeoJSON/zipped shapefile. | required |
| `id_field` | Column in `shp` identifying each region; its values become `region`. Polygons sharing a value are unioned. | required |
| `regions` | Optional vector of `id_field` values to keep; `NULL` returns all. | `NULL` |
| `power` | IDW distance exponent (higher = nearer stations weighted more). | `2` |
| `k_nearest` | Number of nearest reporting stations blended per cell. | `8` |
| `max_dist` | Optional cap (km) on contributing-station distance; beyond it ignored. | `NULL` |
| `pool_size` | Nearest stations per region downloaded when `obs` is not supplied. | `max(30, 3*k_nearest)` |
| `dist_from` | Distance basis: `"surface"` (representative interior point), `"centroid"`, or `"edge"` (polygon boundary; 0 inside). | `"surface"` |
| `stations`, `obs` | Optional pre-fetched station table / pre-downloaded observations (the recommended fast path for repeated runs). | `NULL` |
| `start`, `end`, `type`, `clean`, `na_codes`, `quiet` | Passed to `get_weather()` when `obs` is not supplied. | — |

**`station_panel(stations, start, end, by, active_only, succession, infer_remark)`**
and **`plot_station_panel(x, start, end, by, active_only, sort, colors, labels,
max_labels, label_col, title, xlab, ylab)`** — `by` is `"year"` (default),
`"month"`, or `"day"`; `sort` is `"start"`/`"duration"`/`"succession"`/`"id"`/
`"name"`/`"none"`; `succession` is `"auto"` (default) or `"off"`.

## 4. Output codebook

**`get_stations()`** — a data frame with at least `station_id`, `name`, `lon`,
`lat`, plus (when available) `name_en`, `altitude`, `county`, `town`, `address`,
`area`, `attribute`, `start_date`, `end_date`, `remark`, `active`, and succession
codes `id_before` / `id_after`.

**`get_weather()`** — a leading `station_id` column, an `obs_time` column (the
feed's first column, normalised to `YYYY-MM-DD`, or `YYYY-MM` for monthly), and
one column per measured variable (original Chinese/English labels). Daily/monthly
carry extra max/min/mean columns. With `clean = TRUE`, missing-value sentinels
become `NA` while genuinely signed variables (temperature, dew point,
evaporation) keep negatives; trace precipitation (`T` / `-9991`) becomes `0`.

**`get_region_weather()`** — `region` (your `id_field` values), `obs_time`, one
column per interpolated variable, and `n_stations` (nearby stations reporting at
that step). The IDW `power` is kept in `attr(x, "power")` and the source station
ids in `attr(x, "stations")`.

**`station_panel()`** — one row per station per period: `station_id`, `name`,
`county`, `time` (Date at period start), `period` (label such as `"2020"` or
`"2020-03"`), and `status` (factor: `Not yet established` / `Operating` /
`Decommissioned`, expanded with `Operating (successor 1/2)` when succession is
detected). `plot_station_panel()` returns a `ggplot`.

## 5. Examples

```r
library(twweather)

# Station metadata
st <- get_stations()

# Measurement data: one station hourly, or several daily
w  <- get_weather("467490", "2024-01-01", "2024-01-07")
wd <- get_weather(c("466920", "466930"), "2024-01-01", "2024-01-31",
                  type = "daily")

# Station operating-status panel (needs ggplot2)
plot_station_panel(start = "1990-01-01", end = "2024-12-31", by = "year")
```

The IDW value for each *polygon × time step × variable* is

$$ v = \frac{\sum_i w_i\,x_i}{\sum_i w_i}, \qquad w_i = \frac{1}{d_i^{\,\text{power}}} $$

where $d_i$ is the distance from the polygon to station $i$. The recommended flow
is two steps — download once, then interpolate — so you can re-run with different
polygons or settings without re-downloading:

```r
library(sf)

# step 1 — download once
stations <- get_stations()
obs <- get_weather(stations$station_id, "2024-01-01", "2024-01-07", type = "daily")

# step 2a — interpolate to every Taiwan township (key on the unique code)
bnd <- load_tw_townships()          # official NLSC layer (data.gov.tw 7441)
tw  <- get_region_weather(
  start = "2024-01-01", end = "2024-01-07", type = "daily",
  shp = bnd, id_field = "townid",
  stations = stations, obs = obs,
  power = 2, k_nearest = 8
)

# step 2b — or interpolate to your own polygons (e.g. a hex grid)
hex <- st_sf(geometry = st_make_grid(st_transform(st_union(bnd), 3826),
                                     cellsize = 15000, square = FALSE))
hex$hex_id <- sprintf("H%04d", seq_len(nrow(hex)))
hw <- get_region_weather("2024-01-01", "2024-01-07", type = "daily",
                         shp = hex, id_field = "hex_id",
                         stations = stations, obs = obs)
```

Key townships on `townid`, not name: district names repeat across Taiwan
(中山區 is in both 臺北市 and 基隆市) whereas the code is unique.

## 6. Notes

- **Two-step flow is the fast path.** The interpolation is fast (vectorised);
  the cost is downloading/parsing station CSVs. Fetch `obs` once (optionally
  `saveRDS()`), then reuse via `obs=` with the **same `stations`** table so
  coordinates line up. Installing `data.table` further speeds up parsing.
- **Succession.** When a station is relocated/re-coded its record continues under
  a successor (e.g. 466880 板橋 → 466881 新北). With `succession = "auto"` a chain
  is stacked onto one row. Links are inferred from `remark` text by default;
  supply `id_before` / `id_after` for full accuracy, or set `succession = "off"`.
- **Monthly `type`.** `end` is automatically extended to the end of its month,
  because the source returns a month's record only once the window reaches the
  month end (a sub-month window comes back empty).
- **Township boundaries are not bundled.** `load_tw_townships()` downloads the
  NLSC layer from data.gov.tw 7441; that URL embeds a release date and changes
  occasionally. If it 404s, fetch the current SHP link from
  <https://data.gov.tw/dataset/7441> and pass it via `source=`.

## 7. References & citation

If you use this package, please cite it together with the underlying data sources
(APA 7th edition):

- Liou, Yu-You. (2026). *twweather: Download Taiwan CWA historical weather
  observations* (Version 0.1.0) [Computer software].
  https://github.com/yyliou/weather
- Central Weather Administration. (n.d.). *Observation Data Inquire System
  (CODiS)* [Data set]. Retrieved June 9, 2026, from https://codis.cwa.gov.tw/
- National Land Surveying and Mapping Center. (n.d.). *鄉鎮市區界線
  (TWD97經緯度) [Township and district administrative boundaries]* [Data set].
  政府資料開放平臺 (data.gov.tw). https://data.gov.tw/dataset/7441
- Raingel. (n.d.). *Taiwan historical meteorological observations database*
  [Computer software]. GitHub. https://github.com/Raingel/historical_weather
- Mou, H., Liu, L., & Xu, Y. (2023). Panel data visualization in R (panelView)
  and Stata (panelview). *Journal of Statistical Software, 107*(7), 1–20.
  https://doi.org/10.18637/jss.v107.i07
- Pebesma, E. (2018). Simple features for R: Standardized support for spatial
  vector data. *The R Journal, 10*(1), 439–446. https://doi.org/10.32614