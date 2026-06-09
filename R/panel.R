#' Build a station operating-status panel
#'
#' Turns station metadata into a long, "panel" table of operating status over a
#' time window: one row per station per time step, with a three-level `status`
#' factor. This is the data behind [plot_station_panel()], modelled on the
#' \pkg{panelview} treatment-status plot (Mou, Liu & Xu, 2023): each station is
#' a unit and each time step is coloured by whether the station had been set up
#' yet, was operating, or had been decommissioned.
#'
#' Status for a station in a given period is derived from its `start_date`
#' (set-up) and `end_date` (decommission) using interval overlap, so a period
#' counts as operating if the station's operating interval touches it at all:
#'
#' \itemize{
#'   \item `"未設站"` (not yet set up) --- the whole period falls
#'     before `start_date`.
#'   \item `"營運中"` (operating) --- the period overlaps
#'     `[start_date, end_date]` (an open `end_date` means "still operating").
#'   \item `"撤銷"` (decommissioned) --- the whole period falls after
#'     `end_date`.
#' }
#'
#' A missing `start_date` is treated as "set up before the window" and a missing
#' `end_date` as "never decommissioned", so stations with unknown dates default
#' to operating rather than disappearing from the plot.
#'
#' @param stations Station metadata. Either a data frame as returned by
#'   [get_stations()] (it must contain `station_id`; `start_date` / `end_date`
#'   are used when present), or `NULL` (default) to fetch it automatically with
#'   `get_stations(active_only = active_only)`.
#' @param start,end Window start and end (inclusive). `Date`/`POSIXt` objects or
#'   `YYYYMMDD` / `YYYY-MM-DD` strings.
#' @param by Time resolution of the columns: `"year"` (default), `"month"` or
#'   `"day"`.
#' @param active_only Logical. Only used when `stations = NULL`; passed to
#'   [get_stations()]. Defaults to `FALSE` so decommissioned stations are kept
#'   (otherwise the `"撤銷"` state can never appear).
#'
#' @return A data frame with one row per station-by-period and columns
#'   `station_id`, `name` (if available), `county` (if available), `time` (a
#'   `Date` marking the start of the period), `period` (a label such as
#'   `"2020"` or `"2020-03"`) and `status` (an ordered factor with levels
#'   `"未設站"`, `"營運中"`, `"撤銷"`). The
#'   set-up / decommission dates are carried in `attr(x, "start_date")` and
#'   `attr(x, "end_date")` (named by `station_id`) for sorting downstream.
#'
#' @seealso [plot_station_panel()], [get_stations()]
#'
#' @examples
#' \dontrun{
#' # All stations, yearly, over two decades
#' p <- station_panel(start = "2000-01-01", end = "2024-12-31", by = "year")
#' table(p$status)
#'
#' # A handful of stations, monthly, reusing already-fetched metadata
#' st <- get_stations(active_only = FALSE)
#' p2 <- station_panel(st[st$county == "臺北市", ],
#'                     start = 20200101, end = 20241231, by = "month")
#' }
#' @export
station_panel <- function(stations = NULL,
                          start, end,
                          by = c("year", "month", "day"),
                          active_only = FALSE) {
  by <- match.arg(by)

  if (is.null(stations)) {
    stations <- get_stations(active_only = active_only)
  } else if (!is.data.frame(stations)) {
    stop("`stations` must be a data frame from get_stations(), or NULL.",
         call. = FALSE)
  }
  if (!"station_id" %in% names(stations) || nrow(stations) == 0L) {
    stop("`stations` must be a non-empty table with a `station_id` column.",
         call. = FALSE)
  }

  # Tolerate metadata that lacks date columns: treat as fully unknown.
  est <- if ("start_date" %in% names(stations)) {
    as.Date(stations$start_date)
  } else {
    rep(as.Date(NA), nrow(stations))
  }
  dec <- if ("end_date" %in% names(stations)) {
    as.Date(stations$end_date)
  } else {
    rep(as.Date(NA), nrow(stations))
  }

  start <- .tww_as_date(start, "start")
  end   <- .tww_as_date(end, "end")
  if (end < start) {
    stop("`end` (", end, ") is before `start` (", start, ").", call. = FALSE)
  }

  periods <- .tww_seq_periods(start, end, by)
  ns <- nrow(stations)
  np <- nrow(periods)

  # Cell grid: stations vary fastest within each period.
  idx_s <- rep.int(seq_len(ns), times = np)
  idx_p <- rep(seq_len(np), each = ns)

  est_c <- est[idx_s]
  dec_c <- dec[idx_s]
  ps_c  <- periods$p_start[idx_p]
  pe_c  <- periods$p_end[idx_p]

  status <- rep(.tww_status_levels()[2], length(idx_s))   # "營運中"
  status[!is.na(dec_c) & ps_c > dec_c] <- .tww_status_levels()[3]  # "撤銷"
  status[!is.na(est_c) & pe_c < est_c] <- .tww_status_levels()[1]  # "未設站"

  out <- data.frame(
    station_id = as.character(stations$station_id)[idx_s],
    stringsAsFactors = FALSE
  )
  if ("name" %in% names(stations))   out$name   <- as.character(stations$name)[idx_s]
  if ("county" %in% names(stations)) out$county <- as.character(stations$county)[idx_s]
  out$time   <- periods$time[idx_p]
  out$period <- periods$label[idx_p]
  out$status <- factor(status, levels = .tww_status_levels())

  # Carry per-station dates for sorting in plot_station_panel().
  attr(out, "start_date") <- stats::setNames(est, as.character(stations$station_id))
  attr(out, "end_date")   <- stats::setNames(dec, as.character(stations$station_id))
  attr(out, "by")    <- by
  attr(out, "start") <- start
  attr(out, "end")   <- end
  rownames(out) <- NULL
  out
}

#' Panelview-style plot of station operating status
#'
#' Draws a station-by-time grid coloured by operating status (not yet set up /
#' operating / decommissioned), in the spirit of the \pkg{panelview} package's
#' treatment-status display. Each row is a station, each column a time step, and
#' the fill shows the station's status in that period as computed by
#' [station_panel()].
#'
#' Requires \pkg{ggplot2} (in `Suggests`); install it with
#' `install.packages("ggplot2")`. The returned object is a normal `ggplot`, so
#' you can keep adding layers / themes to it.
#'
#' @param x Either a panel table from [station_panel()] (detected by a `status`
#'   column), a station metadata data frame from [get_stations()], or `NULL`.
#'   When it is not already a panel, [station_panel()] is called with `start`,
#'   `end`, `by` and `active_only` to build one.
#' @param start,end,by,active_only Passed to [station_panel()] when `x` still
#'   needs to be turned into a panel. Ignored when `x` is already a panel.
#' @param sort How to order stations on the y axis: `"start"` (default, by
#'   set-up date so the panel forms a staircase), `"id"`, `"name"` or `"none"`
#'   (keep input order).
#' @param colors Named character vector of fill colours for the three states.
#'   Defaults to a grey / green / red scheme.
#' @param labels Logical or `NA`. Whether to print station labels on the y axis.
#'   `NA` (default) shows them only when there are at most `max_labels`
#'   stations.
#' @param max_labels Threshold used when `labels = NA`. Default `60`.
#' @param label_col Which column to use for y-axis labels, `"station_id"`
#'   (default) or `"name"`.
#' @param title,xlab,ylab Plot title and axis titles. Sensible defaults are
#'   supplied; set to `NULL` to drop.
#'
#' @return A `ggplot` object (returned invisibly-friendly: print it to draw).
#'
#' @seealso [station_panel()]
#'
#' @examples
#' \dontrun{
#' # Straight from the live station list
#' plot_station_panel(start = "1990-01-01", end = "2024-12-31", by = "year")
#'
#' # Reuse a prebuilt panel and restyle
#' p <- station_panel(start = 20000101, end = 20241231, by = "year")
#' library(ggplot2)
#' plot_station_panel(p, sort = "id") + theme(legend.position = "top")
#' }
#' @export
plot_station_panel <- function(x = NULL,
                               start = NULL, end = NULL,
                               by = c("year", "month", "day"),
                               active_only = FALSE,
                               sort = c("start", "id", "name", "none"),
                               colors = .tww_status_colors(),
                               labels = NA,
                               max_labels = 60L,
                               label_col = c("station_id", "name"),
                               title = "測站營運狀態",
                               xlab = NULL,
                               ylab = "測站") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_station_panel() needs the 'ggplot2' package. ",
         "Install it with install.packages(\"ggplot2\"), or build the data ",
         "with station_panel() and plot it however you like.", call. = FALSE)
  }
  by        <- match.arg(by)
  sort      <- match.arg(sort)
  label_col <- match.arg(label_col)

  # Build a panel unless we were handed one already.
  panel <- if (is.data.frame(x) && "status" %in% names(x)) {
    x
  } else {
    if (is.null(start) || is.null(end)) {
      stop("Provide a panel from station_panel(), or `start` and `end` so one ",
           "can be built.", call. = FALSE)
    }
    station_panel(stations = x, start = start, end = end,
                  by = by, active_only = active_only)
  }

  if (!"status" %in% names(panel)) {
    stop("`panel` has no `status` column; was it built by station_panel()?",
         call. = FALSE)
  }
  panel$status <- factor(panel$status, levels = .tww_status_levels())

  # Order stations on the y axis.
  ord <- .tww_station_order(panel, sort)
  panel$station_id <- factor(panel$station_id, levels = ord)

  # Choose y labels.
  show_labels <- if (is.na(labels)) {
    length(ord) <= max_labels
  } else {
    isTRUE(labels)
  }
  y_breaks <- ord
  y_labs   <- ord
  if (label_col == "name" && "name" %in% names(panel)) {
    lk <- panel[!duplicated(panel$station_id),
                c("station_id", "name"), drop = FALSE]
    y_labs <- lk$name[match(ord, as.character(lk$station_id))]
    y_labs[is.na(y_labs)] <- ord[is.na(y_labs)]
  }

  tile_w <- .tww_tile_width(attr(panel, "by") %||% by)

  # Column names are referenced non-standardly inside aes(); declared in
  # utils::globalVariables() below to keep R CMD check quiet.
  gg <- ggplot2::ggplot(
    panel,
    ggplot2::aes(x = time, y = station_id, fill = status)
  )
  gg <- gg +
    ggplot2::geom_tile(width = tile_w, height = 0.95,
                       colour = "white", linewidth = 0.05) +
    ggplot2::scale_fill_manual(values = colors, drop = FALSE,
                               name = NULL) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text()
    )

  if (show_labels) {
    gg <- gg + ggplot2::scale_y_discrete(breaks = y_breaks, labels = y_labs)
  } else {
    gg <- gg + ggplot2::scale_y_discrete(breaks = NULL) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }
  gg
}

# Quiet R CMD check about the bare column names used inside ggplot2::aes().
utils::globalVariables(c("time", "station_id", "status"))

# Internal helpers ------------------------------------------------------------

# The three status levels, in display order (not set up -> operating -> gone).
.tww_status_levels <- function() {
  c("未設站", "營運中", "撤銷")
}

# Default fill palette: grey (not yet) / green (operating) / red (gone).
.tww_status_colors <- function() {
  stats::setNames(c("grey88", "#4DAC60", "#C0444B"), .tww_status_levels())
}

# Parse a date-ish input into a single `Date` (accepts Date/POSIXt, or
# YYYYMMDD / YYYY-MM-DD strings/numbers). Mirrors `.tww_as_yyyymmdd()`.
.tww_as_date <- function(x, arg = "date") {
  as.Date(.tww_as_yyyymmdd(x, arg), format = "%Y%m%d")
}

# Build the sequence of time-step columns covering [start, end] at resolution
# `by`. Returns a data frame: `time` (period start, for the x axis), `p_start`
# and `p_end` (the period's inclusive date bounds) and a `label`.
.tww_seq_periods <- function(start, end, by) {
  if (by == "year") {
    ys <- as.integer(format(start, "%Y"))
    ye <- as.integer(format(end, "%Y"))
    yr <- ys:ye
    p_start <- as.Date(sprintf("%04d-01-01", yr))
    p_end   <- as.Date(sprintf("%04d-12-31", yr))
    label   <- sprintf("%04d", yr)
  } else if (by == "month") {
    first <- as.Date(format(start, "%Y-%m-01"))
    last  <- as.Date(format(end,   "%Y-%m-01"))
    p_start <- seq(first, last, by = "month")
    p_end   <- .tww_month_end(p_start)
    label   <- format(p_start, "%Y-%m")
  } else { # day
    p_start <- seq(start, end, by = "day")
    p_end   <- p_start
    label   <- format(p_start, "%Y-%m-%d")
  }
  data.frame(time = p_start, p_start = p_start, p_end = p_end,
             label = label, stringsAsFactors = FALSE)
}

# Last calendar day of the month containing each date in `d`.
.tww_month_end <- function(d) {
  nextm <- as.Date(cut(d, "month")) # first of this month (robust)
  # add one month, then step back a day
  y <- as.integer(format(nextm, "%Y"))
  m <- as.integer(format(nextm, "%m"))
  m2 <- m + 1L
  y2 <- y + (m2 > 12L)
  m2 <- ((m2 - 1L) %% 12L) + 1L
  as.Date(sprintf("%04d-%02d-01", y2, m2)) - 1L
}

# Approximate tile width (in days) so tiles abut along a Date x axis.
.tww_tile_width <- function(by) {
  switch(by, year = 365, month = 30, day = 1, 365)
}

# Order station ids for the y axis according to `sort`.
.tww_station_order <- function(panel, sort) {
  ids <- unique(as.character(panel$station_id))
  if (sort == "none") return(ids)
  if (sort == "id")   return(sort(ids))
  if (sort == "name" && "name" %in% names(panel)) {
    lk <- panel[!duplicated(panel$station_id), , drop = FALSE]
    nm <- lk$name[match(ids, as.character(lk$station_id))]
    return(ids[order(nm, ids, na.last = TRUE)])
  }
  # sort == "start": by set-up date, then id. Use carried attribute when
  # present, else infer from the first operating period in the panel.
  est <- attr(panel, "start_date")
  key <- if (!is.null(est)) {
    as.numeric(est[ids])
  } else {
    op <- panel[panel$status == .tww_status_levels()[2], , drop = FALSE]
    first_op <- tapply(as.numeric(op$time), as.character(op$station_id), min)
    as.numeric(first_op[ids])
  }
  # Stations with unknown set-up date sort first (they predate the window).
  key[is.na(key)] <- -Inf
  ids[order(key, ids)]
}

# Small null-coalescing helper (kept local to avoid a hard rlang dependency).
`%||%` <- function(a, b) if (is.null(a)) b else a
