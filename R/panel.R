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
#'   \item `"Not yet established"` --- the whole period falls
#'     before `start_date`.
#'   \item `"Operating"` --- the period overlaps
#'     `[start_date, end_date]` (an open `end_date` means "still operating").
#'   \item `"Decommissioned"` --- the whole period falls after
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
#'   (otherwise the `"Decommissioned"` state can never appear).
#' @param succession `"auto"` (default) or `"off"`. When `"auto"` and the
#'   station table carries succession info, an operating station that has taken
#'   over from an older, re-coded/relocated station is shown in a distinct state:
#'   `"Operating (successor 1)"` for the first successor in a chain,
#'   `"Operating (successor 2)"` for the second. Succession is read
#'   from `id_before` / `id_after` columns when present; otherwise it is inferred
#'   from the `remark` text (see `infer_remark`). With no succession found, the
#'   plain three states are used. Supply `id_before` / `id_after` yourself for
#'   full control.
#' @param infer_remark Logical. When `TRUE` (default) and `succession != "off"`,
#'   missing `id_before` / `id_after` are inferred (conservatively) from the
#'   `remark` text. Set `FALSE` to use only explicitly supplied succession
#'   columns.
#'
#' @return A data frame with one row per station-by-period and columns
#'   `station_id`, `name` (if available), `county` (if available), `time` (a
#'   `Date` marking the start of the period), `period` (a label such as
#'   `"2020"` or `"2020-03"`) and `status` (a factor; levels are the three base
#'   states, expanded with `"Operating (successor 1)"` /
#'   `"Operating (successor 2)"` when succession is detected). The
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
#'                     start = "2020-01-01", end = "2024-12-31", by = "month")
#' }
#' @export
station_panel <- function(stations = NULL,
                          start, end,
                          by = c("year", "month", "day"),
                          active_only = FALSE,
                          succession = c("auto", "off"),
                          infer_remark = TRUE) {
  by         <- match.arg(by)
  succession <- match.arg(succession)

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
  np <- nrow(periods)
  ps <- as.numeric(periods$p_start)
  pe <- as.numeric(periods$p_end)
  sid <- as.character(stations$station_id)

  # Succession: chain depth (rank) and origin (root) per station. Members of one
  # chain share a `root` so they stack onto a single row, with the predecessor's
  # segment on the left and each successor's continuing to its right.
  rank <- stats::setNames(integer(length(sid)), sid)
  root <- stats::setNames(sid, sid)
  if (succession != "off") {
    st_s <- if (isTRUE(infer_remark)) .tww_infer_succession(stations) else stations
    rank <- .tww_succession_rank(st_s)[sid]
    root <- .tww_succession_root(st_s)[sid]
  }
  use_succ <- any(!is.na(rank) & rank >= 1L)
  unit <- if (use_succ) unname(root) else sid

  # Open-ended dates: unknown set-up = "before the window", unknown
  # decommission = "still operating".
  est_n <- as.numeric(est); est_n[is.na(est_n)] <- -Inf
  dec_n <- as.numeric(dec); dec_n[is.na(dec_n)] <-  Inf

  units   <- unique(unit)
  nu      <- length(units)
  op_lvl  <- .tww_status_levels()[2]
  not_lvl <- .tww_status_levels()[1]
  dec_lvl <- .tww_status_levels()[3]
  # representative metadata per unit = its origin (lowest-rank) member.
  rep_idx <- vapply(units, function(u) {
    m <- which(unit == u); m[which.min(rank[m])]
  }, integer(1))

  # status of each unit (row) in each period (column).
  status_mat <- matrix(not_lvl, nu, np)
  for (ui in seq_len(nu)) {
    m  <- which(unit == units[ui])
    me <- est_n[m]; md <- dec_n[m]; mr <- rank[m]
    operating <- outer(ps, md, `<=`) & outer(pe, me, `>=`)  # np x |members|
    earliest  <- min(me)
    for (j in seq_len(np)) {
      om <- which(operating[j, ])
      if (length(om)) {
        rk <- max(mr[om])                          # latest successor operating
        status_mat[ui, j] <- if (rk <= 0L) op_lvl
          else if (rk == 1L) "Operating (successor 1)"
          else "Operating (successor 2)"
      } else if (pe[j] < earliest) {
        status_mat[ui, j] <- not_lvl
      } else {
        status_mat[ui, j] <- dec_lvl
      }
    }
  }

  # units vary fastest within each period.
  idx_u <- rep.int(seq_len(nu), times = np)
  idx_p <- rep(seq_len(np), each = nu)
  status <- status_mat[cbind(idx_u, idx_p)]

  out <- data.frame(station_id = units[idx_u], stringsAsFactors = FALSE)
  if ("name" %in% names(stations)) {
    out$name <- as.character(stations$name)[rep_idx][idx_u]
  }
  if ("county" %in% names(stations)) {
    out$county <- as.character(stations$county)[rep_idx][idx_u]
  }
  out$time   <- periods$time[idx_p]
  out$period <- periods$label[idx_p]
  out$status <- factor(status, levels = .tww_status_levels(succession = use_succ))

  # Carry per-station dates for sorting in plot_station_panel().
  attr(out, "succession") <- use_succ
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
#' @param sort How to order rows on the y axis: `"start"` (default, by set-up
#'   date so the panel forms a staircase), `"duration"` (by how long the row
#'   operated, longest at the top), `"succession"` (length first, then by the
#'   row's current colour in the order operating / successor 1 / successor 2 /
#'   decommissioned), `"id"`, `"name"` or `"none"` (keep input order).
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
#' p <- station_panel(start = "2000-01-01", end = "2024-12-31", by = "year")
#' library(ggplot2)
#' plot_station_panel(p, sort = "id") + theme(legend.position = "top")
#' }
#' @export
plot_station_panel <- function(x = NULL,
                               start = NULL, end = NULL,
                               by = c("year", "month", "day"),
                               active_only = FALSE,
                               sort = c("start", "duration", "succession",
                                        "id", "name", "none"),
                               colors = .tww_status_colors(),
                               labels = NA,
                               max_labels = 60L,
                               label_col = c("station_id", "name"),
                               title = "Station operating status",
                               xlab = NULL,
                               ylab = "Station") {
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
  succ <- isTRUE(attr(panel, "succession")) ||
    any(grepl("successor", as.character(panel$status)))
  panel$status <- factor(panel$status,
                         levels = .tww_status_levels(succession = succ))

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
    ggplot2::scale_fill_manual(values = colors, drop = TRUE,
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

# Status levels, in display order. Without succession: not set up -> operating
# -> gone. With succession, "Operating" splits by how far down a relocation /
# re-coding chain the station sits (1st successor, 2nd-or-later successor).
.tww_status_levels <- function(succession = FALSE) {
  if (!succession) {
    return(c("Not yet established", "Operating", "Decommissioned"))
  }
  c("Not yet established", "Operating",
    "Operating (successor 1)", "Operating (successor 2)",
    "Decommissioned")
}

# Default fill palette: grey (not yet) / green (operating) / gold (1st
# successor) / purple (2nd+ successor) / red (gone). Named by level, so a plain
# three-state panel uses the matching subset.
.tww_status_colors <- function() {
  stats::setNames(
    c("grey88", "#4DAC60", "#E8B500", "#7B5EA7", "#C0444B"),
    .tww_status_levels(succession = TRUE))
}

# Predecessor map: pred[X] = the station X took over from. Built from `id_before`
# (the older code) and, where that is silent, the reverse of another station's
# `id_after`. Returns a character vector named by `station_id` (NA = origin).
.tww_pred_map <- function(stations) {
  ids  <- as.character(stations$station_id)
  pred <- stats::setNames(rep(NA_character_, length(ids)), ids)
  if ("id_before" %in% names(stations)) {
    b  <- as.character(stations$id_before)
    ok <- !is.na(b) & nzchar(b) & b %in% ids
    pred[ids[ok]] <- b[ok]
  }
  if ("id_after" %in% names(stations)) {
    a  <- as.character(stations$id_after)
    ok <- which(!is.na(a) & nzchar(a) & a %in% ids)
    for (i in ok) if (is.na(pred[[a[i]]])) pred[[a[i]]] <- ids[i]
  }
  pred
}

# Chain depth per station: how many *present* stations precede it. Only
# predecessors that are themselves in the table count, so a station whose old
# code is not in the data (a pure re-numbering, e.g. C1A970 關渡) is rank 0 — its
# bar is one continuous "Operating", not a stray successor colour. 0 = original,
# 1 = first successor, 2 = second successor, ... Named by `station_id`.
.tww_succession_rank <- function(stations) {
  pred <- .tww_pred_map(stations)
  ids  <- names(pred)
  rank <- stats::setNames(integer(length(ids)), ids)
  for (id in ids) {
    r <- 0L; cur <- pred[[id]]; seen <- character(0)
    while (!is.na(cur) && nzchar(cur) && cur %in% ids &&
           !(cur %in% seen) && r < 50L) {
      r <- r + 1L; seen <- c(seen, cur); cur <- pred[[cur]]
    }
    rank[[id]] <- r
  }
  rank
}

# Origin (rank-0) station of each station's succession chain, so all members of
# a chain share one `root` and can be stacked on a single panel row. Follows
# predecessors only while they are *present* in the table, so the root is the
# earliest member actually in the data. Named by `station_id`.
.tww_succession_root <- function(stations) {
  pred <- .tww_pred_map(stations)
  ids  <- names(pred)
  root <- stats::setNames(ids, ids)
  for (id in ids) {
    cur <- id; seen <- character(0)
    while (cur %in% ids) {
      p <- pred[[cur]]
      if (is.na(p) || !nzchar(p) || !(p %in% ids) || p %in% seen) break
      seen <- c(seen, cur); cur <- p
    }
    root[[id]] <- cur
  }
  root
}

# Best-effort inference of `id_before` / `id_after` from the free-text `remark`,
# which is where the CODiS station_list feed records succession (it documents
# the structured fields but does not return them). Reads the real phrasings:
#   * this station SUCCEEDS an older one  -> id_before, cues like
#     "(站碼466880)遷移之新站", "原氣象站(C0A540)…轉為…", "取代舊站", "升級/改制";
#   * this station BECAME a new code      -> id_after, cues like
#     "變更站碼為(C0UB10)", "由467770改為C0FA30", "更名為…".
# Candidate codes are taken only from clear contexts (parentheses, after 站碼/
# 改為/由), the station's own code is dropped, and "became" is checked before
# "succeeds" so a predecessor's remark (which also says "原(self)站") is read as
# id_after, not id_before. Never overrides a value already present.
.tww_infer_succession <- function(stations) {
  ids <- as.character(stations$station_id)
  id_before <- if ("id_before" %in% names(stations)) {
    as.character(stations$id_before)
  } else rep(NA_character_, length(ids))
  id_after <- if ("id_after" %in% names(stations)) {
    as.character(stations$id_after)
  } else rep(NA_character_, length(ids))

  if (!"remark" %in% names(stations)) {
    stations$id_before <- id_before
    stations$id_after  <- id_after
    return(stations)
  }

  rmk  <- as.character(stations$remark); rmk[is.na(rmk)] <- ""
  code <- "[0-9A-Z][0-9A-Z]{5}"                    # 6-char CWA station code
  # Phrases that mark this station as the *successor* (so the predecessor code
  # is the last code before the phrase) vs the *predecessor* (so the new code is
  # the first code after the phrase). "Successor" wins when both appear, which
  # disambiguates multi-history remarks like
  #   "...變更站碼為(C0C480)。原(C0C480)...轉為(C2C480)農業站"  -> id_before = C0C480.
  succ_cues <- c("轉為", "遷移之新站", "取代舊站", "升級為", "改制為")
  aft_cues  <- c("變更站碼為", "改碼為", "改為", "更名為")
  first_pos <- function(text, cues) {
    ps <- vapply(cues, function(cu) {
      m <- regexpr(cu, text); if (m > 0L) as.integer(m) else NA_integer_
    }, integer(1))
    if (all(is.na(ps))) NA_integer_ else min(ps, na.rm = TRUE)
  }
  for (i in seq_along(ids)) {
    r <- rmk[i]
    if (!nzchar(r)) next
    g <- gregexpr(code, r)[[1]]
    if (g[1] < 0L) next
    cs <- regmatches(r, gregexpr(code, r))[[1]]
    cp <- as.integer(g)
    keep <- cs != ids[i]                           # drop the station's own code
    cs <- cs[keep]; cp <- cp[keep]
    if (!length(cs)) next
    sp <- first_pos(r, succ_cues)
    ap <- first_pos(r, aft_cues)
    if (!is.na(sp)) {                              # this station succeeds another
      before <- cs[cp < sp]
      if (length(before) && (is.na(id_before[i]) || !nzchar(id_before[i]))) {
        id_before[i] <- before[length(before)]     # nearest code before the cue
      }
    } else if (!is.na(ap)) {                       # this station became another
      after <- cs[cp > ap]
      if (length(after) && (is.na(id_after[i]) || !nzchar(id_after[i]))) {
        id_after[i] <- after[1]                    # first code after the cue
      }
    }
  }
  stations$id_before <- id_before
  stations$id_after  <- id_after
  stations
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

# Last calendar day of the month containing each date in `d` (Date in, Date out,
# vectorised). Distinct from utils.R's string-valued `.tww_month_end_chr()`.
.tww_month_end <- function(d) {
  lt <- as.POSIXlt(as.Date(d))
  lt$mday <- 1L
  lt$mon  <- lt$mon + 1L
  as.Date(lt) - 1
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
  if (sort == "duration") {
    # Operating length per station. Prefer the carried set-up/decommission
    # dates (unknown set-up -> window start, unknown decommission -> window
    # end); otherwise fall back to counting operating periods in the panel.
    est <- attr(panel, "start_date")
    dec <- attr(panel, "end_date")
    if (!is.null(est)) {
      s <- as.numeric(est[ids])
      e <- as.numeric(dec[ids])
      win_s <- attr(panel, "start")
      win_e <- attr(panel, "end")
      if (!is.null(win_s)) s[is.na(s)] <- as.numeric(as.Date(win_s))
      if (!is.null(win_e)) e[is.na(e)] <- as.numeric(as.Date(win_e))
      dur <- e - s
    } else {
      op  <- panel[grepl("^Operating", as.character(panel$status)), , drop = FALSE]
      cnt <- tapply(rep(1L, nrow(op)), as.character(op$station_id), sum)
      dur <- as.numeric(cnt[ids])
    }
    # Shortest first (bottom), longest last (top of the y axis).
    dur[is.na(dur)] <- -Inf
    return(ids[order(dur, ids)])
  }
  if (sort == "succession") {
    # Length first (longest at the top), then the row's *current* colour in the
    # order operating (green) -> successor 1 (gold) -> successor 2 (purple) ->
    # decommissioned (red).
    est <- attr(panel, "start_date")
    dec <- attr(panel, "end_date")
    if (!is.null(est)) {
      s <- as.numeric(est[ids]); e <- as.numeric(dec[ids])
      win_s <- attr(panel, "start"); win_e <- attr(panel, "end")
      if (!is.null(win_s)) s[is.na(s)] <- as.numeric(as.Date(win_s))
      if (!is.null(win_e)) e[is.na(e)] <- as.numeric(as.Date(win_e))
      dur <- e - s
    } else {
      op  <- panel[grepl("^Operating", as.character(panel$status)), , drop = FALSE]
      cnt <- tapply(rep(1L, nrow(op)), as.character(op$station_id), sum)
      dur <- as.numeric(cnt[ids])
    }
    dur[is.na(dur)] <- -Inf
    # terminal colour = status in the latest period of each row
    o  <- order(as.character(panel$station_id), panel$time)
    pp <- panel[o, , drop = FALSE]
    term <- tapply(as.character(pp$status), as.character(pp$station_id),
                   function(s) s[length(s)])
    pri_map <- c("Operating" = 1L, "Operating (successor 1)" = 2L,
                 "Operating (successor 2)" = 3L, "Decommissioned" = 4L,
                 "Not yet established" = 5L)
    pri <- unname(pri_map[term[ids]]); pri[is.na(pri)] <- 9L
    return(ids[order(dur, pri, ids)])
  }
  # sort == "start": by set-up date, then id. Use carried attribute when
  # present, else infer from the first operating period in the panel.
  est <- attr(panel, "start_date")
  key <- if (!is.null(est)) {
    as.numeric(est[ids])
  } else {
    op <- panel[grepl("^Operating", as.character(panel$status)), , drop = FALSE]
    first_op <- tapply(as.numeric(op$time), as.character(op$station_id), min)
    as.numeric(first_op[ids])
  }
  # Stations with unknown set-up date sort first (they predate the window).
  key[is.na(key)] <- -Inf
  ids[order(key, ids)]
}

# Small null-coalescing helper (kept local to avoid a hard rlang dependency).
`%||%` <- function(a, b) if (is.null(a)) b else a
