utils::globalVariables(c("start", "end"))

#' Grant panel time-to-completion estimator with scheduled breaks
#'
#' Estimates remaining time in a grant review panel using a stabilized average
#' review duration and a set of scheduled breaks. The stabilized average uses
#' pseudo-count shrinkage toward a planned timer:
#' \deqn{\hat\mu = \frac{prior\_n \cdot planned\_mean + S}{prior\_n + n},}
#' where \eqn{S} is elapsed minutes since \code{begin_hhmm} and \eqn{n} is the
#' number of completed reviews. Scheduled breaks that overlap the projected
#' work window are added to the completion time.
#'
#' By default, the function prints a console summary and (optionally) draws a
#' timeline plot. Break periods are embedded into the main timeline (thicker
#' segments on the same line), and the information panel is placed at the
#' bottom center of the plot.
#'
#' @param begin_hhmm Character scalar giving the panel start time on a 24-hour
#'   clock. Accepts \code{"HH:MM"}, \code{"H:MM"}, or \code{"HHMM"} (e.g.,
#'   \code{"09:36"} or \code{"936"}). Interpreted on \code{date}.
#' @param review_total Integer scalar, total number of grants/reviews scheduled.
#' @param reviews_completed Integer scalar, number of grants/reviews completed so far.
#' @param break_hhmm Optional character vector of break start times on a 24-hour
#'   clock (same formats as \code{begin_hhmm}). Interpreted on \code{date}.
#' @param break_lens Optional numeric vector of break lengths in minutes. Must
#'   have the same length as \code{break_hhmm}.
#' @param planned_mean Numeric scalar, planned minutes per review used as the
#'   shrinkage target.
#' @param prior_n Non-negative numeric scalar. Strength of the shrinkage toward
#'   \code{planned_mean}, interpreted as "pseudo-reviews".
#' @param tz Character scalar. Time zone used for parsing and printing times.
#' @param T POSIXct giving the "current time" for the calculation. Defaults to
#'   \code{Sys.time()}.
#' @param date Date used to interpret \code{begin_hhmm} and \code{break_hhmm}.
#'   Defaults to today's date in \code{tz} (based on \code{T}).
#' @param print_console Logical; if \code{TRUE}, prints a summary block to the console.
#' @param plot Logical; if \code{TRUE}, draws the timeline plot (requires \pkg{ggplot2}).
#'
#' @return Invisibly returns a list with elements:
#' \describe{
#'   \item{review_remain}{Number of reviews remaining.}
#'   \item{elapsed_minutes}{Elapsed minutes since \code{begin_hhmm}.}
#'   \item{avg_rev_time_emp}{Empirical mean minutes per review (\code{elapsed_minutes / reviews_completed}) or \code{NA} if \code{reviews_completed == 0}.}
#'   \item{avg_rev_time_est}{Stabilized mean minutes per review (shrinkage estimate).}
#'   \item{work_minutes}{Estimated work minutes remaining (excluding breaks).}
#'   \item{breaks_added_min}{Total minutes of overlapping breaks added.}
#'   \item{T_fin}{POSIXct estimated completion time.}
#'   \item{total_minutes_left}{Estimated minutes remaining until completion.}
#'   \item{plot}{A \code{ggplot} object if \code{plot=TRUE} and \pkg{ggplot2} is available; otherwise \code{NULL}.}
#' }
#'
#' @examples
#' \dontrun{
#' panel_finish_time(
#'   begin_hhmm = "09:36",
#'   review_total = 40,
#'   reviews_completed = 3,
#'   break_hhmm = c("11:30", "13:45", "16:15"),
#'   break_lens = c(15, 30, 15),
#'   planned_mean = 15,
#'   prior_n = 10
#' )
#' }
#'
#' @export
panel_finish_time <- function(begin_hhmm,
                              review_total,
                              reviews_completed,
                              break_hhmm = NULL,
                              break_lens = NULL,
                              planned_mean = 12,
                              prior_n = 6,
                              T = Sys.time(),
                              tz = attr(T, "tzone"),
                              date = as.Date(T),
                              print_console = TRUE,
                              plot = TRUE) {

  # ---- helper: interpret HH:MM / HHMM on today's date ----
  to_posix_today <- function(hhmm) {
    s <- as.character(hhmm)
    if (grepl("^\\d{3,4}$", s)) {
      s <- sprintf("%04d", as.integer(s))
      s <- paste0(substr(s, 1, 2), ":", substr(s, 3, 4))
    }
    if (!grepl("^\\d{1,2}:\\d{2}$", s)) {
      stop("Time must be 'HH:MM', 'H:MM', or 'HHMM' (e.g., '09:36' or '936').")
    }
    as.POSIXct(paste(date, s), tz = tz)
  }

  # ---- coerce current time ----
  if (!inherits(T, "POSIXct")) T <- as.POSIXct(T)

  # Derive timezone from T if not explicitly supplied
  if (is.null(tz) || is.na(tz) || tz == "") {
    tz <- attr(T, "tzone")
  }
  if (is.null(tz) || is.na(tz) || tz == "") {
    tz <- ""
  }

  date <- as.Date(T)

  # ---- begin time ----
  begin_time <- to_posix_today(begin_hhmm)

  # ---- counts ----
  stopifnot(review_total >= 1)
  stopifnot(reviews_completed >= 0, reviews_completed <= review_total)

  review_remain <- review_total - reviews_completed

  # ---- elapsed review time so far (minutes) ----
  elapsed_minutes <- as.numeric(difftime(T, begin_time, units = "mins"))
  if (elapsed_minutes < 0) stop("T is earlier than begin_time (check begin_hhmm or system time).")

  # ---- stabilized mean minutes per review (pseudo-count shrinkage) ----
  if (prior_n < 0) stop("prior_n must be >= 0.")
  if (planned_mean <= 0) stop("planned_mean must be > 0.")

  n <- reviews_completed
  S <- elapsed_minutes

  avg_rev_time_emp <- if (n > 0) S / n else NA_real_
  avg_rev_time_est <- if (n == 0) planned_mean else (prior_n * planned_mean + S) / (prior_n + n)

  # ---- parse breaks ----
  if (is.null(break_hhmm) || is.null(break_lens) || length(break_hhmm) == 0) {
    break_starts <- as.POSIXct(character(0), tz = tz)
    break_lens_m <- numeric(0)
  } else {
    stopifnot(length(break_hhmm) == length(break_lens))
    break_starts <- as.POSIXct(vapply(break_hhmm, to_posix_today, FUN.VALUE = as.POSIXct(NA)), tz = tz)
    break_lens_m <- as.numeric(break_lens)

    ord <- order(break_starts)
    break_starts <- break_starts[ord]
    break_lens_m <- break_lens_m[ord]
  }

  # ---- base finish time (no breaks) ----
  work_minutes <- review_remain * avg_rev_time_est
  t_end <- T + work_minutes * 60

  # ---- add overlapping breaks (iteratively) ----
  breaks_added <- 0
  for (i in seq_along(break_starts)) {
    b0 <- break_starts[i]
    b1 <- b0 + break_lens_m[i] * 60
    overlaps <- (T < b1) && (t_end > b0)
    if (overlaps) {
      t_end <- t_end + break_lens_m[i] * 60
      breaks_added <- breaks_added + break_lens_m[i]
    }
  }

  total_minutes_left <- as.numeric(difftime(t_end, T, units = "mins"))

  # ---- console output ----
  if (isTRUE(print_console)) {
    cat("============================================================\n")
    cat("Estimated time to completion\n")
    cat("------------------------------------------------------------\n")
    cat("Now:                ", format(T, "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "")
    cat("Begin:              ", format(begin_time, "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "")
    cat("Completed/Total:    ", reviews_completed, " / ", review_total, "\n", sep = "")
    cat("Remaining:          ", review_remain, "\n", sep = "")
    cat("Elapsed minutes:    ", sprintf("%.2f", elapsed_minutes), "\n", sep = "")
    if (n > 0) {
      cat("Empirical avg:      ", sprintf("%.2f", avg_rev_time_emp), " min/review\n", sep = "")
    } else {
      cat("Empirical avg:      NA (no completed reviews)\n")
    }
    cat("Planned mean:       ", sprintf("%.2f", planned_mean), " min/review\n", sep = "")
    cat("Prior strength:     ", prior_n, " pseudo-reviews\n", sep = "")
    cat("Stabilized avg:     ", sprintf("%.2f", avg_rev_time_est), " min/review\n", sep = "")
    cat("Work minutes:       ", sprintf("%.2f", work_minutes), "\n", sep = "")
    cat("Breaks added:       ", sprintf("%.0f", breaks_added), " min\n", sep = "")
    cat("Estimated completion:", format(t_end, "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "")
    cat("Time to completion:  ", sprintf("%.2f", total_minutes_left), " min\n", sep = "")
    cat("============================================================\n")
  }

  # ---- plot ----
  p <- NULL
  if (isTRUE(plot)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for plot=TRUE. Install it or set plot=FALSE.")
    }

    # breaks dataframe
    breaks_df <- if (length(break_starts) == 0) {
      data.frame(start = as.POSIXct(character(0), tz = tz),
                 end   = as.POSIXct(character(0), tz = tz),
                 len   = numeric(0),
                 overlaps = logical(0))
    } else {
      df <- data.frame(
        start = break_starts,
        end   = break_starts + break_lens_m * 60,
        len   = break_lens_m
      )
      df$overlaps <- (T < df$end) & (t_end > df$start)
      df
    }

    breaks_plot_df <- breaks_df[breaks_df$overlaps, , drop = FALSE]

    xmin <- min(begin_time, T, breaks_df$start, na.rm = TRUE) - 10 * 60
    xmax <- max(t_end, breaks_df$end, na.rm = TRUE) + 10 * 60
    x_center <- xmin + 0.5 * (xmax - xmin)

    label_text <- paste0(
      "Completed: ", reviews_completed, " / ", review_total,
      "   |   Remaining: ", review_remain,
      "\nElapsed: ", sprintf("%.1f", elapsed_minutes), " min",
      "   |   Emp avg: ", if (n > 0) sprintf("%.2f", avg_rev_time_emp) else "NA",
      "   |   Stabilized: ", sprintf("%.2f", avg_rev_time_est),
      "   |   Planned: ", sprintf("%.2f", planned_mean),
      "\nBreaks added: ", sprintf("%.0f", breaks_added), " min",
      "\nFinish at: ", format(t_end, "%H:%M"),
      "   |   Time to completion: ", sprintf("%.1f", total_minutes_left), " min"
    )

    p <- ggplot2::ggplot() +
      # main timeline (includes breaks implicitly via t_end)
      ggplot2::geom_segment(
        ggplot2::aes(x = begin_time, xend = t_end, y = 0, yend = 0),
        linewidth = 2
      ) +
      # highlight break periods ON the same line (no separate boxes)
      ggplot2::geom_segment(
        data = breaks_plot_df,
        ggplot2::aes(x = start, xend = end, y = 0, yend = 0),
        linewidth = 5,
        alpha = 0.35
      ) +
      ggplot2::geom_vline(
        xintercept = as.numeric(T),
        linetype = "dashed",
        linewidth = 0.7
      ) +
      ggplot2::geom_point(ggplot2::aes(x = begin_time, y = 0), size = 3) +
      ggplot2::geom_point(ggplot2::aes(x = T, y = 0), size = 3) +
      ggplot2::geom_point(ggplot2::aes(x = t_end, y = 0), size = 3) +
      ggplot2::annotate("text", x = begin_time, y = 0.06, label = "Begin",  vjust = 0) +
      ggplot2::annotate("text", x = T,          y = 0.06, label = "Now",    vjust = 0, hjust=.2) +
      ggplot2::annotate("text", x = t_end,      y = 0.06, label = "Finish", vjust = 0) +
      ggplot2::annotate(
        "label",
        x = x_center, y = -0.14,
        label = label_text,
        hjust = 0.5, vjust = 0.5,
        label.size = 0.25
      ) +
      ggplot2::scale_x_datetime(limits = c(xmin, xmax), date_labels = "%H:%M") +
      ggplot2::coord_cartesian(ylim = c(-0.20, 0.12)) +
      ggplot2::labs(
        x = "Time (today)",
        y = NULL,
        title = "Estimated time to completion (stabilized mean + scheduled breaks)",
        subtitle = paste0(
          "Prior strength = ", prior_n, " pseudo-reviews | Planned mean = ",
          planned_mean, " min/review"
        )
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank()
      )

    print(p)
  }

  invisible(list(
    review_remain      = review_remain,
    elapsed_minutes    = elapsed_minutes,
    avg_rev_time_emp   = avg_rev_time_emp,
    avg_rev_time_est   = avg_rev_time_est,
    work_minutes       = work_minutes,
    breaks_added_min   = breaks_added,
    T_fin              = t_end,
    total_minutes_left = total_minutes_left,
    plot               = p
  ))
}
