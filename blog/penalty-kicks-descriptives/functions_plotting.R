source("utils_constants.R")

convert_opta_to_meters <- function(data) {
  data |>
    dplyr::mutate(
      # Opta Y: 0 is right touchline, 100 is left. 50 is the center.
      # Assuming a standard 68m pitch width, 1 unit = 0.68m.
      shot_x_meters = (50 - goal_mouth_y) * 0.68,
      # Opta height cross bar confirmed in kloppy code
      # The correct height ratio (2.44m real crossbar / 38 Opta crossbar)
      shot_y_meters = goal_mouth_z * (2.44 / 38)
    )
}

draw_goal_base <- function(
  include_shots_over_bar = FALSE,
  include_shots_wide = FALSE,
  include_axes = FALSE,
  include_grid_lines = FALSE
) {
  # Calculate radius (half diameter) to easily center the posts/crossbar
  r <- diameter_post / 2

  # Set ylim based on whether to include shots over the bar
  if (include_shots_over_bar) {
    y_limit <- 6.5
  } else {
    y_limit <- crossbar_height + 0.05
  }

  if (include_shots_wide) {
    x_limit <- post_offset + 1.5
  } else {
    x_limit <- post_offset + 0.05
  }

  grid_lines <- if (include_grid_lines) {
    list(
      # 3x4 Net Grid (Left, Middle, Right x 4 Height Bins)
      ggplot2::annotate(
        "segment",
        x = c(-1.22, 1.22),
        xend = c(-1.22, 1.22),
        y = 0,
        yend = crossbar_height,
        color = "gray75",
        linetype = "dashed"
      ),
      ggplot2::annotate(
        "segment",
        x = post_left,
        xend = post_right,
        y = c(0.61, 1.22, 1.83),
        yend = c(0.61, 1.22, 1.83),
        color = "gray75",
        linetype = "dashed"
      )
    )
  }

  list(
    grid_lines,
    # Left Post
    ggplot2::annotate(
      "rect",
      xmin = post_left - r,
      xmax = post_left + r,
      ymin = 0,
      ymax = crossbar_height + r,
      fill = "black"
    ),
    # Right Post
    ggplot2::annotate(
      "rect",
      xmin = post_right - r,
      xmax = post_right + r,
      ymin = 0,
      ymax = crossbar_height + r,
      fill = "black"
    ),
    # Crossbar
    ggplot2::annotate(
      "rect",
      xmin = post_left - r,
      xmax = post_right + r,
      ymin = crossbar_height - r,
      ymax = crossbar_height + r,
      fill = "black"
    ),
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.8, color = "darkgreen"),
    ggplot2::coord_fixed(
      ratio = 1,
      xlim = c(-x_limit, x_limit),
      ylim = c(-0.5, y_limit)
    ),
    ggplot2::theme_minimal(),
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = if (include_axes) {
        ggplot2::element_text()
      } else {
        ggplot2::element_blank()
      },
      axis.ticks = if (include_axes) {
        ggplot2::element_line()
      } else {
        ggplot2::element_blank()
      },
      axis.title = if (include_axes) {
        ggplot2::element_text()
      } else {
        ggplot2::element_blank()
      }
    ),
    ggplot2::labs(x = "Width (m)", y = "Height (m)")
  )
}

plot_dive_zones <- function() {
  list(
    ggplot2::annotate(
      "segment",
      x = c(dive_zone_left_offset, dive_zone_right_offset),
      xend = c(dive_zone_left_offset, dive_zone_right_offset),
      y = 0,
      yend = crossbar_height,
      color = "gray50",
      linetype = "dashed",
      linewidth = 0.6
    )
  )
}

plot_shots <- function() {
  list(
    ggforce::geom_circle(
      aes(
        x0 = shot_x_meters,
        y0 = shot_y_meters,
        r = ball_radius,
        fill = outcome
      ),
      linewidth = 0.5,
      n = 30,
      alpha = 0.2
    ),
    scale_fill_manual(
      values = c(
        "Goal" = "#2ecc71",
        "Saved" = "#e74c3c",
        "Post" = "#f39c12",
        "Miss" = "gray40"
      )
    )
  )
}

add_simple_heatmap_layer <- function(
  contour_levels = 10,
  alpha = 0.5,
  bandwidth = NULL
) {
  list(
    ggplot2::stat_density_2d(
      ggplot2::aes(fill = ggplot2::after_stat(level)),
      geom = "polygon",
      alpha = alpha,
      bins = contour_levels,
      h = bandwidth
    ),
    ggplot2::scale_fill_viridis_c(option = "cividis", guide = "none")
  )
}

# --- Random-effect summaries from a brms binary GLMM ----------------------
# Helpers for summarising a single grouping factor in a logit model that has
# a global intercept and `(1 | group)` structure. They standardise the
# downstream column names (`base_prob`, `group_prob`, `added_prob`,
# `n_total`, `n_success`) so plot code is reusable across outcomes.

#' Posterior draws of a random effect's added probability.
#' Returns one row per draw per group level, with `added_prob =
#' plogis(b_Intercept + r_<group>) - plogis(b_Intercept)`.
extract_added_prob_draws <- function(model, group_col) {
  re_param_name <- paste0("r_", group_col)
  re_arg <- rlang::parse_expr(paste0(re_param_name, "[", group_col, ", ]"))
  re_sym <- rlang::sym(re_param_name)

  draws <- rlang::inject(
    tidybayes::spread_draws(model, b_Intercept, !!re_arg)
  )

  draws |>
    dplyr::mutate(
      base_prob = plogis(b_Intercept),
      group_prob = plogis(b_Intercept + !!re_sym),
      added_prob = group_prob - base_prob
    )
}

#' Median of `plogis(b_Intercept)` over the posterior — a single
#' representative baseline probability for plot annotations.
compute_baseline_prob <- function(draws) {
  draws |>
    dplyr::ungroup() |>
    dplyr::distinct(.draw, b_Intercept) |>
    dplyr::summarise(p = stats::median(plogis(b_Intercept))) |>
    dplyr::pull(p)
}

#' Per-group counts from the model frame (so levels match brms' R-name
#' format and any rows brms dropped don't poison the sums). Returns
#' `<group_col>`, `n_total`, `n_success`, with spaces in the group name
#' replaced by dots to match the random-effect names.
compute_group_counts <- function(model_data, group_col, outcome_col) {
  model_data |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      n_success = sum(.data[[outcome_col]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(group_col),
      \(x) stringr::str_replace_all(x, " ", "\\.")
    ))
}

#' Summary stats (median/mean/p5/p95 of added_prob) joined with counts,
#' plus a display label. `sort_by` selects which statistic the rows are
#' arranged by; `desc` chooses the direction.
compute_ranks <- function(
  draws,
  group_col,
  counts_df,
  sort_by = "median_added",
  desc = TRUE
) {
  ranks <- draws |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      median_added = stats::median(added_prob),
      mean_added = mean(added_prob),
      p5_added = stats::quantile(added_prob, probs = 0.05, names = FALSE),
      p95_added = stats::quantile(added_prob, probs = 0.95, names = FALSE),
      .groups = "drop"
    ) |>
    dplyr::left_join(counts_df, by = group_col) |>
    dplyr::mutate(
      label = sprintf(
        "%s (n = %d/%d, prop = %.0f%%)",
        stringr::str_replace_all(.data[[group_col]], "\\.", " "),
        n_success,
        n_total,
        100 * n_success / n_total
      )
    )

  sort_sym <- rlang::sym(sort_by)
  if (desc) {
    dplyr::arrange(ranks, dplyr::desc(!!sort_sym))
  } else {
    dplyr::arrange(ranks, !!sort_sym)
  }
}

#' Half-eye plot of added probability for a chosen subset of groups.
#' `sort_by` is the column used to order the y-axis (must exist in
#' `ranks_df`); `desc=TRUE` puts the largest value at the bottom.
plot_re <- function(
  draws,
  names_in,
  ranks_df,
  name_col,
  x_label,
  title,
  baseline_prob,
  sec_axis_name = "Absolute probability",
  sort_by = "median_added",
  desc = FALSE
) {
  re_scale <- ggplot2::scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    sec.axis = ggplot2::sec_axis(
      ~ . + baseline_prob,
      name = sec_axis_name,
      breaks = function(x) {
        b <- scales::breaks_pretty()(x)
        sort(c(b[abs(b - baseline_prob) > 0.05], baseline_prob))
      },
      labels = scales::percent_format(accuracy = 1)
    )
  )

  sort_sym <- rlang::sym(sort_by)

  draws |>
    dplyr::filter(.data[[name_col]] %in% names_in) |>
    dplyr::left_join(ranks_df, by = name_col) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label = forcats::fct_reorder(label, !!sort_sym, .desc = desc)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = added_prob, y = label)) +
    ggdist::stat_halfeye() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    re_scale +
    ggplot2::labs(x = x_label, y = NULL, title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      axis.title.x = ggplot2::element_text(size = 8)
    )
}

#' Side-by-side patchwork of the top-N and bottom-N rows of `ranks_df`.
#' `subject` is the plain noun for the title (e.g. "takers"). `desc_top`
#' and `desc_bottom` control the y-axis sort direction in each panel.
plot_re_top_bottom <- function(
  draws,
  ranks_df,
  name_col,
  x_label,
  subject,
  baseline_prob,
  n = 25,
  sec_axis_name = "Absolute probability",
  sort_by = "median_added",
  desc_top = FALSE,
  desc_bottom = TRUE
) {
  best <- dplyr::slice_head(ranks_df, n = n)[[name_col]]
  worst <- dplyr::slice_tail(ranks_df, n = n)[[name_col]]

  p_top <- plot_re(
    draws, best, ranks_df, name_col, x_label,
    sprintf("Top %d %s", n, subject),
    baseline_prob = baseline_prob,
    sec_axis_name = sec_axis_name,
    sort_by = sort_by,
    desc = desc_top
  )
  p_bottom <- plot_re(
    draws, worst, ranks_df, name_col, x_label,
    sprintf("Bottom %d %s", n, subject),
    baseline_prob = baseline_prob,
    sec_axis_name = sec_axis_name,
    sort_by = sort_by,
    desc = desc_bottom
  )

  patchwork::wrap_plots(p_top, p_bottom)
}
