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

# Random-effect summary helpers (extract_added_prob_draws, compute_baseline_prob,
# compute_group_counts, compute_ranks) now live inline in stat_models.qmd, moved
# there for transparency.

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
      # nudge the names up a touch; the x-axis titles get a much bigger bump
      # (size on axis.title.x carries to both the bottom and the secondary top title)
      axis.text.y = ggplot2::element_text(size = 11),
      axis.title.x = ggplot2::element_text(size = 14)
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
