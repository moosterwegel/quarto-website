get_df_most_common_position <- function() {
  nanoparquet::read_parquet(
    "data/lineups_all_seasons.parquet"
  ) |>
    dplyr::group_by(player_id, position) |>
    dplyr::tally() |>
    dplyr::mutate(
      total_appearances = sum(n),
      proportion_appearance_position = n / total_appearances
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(position != "Sub") |>
    dplyr::group_by(player_id) |>
    dplyr::mutate(
      total_appearances_non_sub = sum(n),
      proportion_appearance_position_non_sub = n / total_appearances_non_sub
    ) |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(
      player_id,
      most_common_start_position = position,
      proportion_start_appearances_position = proportion_appearance_position_non_sub
    )
}
