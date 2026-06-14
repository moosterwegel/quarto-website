get_penalty_lineups <- function() {
  nanoparquet::read_parquet(
    "data/penalties_all_seasons_lineups.parquet"
  ) |>
    dplyr::filter(is_taking_team) |>
    dplyr::select(
      match_id,
      event_id,
      player_id,
      player_name,
      position,
      is_taker
    )
}
