get_penalty_lineups <- function() {
  nanoparquet::read_parquet(
    "data/penalty_lineups_ws.parquet"
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
