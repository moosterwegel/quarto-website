source("utils_constants.R")
source("functions_parsing_other_data.R")

#' Determine which zone the shot landed in based on dive zone boundaries.
#' Returns "Left", "Centre", or "Right".
classify_shot_zone <- function(shot_x) {
  dplyr::case_when(
    is.na(shot_x) ~ NA_character_,
    shot_x < dive_zone_left_offset ~ "Left",
    shot_x > dive_zone_right_offset ~ "Right",
    TRUE ~ "Centre"
  )
}

#' Add derived feature columns to the penalty dataframe.
#' Expects the dataframe to already have shot_x_meters (via convert_opta_to_meters).
add_features <- function(data) {
  data |>
    dplyr::mutate(
      match_date = lubridate::as_date(match_date),
      is_female_league = stringr::str_detect(
        competition,
        stringr::regex("women", ignore_case = TRUE)
      ),
      competition_type = dplyr::case_when(
        stringr::str_detect(competition, "^INT") ~ "international country",
        competition ==
          "EUR-European Championship Qualification" ~ "international country",
        stringr::str_detect(competition, "^EUR") ~ "international club",
        stringr::str_detect(
          competition,
          stringr::regex("cup", ignore_case = TRUE)
        ) ~ "cup",
        TRUE ~ "league"
      ),
      competition_type_detailed = dplyr::case_when(
        competition_type != "league" ~ competition_type,
        competition %in%
          c(
            "ENG-Premier League",
            "ESP-La Liga",
            "FRA-Ligue 1",
            "GER-Bundesliga",
            "ITA-Serie A"
          ) ~ "top 5 league",
        competition %in%
          c(
            "ENG-Championship",
            "GER-2. Bundesliga",
            "ENG-League One",
            "ENG-League Two"
          ) ~ "non top level league",
        competition %in%
          c(
            "BEL-Jupiler Pro League",
            "NED-Eredivisie",
            "POR-Liga Portugal",
            "RUS-Premier League",
            "SCO-Premiership",
            "TUR-Super Lig"
          ) ~ "other European league",
        .default = "league outside Europe"
      ),
      shot_zone = classify_shot_zone(shot_x_meters),
      shot_zone_dominance = dplyr::case_when(
        is.na(shot_zone) | is.na(kick_foot) ~ NA_character_,
        shot_zone == "Centre" ~ "Centre",
        kick_foot == "Right" & shot_zone == "Left" ~ "Dominant",
        kick_foot == "Left" & shot_zone == "Right" ~ "Dominant",
        kick_foot == "Right" & shot_zone == "Right" ~ "Non_dominant",
        kick_foot == "Left" & shot_zone == "Left" ~ "Non_dominant",
        TRUE ~ NA_character_
      ),
      # Keeper action from the shooter's perspective (flip left/right)
      gk_action = dplyr::case_when(
        gk_movement == "Standing" ~ "Standing",
        gk_dive_direction == "Left" ~ "Dived Right",
        gk_dive_direction == "Right" ~ "Dived Left",
        TRUE ~ NA_character_
      ),
      gk_dive_zone = dplyr::case_when(
        gk_action == "Dived Left" ~ "Left",
        gk_action == "Dived Right" ~ "Right",
        gk_action == "Standing" ~ "Centre",
        TRUE ~ NA_character_
      ),
      is_goal = outcome == "Goal",
      score_diff_taking_team_coarse = factor(
        dplyr::case_when(
          is.na(score_diff_taking_team) ~ NA_character_,
          score_diff_taking_team <= -3 ~ "losing_3_plus",
          score_diff_taking_team == -2 ~ "losing_2",
          score_diff_taking_team == -1 ~ "losing_1",
          score_diff_taking_team == 0 ~ "equal",
          score_diff_taking_team == 1 ~ "winning_1",
          score_diff_taking_team == 2 ~ "winning_2",
          score_diff_taking_team >= 3 ~ "winning_3_plus"
        ),
        levels = c(
          "losing_3_plus",
          "losing_2",
          "losing_1",
          "equal",
          "winning_1",
          "winning_2",
          "winning_3_plus"
        )
      ),
      taker_position_binned = dplyr::case_when(
        is.na(taker_position) ~ NA_character_,
        taker_position == "Sub" ~ "Sub",
        TRUE ~ substr(taker_position, 1, 1)
      ),
      # Time variables
      ## Within-half "regular" minute, capped at the regulation length of the
      ## current period (45 for first/second half, 15 for extra-time halves).
      ## NA for shoot-outs and any unrecognised period.
      regular_minute_in_half = dplyr::case_when(
        period == "FirstHalf" ~ pmin(minute, 45L),
        period == "SecondHalf" ~ pmin(minute - 45L, 45L),
        period == "FirstPeriodOfExtraTime" ~ pmin(minute - 90L, 15L),
        period == "SecondPeriodOfExtraTime" ~ pmin(minute - 105L, 15L),
        TRUE ~ NA_integer_
      ),
      ## Minutes into stoppage time within the current half. NA when the kick
      ## is in regular time, in a shoot-out, or in an unrecognised period.
      minute_of_added_time = dplyr::case_when(
        period == "FirstHalf" & minute > 45L ~ minute - 45L,
        period == "SecondHalf" & minute > 90L ~ minute - 90L,
        period == "FirstPeriodOfExtraTime" & minute > 105L ~ minute - 105L,
        period == "SecondPeriodOfExtraTime" & minute > 120L ~ minute - 120L,
        TRUE ~ NA_integer_
      ),
      ## Total minutes elapsed in the half, including added time.
      minute_in_half = regular_minute_in_half +
        dplyr::coalesce(minute_of_added_time, 0L),
      gk_correct = dplyr::case_when(
        is.na(shot_x_meters) | is.na(gk_action) ~ NA,
        shot_zone == gk_dive_zone ~ TRUE,
        TRUE ~ FALSE
      ),
      gk_wrong_way = !gk_correct,
      time_since_start = lubridate::minutes(expanded_minute) +
        lubridate::seconds(second),
      ## Strictly past the 90:00 mark in the second half (e.g. 90:30 is TRUE,
      ## 90:00 is FALSE). Extra time and shoot-outs are not counted as added time.
      is_second_half_added_time = period == "SecondHalf" &
        time_since_start > lubridate::minutes(90),
    ) |>
    # Shootouts
    ## Per-match shootout outcome: which side won and whether the taker's team won.
    dplyr::group_by(match_id) |>
    # Tie-break on the kick sequence: ~half of shootouts share a timestamp
    # across kicks, and every cumulative shootout feature below (running score,
    # match points, final score) depends on this being the true kick order.
    dplyr::arrange(time_since_start, shootout_seq_total) |>
    dplyr::mutate(
      shootout_home_goals = sum(
        is_shootout & taking_team_ha == "home" & is_goal,
        na.rm = TRUE
      ),
      shootout_away_goals = sum(
        is_shootout & taking_team_ha == "away" & is_goal,
        na.rm = TRUE
      ),
      shootout_winner = dplyr::case_when(
        shootout_home_goals > shootout_away_goals ~ "home",
        shootout_away_goals > shootout_home_goals ~ "away",
        TRUE ~ NA_character_
      ),
      shootout_taker_won = taking_team_ha == shootout_winner,
      shootout_taker_team_kicked_first = dplyr::if_else(
        is_shootout,
        taking_team_ha == taking_team_ha[which(is_shootout)[1]],
        NA
      ),
      ## Running shootout state via cumulative sums (within match, in time order).
      shootout_home_kick = is_shootout & taking_team_ha == "home",
      shootout_away_kick = is_shootout & taking_team_ha == "away",
      shootout_home_goals_after = cumsum(
        dplyr::coalesce(shootout_home_kick & is_goal, FALSE)
      ),
      shootout_away_goals_after = cumsum(
        dplyr::coalesce(shootout_away_kick & is_goal, FALSE)
      ),
      shootout_home_kicks_after = cumsum(dplyr::coalesce(
        shootout_home_kick,
        FALSE
      )),
      shootout_away_kicks_after = cumsum(dplyr::coalesce(
        shootout_away_kick,
        FALSE
      )),
      ## Score / kicks-taken going INTO this kick (i.e. before it has been taken).
      shootout_taker_goals_before = dplyr::case_when(
        !is_shootout ~ NA_integer_,
        taking_team_ha == "home" ~ as.integer(
          shootout_home_goals_after - is_goal
        ),
        taking_team_ha == "away" ~ as.integer(
          shootout_away_goals_after - is_goal
        )
      ),
      shootout_opp_goals_before = dplyr::case_when(
        !is_shootout ~ NA_integer_,
        taking_team_ha == "home" ~ as.integer(shootout_away_goals_after),
        taking_team_ha == "away" ~ as.integer(shootout_home_goals_after)
      ),
      shootout_taker_kicks_before = dplyr::case_when(
        !is_shootout ~ NA_integer_,
        taking_team_ha == "home" ~ as.integer(shootout_home_kicks_after - 1L),
        taking_team_ha == "away" ~ as.integer(shootout_away_kicks_after - 1L)
      ),
      shootout_opp_kicks_before = dplyr::case_when(
        !is_shootout ~ NA_integer_,
        taking_team_ha == "home" ~ as.integer(shootout_away_kicks_after),
        taking_team_ha == "away" ~ as.integer(shootout_home_kicks_after)
      ),
      ## Match point: at least one outcome of THIS kick (goal or miss) mathematically
      ## ends the shootout. Works for both regulation and sudden death:
      ## - regulation (≤ 5 kicks each): trailing side can no longer catch up
      ## - sudden death  (> 5 kicks each): the round completes with scores unequal
      shootout_is_match_point = dplyr::if_else(
        is_shootout,
        {
          a_b <- shootout_taker_goals_before
          b_b <- shootout_opp_goals_before
          ka <- shootout_taker_kicks_before + 1L # taker count AFTER this kick
          kb <- shootout_opp_kicks_before # opp count is unchanged
          in_reg <- ka <= 5L & kb <= 5L
          # outcome = goal
          a_g <- a_b + 1L
          reg_g <- (a_g > b_b + pmax(5L - kb, 0L)) |
            (b_b > a_g + pmax(5L - ka, 0L))
          sd_g <- (ka == kb) & (a_g != b_b)
          ended_g <- dplyr::if_else(in_reg, reg_g, sd_g)
          # outcome = miss
          a_m <- a_b
          reg_m <- (a_m > b_b + pmax(5L - kb, 0L)) |
            (b_b > a_m + pmax(5L - ka, 0L))
          sd_m <- (ka == kb) & (a_m != b_b)
          ended_m <- dplyr::if_else(in_reg, reg_m, sd_m)
          ended_g | ended_m
        },
        NA
      ),
      ## Match-point counts within the shootout.
      shootout_match_points_before = dplyr::if_else(
        is_shootout,
        cumsum(dplyr::coalesce(shootout_is_match_point, FALSE)) -
          dplyr::coalesce(as.integer(shootout_is_match_point), 0L),
        NA_integer_
      ),
      shootout_match_points_total = dplyr::if_else(
        is_shootout,
        as.integer(sum(shootout_is_match_point, na.rm = TRUE)),
        NA_integer_
      )
    ) |>
    dplyr::ungroup()
}
