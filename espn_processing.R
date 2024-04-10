# ESPN processing

# PBP columns
pbp_cols <- c("game_id", "season", "home_team_spread",
              "play_id" = "id",
              "half" = "period_number",
              "home_id" = "home_team_id", "home" = "home_team_name",
              "away_id" = "away_team_id", "away" = "away_team_name",
              "home_score", "away_score",
              "score_diff", "team_id", "event_team",
              "game_secs_remaining", "half_secs_remaining", "play_length",
              "desc" = "text", "shot_outcome", "points" = "score_value",
              "free_throw", "three_pt", "shot_desc" = "type_text",
              "loc_x" = "coordinate_x_raw", "loc_y" = "coordinate_y_raw",
              "shooter_id" = "athlete_id_1", "assist_id" = "athlete_id_2",
              "poss_before", "poss_after", "poss_number")

# Schedule columns
schedule_cols <- c("season", "game_id", "game_date" = "date", "hour",
                   "neutral" = "neutral_site", "postseason" = "season_type",
                   "venue_id", "city" = "venue_address_city", "state" = "venue_address_state",
                   "home_id", "home" = "home_location",
                   "away_id", "away" = "away_location",
                   "home_conference_id", "away_conference_id",
                   "home_score", "away_score", "score_diff")

# Process ESPN pbp
process_espn_pbp <- function(pbp, womens = FALSE) {

  # Convert to dataframe
  pbp <- as.data.frame(pbp)
  pbp$row <- 1L:nrow(pbp)

  # Distinct events
  fts <- collapse::fsubset(pbp, type_id %in% c(540L))
  non_fts <- dplyr::distinct(collapse::fsubset(pbp, !type_id %in% c(540L)), game_id, period_number, clock_display_value, text, .keep_all = TRUE)

  pbp <- collapse::roworder(collapse::rowbind(fts, non_fts), row)

  # Fix team Ids
  pbp <- pbp %>%
    dplyr::group_by(game_id) %>%
    dplyr::mutate(home_points = home_score - dplyr::lag(home_score, default = 0L),
                  away_points = away_score - dplyr::lag(away_score, default = 0L),
                  lag_type_id = dplyr::lag(type_id)) %>%
    dplyr::ungroup() %>%
    # If home scored, make sure team_id = home_team_id/If away scored, make sure team_id = away_team_id
    # (if previous play was end of half/game then don't change)
    collapse::fmutate(team_id = ifelse(lag_type_id %in% c(402L, 412L), team_id, ifelse(home_points > 0L & home_points <= score_value, home_team_id, ifelse(away_points > 0L & away_points <= score_value, away_team_id, team_id))))

  # Event team
  pbp$event_team <- ifelse(pbp$team_id == pbp$home_team_id, pbp$home_team_name, pbp$away_team_name)
  # Convert to integer
  pbp$team_id <- as.integer(pbp$team_id)
  pbp$period_number <- as.integer(pbp$period_number)
  pbp$clock_seconds <- as.integer(pbp$clock_seconds)
  pbp$clock_minutes <- as.integer(pbp$clock_minutes)

  pbp$type_text <- stringr::str_remove_all(pbp$type_text, " ")
  ## Neutral court flag
  # pbp$neutral <- as.integer(pbp$season_type == 3L)
  # Home score differential
  pbp$score_diff <- pbp$home_score - pbp$away_score

  # Time remaining
  if (isTRUE(womens)) {
    pbp$qtr_secs_remaining <- (pbp$clock_minutes*60) + pbp$clock_seconds
    pbp$half_secs_remaining <- ifelse(pbp$qtr %in% c(1L, 3L), 600L, 0L) + pbp$qtr_secs_remaining
    pbp$game_secs_remaining <- (4-pbp$qtr)*10*60 + pbp$qtr_secs_remaining
    # Fix for OT
    pbp$game_secs_remaining <- ifelse(!pbp$qtr %in% c(1L, 2L, 3L, 4L),
                                      (pbp$clock_minutes*60) + pbp$clock_seconds,
                                      pbp$game_secs_remaining)

    # Get rid of bad feeds if possible
    pbp <- dplyr::distinct(pbp, game_id, period, home_score, away_score, game_secs_remaining, text, .keep_all = TRUE)
  } else {
    pbp$half_secs_remaining <- (pbp$clock_minutes*60) + pbp$clock_seconds
    pbp$game_secs_remaining <- (2-pbp$period_number)*20*60+ (pbp$clock_minutes*60) + pbp$clock_seconds
    # Fix for OT
    pbp$game_secs_remaining <- ifelse(!pbp$period_number %in% c(1, 2),
                                      (pbp$clock_minutes*60) + pbp$clock_seconds,
                                      pbp$game_secs_remaining)

    # Get rid of bad feeds if possible
    pbp <- dplyr::distinct(pbp, game_id, period_number, home_score, away_score, game_secs_remaining, text, .keep_all = TRUE)
  }

  # Play length
  pbp <- pbp %>%
    dplyr::mutate(play_length = pmax(0L, dplyr::lag(.data$game_secs_remaining) - .data$game_secs_remaining, na.rm = TRUE))

  # Stoppage flag
  pbp$stoppage <- as.integer(pbp$type_text %in% c("EndGame", "EndPeriod", "OfficialTVTimeOut", "ShortTimeOut", "RegularTimeOut"))

  # Shot location cleaning
  pbp$to_na <- as.integer(pbp$coordinate_x_raw == 25L & (pbp$coordinate_y_raw == 0L | pbp$coordinate_y_raw < -5L) | pbp$stoppage == 1L)
  pbp$coordinate_x_raw <- ifelse(pbp$to_na == 1L, NA_integer_, pbp$coordinate_x_raw)
  pbp$coordinate_y_raw <- ifelse(pbp$to_na == 1L, NA_integer_, pbp$coordinate_y_raw)

  # Free throw flag
  pbp$free_throw <- as.logical(stringr::str_detect(pbp$text, "(F|f)ree (T|t)hrow"))
  # Three point flag
  pbp$three_pt <- as.logical(stringr::str_detect(pbp$text, "(T|t)hree (P|p)oint"))

  # Fix not available text
  pbp$type_text <- dplyr::case_when(
    pbp$type_text == "NotAvailable" & pbp$shooting_play & stringr::str_detect(pbp$text, "Three Point Jumper") ~ "ThreePointJumpShot",
    pbp$type_text == "NotAvailable" & pbp$shooting_play & stringr::str_detect(pbp$text, "Block") ~ "BlockShot",
    pbp$type_text == "NotAvailable" & pbp$shooting_play & stringr::str_detect(pbp$text, "Dunk") ~ "DunkShot",
    pbp$type_text == "NotAvailable" & pbp$shooting_play & stringr::str_detect(pbp$text, "Layup") ~ "Layup",
    pbp$type_text == "NotAvailable" & pbp$shooting_play & stringr::str_detect(pbp$text, "Tip") ~ "TipShot",
    pbp$type_text == "NotAvailable" & pbp$shooting_play & stringr::str_detect(pbp$text, "(Jumper)|(missed)|(made)") ~ "JumpShot",
    pbp$type_text == "NotAvailable" & !pbp$shooting_play & stringr::str_detect(pbp$text, "Turnover") ~ "LostBallTurnover",
    pbp$type_text == "NotAvailable" & !pbp$shooting_play & stringr::str_detect(pbp$text, "Foul") ~ "PersonalFoul",
    pbp$type_text == "NotAvailable" & !pbp$shooting_play & stringr::str_detect(pbp$text, "Timeout") ~ "RegularTimeOut",
    TRUE ~ pbp$type_text
  )

  # Shot outcome flag
  pbp$shot_outcome <- dplyr::case_when(
    !pbp$type_text %in% c("BlockShot", "DunkShot", "JumpShot", "LayUpShot", "ThreePointJumpShot", "TipShot") & !pbp$free_throw & !pbp$shooting_play ~ NA_character_,
    (stringr::str_detect(pbp$text, "(M|m)issed") | stringr::str_detect(pbp$text, "(B|b)lock") & !pbp$scoring_play) ~ "missed",
    stringr::str_detect(pbp$text, "(M|m)ade") | pbp$scoring_play ~ "made",
    TRUE ~ NA_character_
  )

  # Fix shot outcome for blocks (when previous play is the shot attempt that is blocked, make row with block not be a shot)
  pbp <- pbp %>%
    dplyr::mutate(shot_outcome = ifelse(.data$type_text == "BlockShot" &
                                          ((dplyr::lag(.data$shot_outcome) == "missed" & dplyr::lag(.data$team_id) != .data$team_id) |
                                             (dplyr::lag(.data$shot_outcome, n = 2L) == "missed" & dplyr::lag(.data$team_id, n = 2L) != .data$team_id)),
                                        NA_character_, .data$shot_outcome))

  # Fix score value
  pbp$score_value <- dplyr::if_else(pbp$shot_outcome == "missed", 0L, pbp$score_value, missing = 0L)

  n <- nrow(pbp)

  non_stoppage <- pbp %>%
    # Filter out all stoppages
    collapse::fsubset(stoppage == 0L) %>%
    # Indicators for various plays
    dplyr::mutate(opp_team_id = ifelse(home_team_id == team_id, away_team_id, home_team_id),
                  opp_team = ifelse(home_team_id == team_id, away_team_name, home_team_name),
                  d_reb = as.integer(.data$type_text == "DefensiveRebound"),
                  o_reb = as.integer(.data$type_text == "OffensiveRebound"),
                  turnover = as.integer(.data$type_text == "LostBallTurnover"),
                  steal = as.integer(.data$type_text == "Steal"),
                  block = as.integer(.data$type_text == "BlockShot"),
                  off_foul = as.integer(.data$type_text == "PersonalFoul" &
                                          dplyr::lead(.data$type_text == "LostBallTurnover") &
                                          dplyr::lead(.data$play_length == 0) &
                                          dplyr::lead(.data$team_id) == .data$team_id),
                  shooting_foul = as.integer(.data$type_text == "PersonalFoul" &
                                               dplyr::lead(.data$free_throw == TRUE) &
                                               dplyr::lead(.data$team_id) != .data$team_id),
                  personal_foul = as.integer(.data$type_text == "PersonalFoul" &
                                               .data$shooting_foul == 0L),
                  technical_foul = as.integer(.data$type_text == "TechnicalFoul"),
                  front_end = as.integer(.data$free_throw == TRUE &
                                           .data$event_team == dplyr::lead(.data$event_team) &
                                           (dplyr::lead(.data$free_throw == TRUE) |
                                              dplyr::lead(.data$free_throw == TRUE, n = 2))),
                  one_and_one = as.integer(stringr::str_detect(.data$text, stringr::fixed("1st of 1-and-1"))),
                  technical_ft = as.integer(stringr::str_detect(.data$text, stringr::fixed("free throw technical"))),
                  poss_before = dplyr::case_when(
                    # Steals/blocks mean possession is with `event_team`
                    # steals/blocks are coded as having possession of the team that
                    # steals/blocks the ball
                    .data$steal + .data$block >= 1L ~ .data$opp_team,
                    # Turnover means possession is with `event_team`
                    .data$turnover == 1L ~ .data$event_team,
                    # Offensive foul means possession is with `event_team`
                    .data$off_foul == 1L ~ .data$event_team,
                    # Shooting or personal fouls mean possession is the other team
                    .data$shooting_foul + .data$personal_foul >= 1 & .data$event_team == .data$away_team_name ~ .data$home_team_name,
                    .data$shooting_foul + .data$personal_foul >= 1 & .data$event_team == .data$home_team_name ~ .data$away_team_name,
                    # Offensive rebounds means possession is with `event_team`
                    .data$o_reb == 1L ~ .data$event_team,
                    # Defensive rebounds means possession was with the other team
                    .data$d_reb == 1L & .data$event_team == .data$away_team_name ~ .data$home_team_name,
                    .data$d_reb == 1L & .data$event_team == .data$home_team_name ~ .data$away_team_name,
                    # otherwise, possession before is the `event_team`
                    !is.na(.data$shot_outcome) ~ .data$event_team,
                    TRUE ~ NA_character_),
                  poss_after = dplyr::case_when(
                    # If a turnover and the next row is a steal, keep possession with turnover team, next row will change possession
                    .data$turnover == 1L & dplyr::lead(.data$steal == 1L) ~ .data$event_team,
                    # An offensive rebound with the next `poss_before` being the other team
                    # means the offensive rebound led to nothing and should have a change of possession
                    .data$o_reb == 1L & dplyr::lead(.data$poss_before) != .data$event_team ~ dplyr::lead(.data$poss_before),
                    # If a missed shot and the next row says it was blocked, keep with same team
                    .data$shot_outcome == "missed" & dplyr::lead(.data$block == 1L) ~ .data$event_team,
                    # If a blocked shot, keep possession with same team (which is opponent team)
                    .data$block == 1L ~ .data$opp_team,
                    # Offensive and defensive rebounds mean possession is with `event_team`
                    .data$d_reb == 1L ~ .data$event_team,
                    .data$o_reb == 1L ~ .data$event_team,
                    # Free throw and next possession is not same team, change to other team
                    .data$free_throw == TRUE & dplyr::lead(.data$poss_before) != .data$poss_before ~ .data$opp_team,
                    # All technical free throws keep possession
                    .data$technical_ft == 1L ~ .data$poss_before,
                    # Turnover means possession changes teams
                    .data$turnover == 1L & .data$event_team == .data$away_team_name ~ .data$home_team_name,
                    .data$turnover == 1L & .data$event_team == .data$home_team_name ~ .data$away_team_name,
                    # An offensive foul followed by turnover by same team keeps possession
                    # the next row (turnover row) will change the possession
                    .data$off_foul == 1L & .data$event_team == dplyr::lead(.data$event_team) &
                      dplyr::lead(.data$turnover == 1L) ~ .data$event_team,
                    # Offensive foul means possession changes teams
                    .data$off_foul == 1L & .data$event_team == .data$away_team_name ~ .data$home_team_name,
                    .data$off_foul == 1L & .data$event_team == .data$home_team_name ~ .data$away_team_name,
                    # Shooting or personal fouls mean possession stays with the other team
                    .data$shooting_foul + .data$personal_foul >= 1 ~ .data$poss_before,
                    # And-one's keep possession
                    .data$shot_outcome == "made" & dplyr::lead(.data$shooting_foul == 1L) & .data$event_team != dplyr::lead(.data$event_team) ~ .data$event_team,
                    # Front ends of free throw's keep possession
                    .data$front_end == 1L ~ .data$poss_before,
                    # Made front ends of 1-and-1 free throw's keep possession
                    .data$one_and_one == 1L & shot_outcome == "made" ~ .data$poss_before,
                    # Made fga's that are not and-one's change possession
                    .data$shot_outcome == "made" & .data$event_team == .data$away_team_name ~ .data$home_team_name,
                    .data$shot_outcome == "made" & .data$event_team == .data$home_team_name ~ .data$away_team_name,
                    # otherwise, change to the next `poss_before`
                    # poor logic for finding the next non-NA `poss_before`
                    dplyr::lead(is.na(.data$poss_before)) ~ dplyr::lead(.data$poss_before, n = 2L),
                    TRUE ~ dplyr::lead(.data$poss_before)),
                  # If there's a technical or flagrant foul or deadball team rebound, `poss_before` is the previous `poss_after`
                  poss_before = dplyr::case_when(
                    .data$type_text == "DeadBallRebound" ~ dplyr::lag(.data$poss_after),
                    .data$technical_foul == 1L ~ dplyr::lag(.data$poss_after),
                    TRUE ~ .data$poss_before),
                  # Indicator for when a possession changes teams
                  poss_change = dplyr::if_else(.data$poss_before != .data$poss_after,
                                               1L, 0L, missing = 0L))

  pbp <- dplyr::left_join(pbp, non_stoppage[, c("id", "game_id", "sequence_number", "poss_before", "poss_after", "poss_change")],
                          by = c("id", "sequence_number", "game_id"))

  assertthat::are_equal(n, nrow(pbp))

  pbp <- pbp %>%
    # Possession change
    dplyr::mutate(poss_change = tidyr::replace_na(.data$poss_change, 0L),
                  poss_change = ifelse(dplyr::lead(.data$type_text == "EndPeriod"), 1L, .data$poss_change)) %>%
    dplyr::group_by(game_id) %>%
    # Add in possession number
    dplyr::mutate(poss_number = dplyr::lag(cumsum(.data$poss_change)) + 1L,
                  poss_number = dplyr::case_when(
                    .data$poss_number == 0 ~ 1L,
                    .data$type_text == "EndGame" ~ NA_integer_,
                    TRUE ~ .data$poss_number),
                  # Make end of games have last possession
                  dplyr::across(c(poss_before, poss_after),
                                ~ ifelse(.data$type_text == "EndGame",
                                         dplyr::last(stats::na.omit(.data$poss_after)),
                                         .)),
                  # Make first row of game be poss number 1
                  poss_number = ifelse(dplyr::row_number() == 1L & is.na(.data$poss_number), 1L, .data$poss_number)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("poss_"),
                                ~ ifelse(.data$stoppage == 1L & !.data$type_text %in% c("EndGame", "EndPeriod"),
                                         NA_character_,
                                         .)))

  if (isTRUE(womens)) {
    pbp_cols <- c(pbp_cols, "qtr", "qtr_secs_remaining")
  }

  return(dplyr::select(pbp, dplyr::all_of(pbp_cols)))
}

# Process ESPN schedule
process_espn_schedule <- function(schedule) {

  # Convert to dataframe
  schedule <- as.data.frame(schedule)

  # Convert to EST date
  schedule$date <- lubridate::ymd_hm(schedule$date)
  schedule$date <- as.POSIXct(schedule$date, tz = "UTC", format = "%d/%m/%Y %H:%M:%S")
  schedule$date <- format(schedule$date, tz = "EST", usetz = TRUE)
  schedule$hour <- lubridate::hour(schedule$date) + lubridate::minute(schedule$date)/60
  schedule$date <- as.Date(substr(schedule$date, start = 1, stop = 10))

  # Get rid of empty strings
  schedule$venue_address_state <- ifelse(schedule$venue_address_state == "", NA_character_, schedule$venue_address_state)
  schedule$venue_address_city <- ifelse(schedule$venue_address_city == "", NA_character_, schedule$venue_address_city)

  # Venue manual fixes
  schedule$venue_id <- dplyr::case_when(
    # 2019 Fl Sunshine Classic
    schedule$game_id %in% c(401176884, 401171201) ~ 4871L,
    # 2016 Stillman Florida A&M
    schedule$game_id == 400922549 ~ 1925L,
    # Hawaii games
    schedule$game_id %in% c(400914518, 401180094) ~ 540L,
    # UPenn vs Chaminade 2020
    schedule$game_id == 400999083 ~ 2077L,
    # NIT Final 2022
    schedule$game_id == 401415936 ~ 1996L,
    # Johnson & Wales (NC)
    schedule$game_id == 400999084 ~ 7342L,
    TRUE ~ as.integer(schedule$venue_id)
  )

  cat("Missing venues:", sum(is.na(schedule$venue_id)), "..\n")

  # Make integer instead of character/logical
  for (col in c("neutral_site", "venue_id", "home_id", "away_id",
                "home_conference_id", "away_conference_id", "home_score", "away_score")) {
    schedule[[col]] <- as.integer(schedule[[col]])
  }

  # Get rid of placeholder 0 scores
  for (col in c("home_score", "away_score")) {
    schedule[[col]] <- ifelse(schedule[[col]] == 0L, NA_integer_, schedule[[col]])
  }

  # Score manual fixes
  schedule$home_score <- dplyr::case_when(
    # 2018 Hampton vs Morgan State
    schedule$game_id == 400992789 ~ 57L,
    TRUE ~ as.integer(schedule$home_score)
  )
  schedule$away_score <- dplyr::case_when(
    # 2018 Hampton vs Morgan State
    schedule$game_id == 400992789 ~ 38L,
    TRUE ~ as.integer(schedule$away_score)
  )

  # Score difference
  schedule$score_diff <- schedule$home_score - schedule$away_score

  # Postseason
  schedule$season_type <- as.integer(schedule$season_type == 3L)

  # Order by date
  schedule <- collapse::roworder(schedule, date)

  schedule <- collapse::fsubset(schedule, home_id > 0L & away_id > 0L & status_type_name %in% c("STATUS_FINAL", "STATUS_SCHEDULED"))

  return(dplyr::select(schedule, dplyr::all_of(schedule_cols)))
}

# Process possessions
process_possessions <- function(pbp) {

  possessions <- pbp %>%
    collapse::fsubset(!is.na(poss_number) & !is.na(poss_before)) %>%
    tidyr::replace_na(list(shot_outcome = "NA")) %>%
    dplyr::transmute(season, game_id,
                     team_id = ifelse(poss_before == home, home_id, away_id),
                     game_secs_remaining, shot_outcome, free_throw, three_pt,
                     poss_number = as.integer(poss_number), poss_before, score_diff,
                     play_length = pmax(0L, pmin(100L, play_length)),
                     points = as.integer(shot_outcome == "made")*points,
                     sdv_time = score_diff/sqrt(game_secs_remaining + 1L)) %>%
    dplyr::group_by(season, game_id, team_id, poss_number) %>%
    dplyr::summarise(fgm = sum(shot_outcome == "made" & !free_throw, na.rm = TRUE),
                     fga = sum(shot_outcome %in% c("made", "missed") & !free_throw, na.rm = TRUE),
                     three_fgm = sum(shot_outcome == "made" & three_pt, na.rm = TRUE),
                     three_fga = sum(shot_outcome %in% c("made", "missed") & three_pt, na.rm = TRUE),
                     ftm = sum(shot_outcome == "made" & free_throw, na.rm = TRUE),
                     fta = sum(shot_outcome %in% c("made", "missed") & free_throw, na.rm = TRUE),
                     points = sum(points, na.rm = TRUE),
                     poss_length = sum(play_length, na.rm = TRUE),
                     sdv_time = abs(dplyr::first(sdv_time)),
                     score_diff = dplyr::first(score_diff),
                     .groups = "drop") %>%
    collapse::fmutate(sdv_time = pmin(15L, sdv_time),
                      weight = 1L/(sdv_time + 1L))

  return(possessions)
}

# Venues by team
find_venue_locations <- function(schedule) {

  # Distinct venues
  venues <- dplyr::distinct(collapse::fsubset(schedule, !is.na(city), venue_id, city, state)) %>%
    collapse::fmutate(
      city = dplyr::case_when(
        city == "Laval" ~ "Quebec",
        TRUE ~ city
      ),
      state = dplyr::case_when(
        state %in% c("England", "Northern Ireland") ~ "UK",
        state == "GB" ~ "Bahamas",
        state == "PQ" ~ "Canada",
        state %in% c("ON", "BC") ~ "Canada",
        state == "Virgin Islands" ~ "US Virgin Islands",
        TRUE ~ state
      ))

  # Cities
  cities <- dplyr::bind_rows(maps::us.cities, maps::world.cities) %>%
    collapse::fmutate(city = stringr::str_remove(name, paste0(" ", country.etc))) %>%
    collapse::fselect(city, state = country.etc, lat, long)

  venues_mapped <- dplyr::inner_join(venues, cities, by = c("city", "state"))
  venues_unmapped <- dplyr::anti_join(venues, cities, by = c("city", "state"))

  # Average lat/long within state (to impute)
  avg_state_loc <- cities %>%
    dplyr::group_by(state) %>%
    dplyr::summarise(lat = mean(lat),
                     long = mean(long),
                     .groups = "drop")

  venues_unmapped <- dplyr::inner_join(venues_unmapped, avg_state_loc, by = "state")

  return(dplyr::distinct(dplyr::bind_rows(venues_mapped, venues_unmapped), venue_id, .keep_all = TRUE))
}
