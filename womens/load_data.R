### Load women's data

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(collapse)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(wehoop)))
suppressMessages(suppressWarnings(library(xgboost)))

# Source helpers
source(here::here("utils.R"))
source(here::here("espn_processing.R"))

current_season <- get_current_season()

option_list <- list(
  make_option("--min_season", type = "integer", default = 2017L, help = "minimum season of data"),
  make_option("--season", type = "integer", default = current_season, help = "season, default current_season"),
  make_option("--combine", type = "logical", default = FALSE, help = "combine all ESPN data? default FALSE")
)
opt <- parse_args(OptionParser(option_list = option_list))
# Rscript load_data.R --season 2018

if (opt$season < opt$min_season | opt$season > current_season) {
  cat(opt$season, " must be between", opt$min_season, "and", current_season , "...\n")
  quit(status = 0)
}

if (isFALSE(opt$combine)) {
  cat("Loading in women's ESPN play-by-play data", opt$season, "...\n")

  # Load play-by-play
  pbp <- wehoop::load_wbb_pbp(seasons = opt$season) %>%
    process_espn_pbp(womens = TRUE)

  cat("Loading in schedule", opt$season, "...\n")

  # Load schedule
  schedule <- wehoop::load_wbb_schedule(seasons = opt$season) %>%
    process_espn_schedule()

  # Find venue locations
  venues <- find_venue_locations(schedule = schedule)
  schedule <- dplyr::inner_join(schedule, venues[, c("venue_id", "long", "lat")], by = "venue_id")

  home_venues <- collapse::fsubset(schedule, neutral == 0L, season, team_id = home_id, venue_id) %>%
    dplyr::group_by(team_id) %>%
    dplyr::summarise(home_venue_id = collapse::fmode(venue_id),
                     .groups = "drop") %>%
    dplyr::inner_join(venues[, c("venue_id", "long", "lat")], by = c("home_venue_id" = "venue_id")) %>%
    collapse::fselect(team_id, home_long = long, home_lat = lat)

  # Load conference dictionary
  conference_dictionary <- wehoop::espn_wbb_conferences() %>%
    dplyr::transmute(conference_id = as.integer(group_id), conf = conference_short_name) %>%
    as.data.frame()

  dicts <- make_team_conference_dicts(schedule = schedule, conference_dictionary = conference_dictionary)
  conferences <- dicts$conferences
  teams <- dicts$teams

  # Dictionary of home and away teams and rest times
  home_away_dict <- process_rest_travel(schedule = schedule, home_venues = home_venues)

  double_games <- dplyr::bind_rows(
    collapse::fselect(schedule, season, game_id, off_id = home_id, def_id = away_id, points = home_score),
    collapse::fselect(schedule, season, game_id, off_id = away_id, def_id = home_id, points = away_score)
  ) %>%
    # Join in offense information
    dplyr::left_join(home_away_dict %>%
                       dplyr::rename(off_location = location,
                                     off_conf = conf,
                                     off_id = team_id,
                                     off_rest = rest,
                                     off_distance_from_home = distance_from_home,
                                     off_b2b_home = b2b_home,
                                     off_b2b_road = b2b_road,
                                     off_streak = streak) %>%
                       dplyr::select(-c(triple_road, triple_home)),
                     by = c("game_id", "off_id")) %>%
    # Join in defense information
    dplyr::left_join(home_away_dict %>%
                       dplyr::rename(def_conf = conf,
                                     def_id = team_id,
                                     def_rest = rest,
                                     def_distance_from_home = distance_from_home,
                                     def_b2b_home = b2b_home,
                                     def_b2b_road = b2b_road,
                                     def_streak = streak) %>%
                       dplyr::select(-c(location, postseason, game_date, hour, triple_road, triple_home)),
                     by = c("game_id", "def_id")) %>%
    collapse::fmutate(off_b2b = ifelse(off_location == "home", off_b2b_home, off_b2b_road),
                      def_b2b = ifelse(off_location == "home", def_b2b_road, def_b2b_home),
                      completed = as.integer(!is.na(points))) %>%
    dplyr::select(-c(off_b2b_home, off_b2b_road, def_b2b_road, def_b2b_home)) %>%
    collapse::fsubset(!(completed == 0L & game_date < Sys.Date() - 10L)) %>%
    collapse::roworder(game_date, game_id)

  cat("Summarizing play-by-play data...\n")

  # Games where ESPN charts only one team id as the event team
  bad_charted_team_ids <- pbp %>%
    dplyr::group_by(game_id) %>%
    dplyr::summarise(home = sum(team_id == home_id, na.rm = TRUE),
                     away = sum(team_id == away_id, na.rm = TRUE),
                     .groups = "drop") %>%
    collapse::fmutate(diff = abs(home - away)) %>%
    collapse::fsubset(diff >= 100L, game_id)

  pbp <- collapse::fsubset(pbp, !game_id %in% bad_charted_team_ids$game_id)

  # Possessions level points
  possessions <- process_possessions(pbp = pbp)

  # Minutes played each game
  game_minutes <- pbp %>%
    dplyr::distinct(game_id, qtr) %>%
    collapse::fmutate(minutes = ifelse(qtr <= 4L, 10L, 5L)) %>%
    dplyr::group_by(game_id) %>%
    dplyr::summarise(minutes = sum(minutes),
                     .groups = "drop") %>%
    collapse::fsubset(minutes >= 40L)

  cat("Shot quality predictions...\n")

  # Filter for shots
  shots <- collapse::fsubset(pbp, !is.na(shot_outcome) & !free_throw) %>%
    prepare_for_shot_prob(randomize = FALSE, gamezone = FALSE) %>%
    dplyr::inner_join(schedule[, c("game_id", "neutral")], by = "game_id") %>%
    collapse::fselect(game_id, team_id, season, shot_outcome, shot_desc, shot_distance, shot_angle,
                      three_pt, play_length, neutral, home_shot, score_diff, shot_value,
                      qtr_secs_remaining, half_secs_remaining, game_secs_remaining, qtr) %>%
    collapse::fmutate(winning = ifelse(score_diff == 0L, 0L, ifelse(score_diff > 0L, 1L, -1L)),
                      off_home = ifelse(neutral == 1L, 0L, ifelse(home_shot == 1L, 1L, -1L)),
                      half = ifelse(qtr <= 2L, 1L, ifelse(qtr <= 4L, 2L, 3L)),
                      shot_desc = ifelse(shot_desc %in% c("ThreePointJumpShot", "BlockShot"), "JumpShot", shot_desc),
                      shot_desc_DunkShot = as.integer(shot_desc == "DunkShot"),
                      shot_desc_JumpShot = as.integer(shot_desc == "JumpShot"),
                      shot_desc_LayupShot = as.integer(shot_desc %in% c("LayUpShot", "Layup")),
                      shot_desc_TipShot = as.integer(shot_desc == "TipShot"))

  # Load shot probability models
  shot_model <- readRDS(here::here("shot_model/w_shot_model.rds"))
  shot_model_loc <- readRDS(here::here("shot_model/w_shot_modelLOC.rds"))

  shots_no_loc <- collapse::fsubset(shots, is.na(shot_distance)) %>%
    collapse::fmutate(pred_make = predict(shot_model, newdata = as.matrix(.[, shot_model$feature_names]), type = "prob"))
  shots_loc <- collapse::fsubset(shots, !is.na(shot_distance))
  if (nrow(shots_loc) > 0L) {
    shots_loc <- collapse::fmutate(shots_loc, pred_make = predict(shot_model_loc, newdata = as.matrix(shots_loc[, shot_model_loc$feature_names]), type = "prob"))
  }

  # Get shot probability predictions
  shots <- dplyr::bind_rows(shots_no_loc, shots_loc)

  cat("Summarizing all play-by-play statistics...\n")

  # Clean shot level data
  shots_clean <- shots %>%
    dplyr::transmute(game_id, off_id = team_id, shot_outcome, shot_prob = pred_make,
                     points = shot_value * as.integer(shot_outcome == "made"),
                     epps = shot_value * shot_prob,
                     game_secs_remaining, score_diff,
                     sdv_time = pmin(15, abs(score_diff/sqrt(1 + game_secs_remaining))),
                     weight = 1/(sdv_time + 1)) %>%
    dplyr::inner_join(collapse::fselect(double_games, -points), by = c("game_id", "off_id")) %>%
    collapse::roworder(game_date) %>%
    dplyr::transmute(season, game_date, hour, game_id, off_id, def_id, off_location, postseason,
                     shot_outcome, points, shot_prob, epps, weight, score_diff,
                     off_rest_diff = off_rest - def_rest, off_rest, def_rest, off_b2b, def_b2b,
                     off_distance_from_home, def_distance_from_home,
                     off_conf, def_conf)

  # Clean possession level data
  possessions_clean <- possessions %>%
    dplyr::rename(off_id = team_id) %>%
    dplyr::inner_join(collapse::fselect(double_games, -points), by = c("season", "game_id", "off_id")) %>%
    collapse::roworder(game_date, game_id, poss_number) %>%
    collapse::fmutate(off_rest_diff = off_rest - def_rest)

  cat("Saving data...\n")

  # Clean environment
  rm(list = setdiff(ls(), c("game_minutes", "schedule", "home_away_dict",
                            "possessions_clean", "shots_clean", "opt")))

  # Save all data
  saveRDS(game_minutes, here::here(paste0("womens/data/seasons/game_minutes", opt$season, ".rds")))
  saveRDS(schedule, here::here(paste0("womens/data/seasons/schedule", opt$season, ".rds")))
  saveRDS(home_away_dict, here::here(paste0("womens/data/seasons/home_away_dict", opt$season, ".rds")))
  saveRDS(possessions_clean, here::here(paste0("womens/data/seasons/possessions_clean", opt$season, ".rds")))
  saveRDS(shots_clean, here::here(paste0("womens/data/seasons/shots_clean", opt$season, ".rds")))

  cat("Done with data load.\n")

  quit(status = 0)

} else if (isTRUE(opt$combine)) {

  cat("Combining all women's ESPN play-by-play data...\n")

  # Read all data by season
  game_minutes <- data.frame()
  schedule <- data.frame()
  home_away_dict <- data.frame()
  possessions_clean <- data.frame()
  shots_clean <- data.frame()
  for (sn in opt$min_season:get_current_season()) {
    game_minutes <- dplyr::bind_rows(game_minutes, readRDS(here::here(paste0("womens/data/seasons/game_minutes", sn, ".rds"))))
    schedule <- dplyr::bind_rows(schedule, readRDS(here::here(paste0("womens/data/seasons/schedule", sn, ".rds"))))
    home_away_dict <- dplyr::bind_rows(home_away_dict, readRDS(here::here(paste0("womens/data/seasons/home_away_dict", sn, ".rds"))))
    possessions_clean <- dplyr::bind_rows(possessions_clean, readRDS(here::here(paste0("womens/data/seasons/possessions_clean", sn, ".rds"))))
    shots_clean <- dplyr::bind_rows(shots_clean, readRDS(here::here(paste0("womens/data/seasons/shots_clean", sn, ".rds"))))
  }

  # Load conference dictionary
  conference_dictionary <- wehoop::espn_wbb_conferences() %>%
    dplyr::transmute(conference_id = as.integer(group_id), conf = conference_short_name) %>%
    as.data.frame()

  dicts <- make_team_conference_dicts(schedule = schedule, conference_dictionary = conference_dictionary)
  conferences <- dicts$conferences
  teams <- dicts$teams

  # Rest and back to backs
  rest <- schedule[, c("game_id", "home_id", "away_id")] %>%
    dplyr::left_join(collapse::fsubset(home_away_dict, !is.na(team_id),
                                       game_id, team_id, home_rest = rest, b2b_home, home_streak = streak,
                                       home_distance_from_home = distance_from_home),
                     by = c("game_id", "home_id" = "team_id")) %>%
    dplyr::left_join(collapse::fsubset(home_away_dict, !is.na(team_id),
                                       game_id, team_id, away_rest = rest, b2b_road, away_streak = streak,
                                       away_distance_from_home = distance_from_home),
                     by = c("game_id", "away_id" = "team_id"))

  # Double games by team
  double_games <- dplyr::bind_rows(
    collapse::fselect(schedule, season, game_id, off_id = home_id, def_id = away_id, points = home_score),
    collapse::fselect(schedule, season, game_id, off_id = away_id, def_id = home_id, points = away_score)
  ) %>%
    # Join in offense information
    dplyr::left_join(home_away_dict %>%
                       dplyr::rename(off_location = location,
                                     off_conf = conf,
                                     off_id = team_id,
                                     off_rest = rest,
                                     off_distance_from_home = distance_from_home,
                                     off_b2b_home = b2b_home,
                                     off_b2b_road = b2b_road,
                                     off_streak = streak) %>%
                       dplyr::select(-c(triple_road, triple_home)),
                     by = c("game_id", "off_id")) %>%
    # Join in defense information
    dplyr::left_join(home_away_dict %>%
                       dplyr::rename(def_conf = conf,
                                     def_id = team_id,
                                     def_rest = rest,
                                     def_distance_from_home = distance_from_home,
                                     def_b2b_home = b2b_home,
                                     def_b2b_road = b2b_road,
                                     def_streak = streak) %>%
                       dplyr::select(-c(location, postseason, game_date, hour, triple_road, triple_home)),
                     by = c("game_id", "def_id")) %>%
    collapse::fmutate(off_b2b = ifelse(off_location == "home", off_b2b_home, off_b2b_road),
                      def_b2b = ifelse(off_location == "home", def_b2b_road, def_b2b_home),
                      completed = as.integer(!is.na(points))) %>%
    dplyr::select(-c(off_b2b_home, off_b2b_road, def_b2b_road, def_b2b_home)) %>%
    collapse::fsubset(!(completed == 0L & game_date < Sys.Date() - 10L)) %>%
    collapse::roworder(game_date, game_id)

  # Summarize game stats
  pbp_summarized <- possessions_clean %>%
    dplyr::group_by(game_id, off_id) %>%
    dplyr::summarise(poss = dplyr::n(),
                     dplyr::across(fgm:points, sum),
                     .groups = "drop") %>%
    collapse::fmutate(ppp = points/poss,
                      pps = (points - ftm)/fga) %>%
    # Some games ESPN never charts made baskets (other than FTs)
    collapse::fsubset(pps > 0L) %>%
    collapse::frename(pbp_points = points)

  # Average possession lengths by team-game
  poss_lengths <- possessions_clean %>%
    dplyr::group_by(game_id, off_id) %>%
    dplyr::summarise(poss = dplyr::n(),
                     poss_length = mean(poss_length),
                     .groups = "drop") %>%
    # Only games with more than 20 possessions, less than 27 seconds per possession
    collapse::fsubset(poss >= 20L & poss_length <= 27 & poss_length >= 3, -poss)

  # Shot prediction metrics by team-game
  shot_prediction_metrics <- shots_clean %>%
    dplyr::group_by(game_id, off_id) %>%
    dplyr::summarise(exp_fg_pct = mean(shot_prob),
                     epps = mean(epps),
                     .groups = "drop")

  cat("Summarizing all play-by-play statistics...\n")

  # Game statistics
  game_stats <- dplyr::left_join(double_games, pbp_summarized, by = c("game_id", "off_id")) %>%
    # Join in minutes in game
    dplyr::left_join(game_minutes, by = "game_id") %>%
    collapse::fmutate(poss_min = poss/minutes) %>%
    dplyr::relocate(poss_min, .after = poss) %>%
    # Join in shot probability metrics
    dplyr::left_join(shot_prediction_metrics, by = c("game_id", "off_id")) %>%
    # Join in average possession lengths
    dplyr::left_join(poss_lengths, by = c("game_id", "off_id")) %>%
    dplyr::mutate(train = as.integer(!(dplyr::coalesce(abs(points - pbp_points), 0L) > 10L | is.na(poss_length))),
                  dplyr::across(poss:poss_length, ~ ifelse(train == 0L, NA, .))) %>%
    collapse::roworder(game_date)

  # League average PPP, PPS, and possessions
  lg_avgs <- game_stats %>%
    collapse::fsubset(train == 1L) %>%
    dplyr::group_by(season, game_date) %>%
    dplyr::summarise(games = dplyr::n_distinct(game_id),
                     cur_lg_mean_ppp = sum(ppp*poss)/sum(poss),
                     cur_lg_mean_pps = sum(pps*fga)/sum(fga),
                     cur_lg_mean_epps = sum(epps*fga)/sum(fga),
                     cur_lg_mean_poss = mean(poss_min),
                     cur_lg_mean_poss_length = mean(poss_length),
                     .groups = "drop") %>%
    collapse::roworder(game_date) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("cur_lg_"),
                                ~ jacklich::wt_mov_avg(var = .,
                                                       weight = games,
                                                       window = 15,
                                                       type = "s",
                                                       moving = TRUE))) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("cur_lg_mean_"),
                                ~ dplyr::lag(cumsum(.*games)/cumsum(games)),
                                .names = "lag_{.col}")) %>%
    dplyr::ungroup() %>%
    dplyr::rename_with(.cols = dplyr::starts_with("lag_cur_"), ~ stringr::str_remove(., "_cur")) %>%
    tidyr::fill(lag_lg_mean_ppp, lag_lg_mean_pps, lag_lg_mean_epps, lag_lg_mean_poss, lag_lg_mean_poss_length, .direction = "down")

  # Find last game that is completed
  last_game <- max(game_stats[game_stats$completed == 1L, ]$game_date)

  # Find current season
  current_season <- max(game_stats$season)

  # Date grid
  date_grid <- dplyr::distinct(collapse::fsubset(schedule, game_date < Sys.Date()+1L), season, game_date)

  # Start of season dates
  start_dates <- date_grid %>%
    dplyr::group_by(season) %>%
    dplyr::slice_min(game_date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::pull(game_date)

  # Season day numbers
  daynumbers <- date_grid %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(daynum = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    collapse::fselect(game_date, daynum)

  # Next games to be played
  new_games <- dplyr::bind_rows(
    schedule %>%
      dplyr::filter(game_date >= last_game + 1 | (game_date == last_game & is.na(score_diff)),
                    !(is.na(home_id) & is.na(away_id))) %>%
      dplyr::transmute(season, game_id, game_date, hour, neutral, postseason, team_id = home_id),

    schedule %>%
      dplyr::filter(game_date >= last_game + 1 | (game_date == last_game & is.na(score_diff)),
                    !(is.na(home_id) & is.na(away_id))) %>%
      dplyr::transmute(season, game_id, game_date, hour, neutral, postseason, team_id = away_id)
  ) %>%
    collapse::roworder(game_date, game_id) %>%
    dplyr::left_join(home_away_dict %>%
                       dplyr::select(-c(triple_road, triple_home, hour, postseason)),
                     by = c("game_id", "game_date", "team_id"))

  new_games_off_def <- new_games %>%
    dplyr::rename(off_id = team_id,
                  off_location = location,
                  off_rest = rest,
                  off_conf = conf,
                  off_distance_from_home = distance_from_home,
                  off_b2b_home = b2b_home,
                  off_b2b_road = b2b_road,
                  off_streak = streak) %>%
    dplyr::left_join(new_games %>%
                       dplyr::rename(def_id = team_id,
                                     def_location = location,
                                     def_rest = rest,
                                     def_conf = conf,
                                     def_distance_from_home = distance_from_home,
                                     def_b2b_home = b2b_home,
                                     def_b2b_road = b2b_road,
                                     def_streak = streak),
                     by = c("game_id", "game_date", "neutral", "season", "hour", "postseason"),
                     relationship = "many-to-many") %>%
    collapse::fsubset(off_id != def_id) %>%
    collapse::fmutate(off_rest_diff = off_rest - def_rest,
                      off_b2b = ifelse(off_location == "home", off_b2b_home, off_b2b_road),
                      def_b2b = ifelse(off_location == "home", def_b2b_road, def_b2b_home)) %>%
    dplyr::select(-c(off_b2b_home, off_b2b_road, def_b2b_road, def_b2b_home))

  cat("Saving data...\n")

  # Save .RData
  save(game_stats, schedule, lg_avgs,
       date_grid, start_dates, daynumbers,
       last_game, current_season,
       conferences, teams, home_away_dict,
       rest, possessions_clean, shots_clean,
       new_games, new_games_off_def,
       file = here::here("womens/data/loaded.RData"))

  quit(status = 0)
}
