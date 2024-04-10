### Women's all possible tourney matchup WPs

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(xgboost)))

# Source helpers
source(here::here("utils.R"))
source(here::here("config.R"))

# Current season
cur_sn <- 2024L

# Load data
load(file = here::here("womens/data/loaded.RData"))

# Tournament teams
tourney_teams <- readr::read_csv("~/Desktop/2024_tourney_seeds_test.csv", col_types = readr::cols()) %>%
  collapse::fsubset(Tournament == "W", seed = Seed, team = TeamName) %>%
  collapse::fmutate(region = stringr::str_extract(seed, "[A-Z]"),
                    seed = readr::parse_integer(stringr::str_remove(stringr::str_remove(seed, region), "(a)|(b)")))

ncaa_tournament <- dplyr::inner_join(tourney_teams, teams, by = c("team" = "team_name"))

assertthat::assert_that(nrow(tourney_teams) == nrow(ncaa_tournament))

# Kaggle team IDs
kaggle_teams <- readr::read_csv("~/Downloads/march-machine-learning-mania-2024/WTeams.csv", col_types = readr::cols()) %>%
  janitor::clean_names() %>%
  dplyr::transmute(kaggle_id = team_id, kaggle_name = team_name, clean_name = team_name) %>%
  clean_kaggle_names(col = "clean_name") %>%
  dplyr::left_join(teams, by = c("clean_name" = "team_name"))

sum(is.na(kaggle_teams$team_id))

# Teams
mm_teams <- dplyr::left_join(tourney_teams, kaggle_teams, by = c("team" = "clean_name")) %>%
  collapse::fselect(-team)

assertthat::assert_that(nrow(tourney_teams) == nrow(mm_teams))

# Read in preseason AP poll data
preseason_ap <- readr::read_csv(here::here(paste0("womens/data/ap/preseason_", cur_sn, ".csv")), col_types = readr::cols()) %>%
  collapse::fsubset(type == "preseason", season, team_id, preseason_ap = ap_rank)

# Simple win probability model using previous score differential predictions
game_predictions <- readRDS(here::here("womens/data/game_preds.rds"))

wp_model <- game_predictions %>%
  collapse::fsubset(!is.na(score_diff) & season >= 2021L) %>%
  dplyr::mutate(home_win = factor(as.integer(score_diff > 0L))) %>%
  glm(home_win ~ pred_score_diff, data = ., family = "binomial")

# Make all potential matchups
potential_matchups <- tidyr::crossing(team1 = mm_teams$kaggle_id,
                                      team2 = mm_teams$kaggle_id) %>%
  collapse::fsubset(team1 < team2) %>%
  dplyr::mutate(small = ifelse(team1 < team2, team1, team2),
                large = ifelse(team1 < team2, team2, team1),
                game_id = paste(cur_sn, small, large, sep = "_"),
                season = cur_sn) %>%
  dplyr::select(-c(small, large)) %>%
  dplyr::left_join(mm_teams %>%
                     dplyr::rename(team1_seed = seed,
                                   team1_team_id = team_id,
                                   team1_region = region,
                                   team1_name = kaggle_name),
                   by = c("team1" = "kaggle_id")) %>%
  dplyr::left_join(mm_teams %>%
                     dplyr::rename(team2_seed = seed,
                                   team2_team_id = team_id,
                                   team2_region = region,
                                   team2_name = kaggle_name),
                   by = c("team2" = "kaggle_id")) %>%
  second_round_matches() %>%
  dplyr::mutate(first_round = as.integer(team1_region == team2_region & team1_seed + team2_seed == 17L),
                neutral = ifelse((first_round + second_round >= 1L) & (team1_seed <= 4L | team2_seed <= 4) & team1_region == team2_region, 0, 1),
                team1_home = as.integer(neutral == 0L & team1_seed <= 4L),
                team2_home = as.integer(neutral == 0L & team2_seed <= 4L))

# Round matchup will occur in
potential_matchups <- purrr::map_df(seq_len(nrow(potential_matchups)), function(row) {
  cur_row <- potential_matchups[row, ]
  cur_row$round <- ifelse(cur_row$team1_region == cur_row$team2_region & cur_row$team1_seed == cur_row$team2_seed,
                          0L,
                          ifelse(cur_row$team1_region == cur_row$team2_region,
                                 region_seeds[[as.character(cur_row$team1_seed)]][[as.character(cur_row$team2_seed)]],
                                 5L))
  return(cur_row)
})

assertthat::assert_that(nrow(potential_matchups) == nrow(mm_teams) * (nrow(mm_teams)-1L)/ 2L, msg = "Probably missing kaggle IDs")

# Most recent ratings
team_ratings <- readRDS(here::here("womens/data/current_metrics.rds")) %>%
  dplyr::left_join(preseason_ap, by = c("season", "team_id"))

# Matchup adjustment model
most_recent_fit <- list.files(here::here("womens/models/team_ratings/lme4/matchup/")) %>%
  stringr::str_remove(., "\\.rds$") %>%
  max()

matchup_fit <- readRDS(here::here(paste0("womens/models/team_ratings/lme4/matchup/", most_recent_fit, ".rds")))

matchup_preds <- data.frame(team_id = mm_teams$team_id,
                            season = cur_sn) %>%
  dplyr::cross_join(data.frame(opp_id = mm_teams$team_id)) %>%
  collapse::fsubset(team_id != opp_id) %>%
  dplyr::inner_join(
    dplyr::bind_rows(
      dplyr::transmute(potential_matchups, team_id = team1_team_id, opp_id = team2_team_id, neutral, home = team1_home),
      dplyr::transmute(potential_matchups, team_id = team2_team_id, opp_id = team1_team_id, neutral, home = team2_home),
    ),
    by = c("team_id", "opp_id")
  ) %>%
  collapse::fmutate(off_location = ifelse(neutral == 1L, "neutral", ifelse(home == 1L, "home", "away"))) %>%
  dplyr::left_join(collapse::fselect(team_ratings, season, team_id, games_played), by = c("season", "team_id")) %>%
  dplyr::left_join(collapse::fselect(team_ratings, opp_id = team_id, opp_ridge_points = ridge_points), by = "opp_id")
matchup_preds$matchup_adj <- unname(predict(matchup_fit, newdata = matchup_preds, allow.new.levels = TRUE))
matchup_preds$games_played <- dplyr::coalesce(matchup_preds$games_played, 0L)
matchup_preds$matchup_adj <- (matchup_preds$matchup_adj*matchup_preds$games_played + 0*1L) / (matchup_preds$games_played + 1L)
matchup_preds <- matchup_preds[, c("team_id", "opp_id", "matchup_adj")]

# Create fake offense data
fake_off_data <- potential_matchups %>%
  dplyr::distinct(season, game_id, off_location = ifelse(team1_home == 1L, "home", ifelse(team2_home == 1L, "away", "neutral")),
                  off_id = team1_team_id, def_id = team2_team_id,
                  off_kaggle_id = team1, def_kaggle_id = team2) %>%
  dplyr::left_join(dplyr::select(conferences, season, off_id = team_id, off_conf = conf),
                   by = c("season", "off_id")) %>%
  dplyr::left_join(dplyr::select(conferences, season, def_id = team_id, def_conf = conf),
                   by = c("season", "def_id")) %>%
  dplyr::left_join(dplyr::select(team_ratings, off_id = team_id, off_games_played = games_played, off_preseason_ap = preseason_ap, dplyr::contains("off"), dplyr::ends_with("_points")),
                   by = "off_id") %>%
  dplyr::left_join(dplyr::rename(matchup_preds, off_matchup_adj = matchup_adj), by = c("off_id" = "team_id", "def_id" = "opp_id")) %>%
  dplyr::left_join(dplyr::select(team_ratings, def_id = team_id, def_games_played = games_played, def_preseason_ap = preseason_ap, def_ridge_off_poss_length = ridge_off_poss_length, dplyr::contains("def"), dplyr::ends_with("_points")),
                   by = "def_id",
                   suffix = c("_off", "_def")) %>%
  dplyr::left_join(dplyr::rename(matchup_preds, def_matchup_adj = matchup_adj), by = c("off_id" = "opp_id", "def_id" = "team_id"))

# Create fake defense data
fake_def_data <- potential_matchups %>%
  dplyr::distinct(season, game_id, off_location = ifelse(team2_home == 1L, "home", ifelse(team1_home == 1L, "away", "neutral")),
                  def_id = team1_team_id, off_id = team2_team_id,
                  def_kaggle_id = team1, off_kaggle_id = team2) %>%
  dplyr::left_join(dplyr::select(conferences, season, off_id = team_id, off_conf = conf),
                   by = c("season", "off_id")) %>%
  dplyr::left_join(dplyr::select(conferences, season, def_id = team_id, def_conf = conf),
                   by = c("season", "def_id")) %>%
  dplyr::left_join(dplyr::select(team_ratings, off_id = team_id, off_games_played = games_played, off_preseason_ap = preseason_ap, dplyr::contains("off"), dplyr::ends_with("_points")),
                   by = "off_id") %>%
  dplyr::left_join(dplyr::rename(matchup_preds, off_matchup_adj = matchup_adj), by = c("off_id" = "team_id", "def_id" = "opp_id")) %>%
  dplyr::left_join(dplyr::select(team_ratings, def_id = team_id, def_games_played = games_played, def_preseason_ap = preseason_ap, def_ridge_off_poss_length = ridge_off_poss_length, dplyr::contains("def"), dplyr::ends_with("_points")),
                   by = "def_id",
                   suffix = c("_off", "_def")) %>%
  dplyr::left_join(dplyr::rename(matchup_preds, def_matchup_adj = matchup_adj), by = c("off_id" = "opp_id", "def_id" = "team_id"))

fake_data <- dplyr::bind_rows(fake_off_data, fake_def_data) %>%
  collapse::roworder(game_id) %>%
  dplyr::mutate(postseason = 1L,
                conf_game = as.integer(off_conf == def_conf),
                off_rest_diff = 0L,
                off_rest = 4L,
                off_streak = 1L,
                def_streak = 1L,
                off_b2b = 0L,
                def_b2b = 0L,
                off_matchup_adj_diff = off_matchup_adj - def_matchup_adj,
                ridge_points_off_diff = ridge_points_off - ridge_points_def,
                ridge_off_ppp_diff = ridge_off_ppp + ridge_def_ppp,
                ridge_off_pps_diff = ridge_off_pps + ridge_def_pps,
                off_lme4_ppp_diff = off_lme4_ppp + def_lme4_ppp,
                off_lme4_epps_diff = off_lme4_epps + def_lme4_epps) %>%
  group_conferences() %>%
  make_conf_dummies() %>%
  make_off_location_dummies()

# Most recent model to predict team level efficiency and tempo
most_recent_fit <- list.files(here::here("womens/models/team_level/")) %>%
  stringr::str_subset(., "ridge", negate = TRUE) %>%
  stringr::str_remove(., "ppp|poss") %>%
  stringr::str_remove(., "\\.rds$") %>%
  max()

ppp_fit <- readRDS(here::here(paste0("womens/models/team_level/ppp", most_recent_fit, ".rds")))
poss_fit <- readRDS(here::here(paste0("womens/models/team_level/poss", most_recent_fit, ".rds")))

fake_team_level_preds <- dplyr::bind_rows(
  data.frame(fake_data[, c("game_id", "off_id")],
             pred = predict(ppp_fit, newdata = as.matrix(fake_data[, ppp_fit$feature_names]))) %>%
    dplyr::transmute(game_id, team_id = off_id, metric = "ppp", pred),

  data.frame(fake_data[, c("game_id", "off_id")],
             pred = predict(poss_fit, newdata = as.matrix(fake_data[, poss_fit$feature_names]))) %>%
    dplyr::transmute(game_id, team_id = off_id, metric = "poss", pred)
) %>%
  tidyr::pivot_wider(names_from = metric, values_from = pred) %>%
  dplyr::rename(pred_poss = poss, pred_ppp = ppp)

# Create fake game data (CHECK THAT THIS IS ONLY 2 ROWS/GAME)
fake_game_data <- bind_rows(
  potential_matchups %>%
    dplyr::distinct(season, game_id, round, home = team1_home, neutral, home_kaggle_id = team1, away_kaggle_id = team2, home_seed = team1_seed, home_team_id = team1_team_id, away_team_id = team2_team_id),
  potential_matchups %>%
    dplyr::distinct(season, game_id, round, home = team2_home, neutral, home_kaggle_id = team2, away_kaggle_id = team1, home_seed = team2_seed, home_team_id = team2_team_id, away_team_id = team1_team_id)
) %>%
  # Join in efficiency and possession predictions
  dplyr::inner_join(fake_team_level_preds %>%
                      dplyr::rename_with(.cols = dplyr::starts_with("pred_"), ~ paste0("home_", .)),
                    by = c("game_id", "home_team_id" = "team_id")) %>%
  dplyr::inner_join(fake_team_level_preds %>%
                      dplyr::rename_with(.cols = dplyr::starts_with("pred_"), ~ paste0("away_", .)),
                    by = c("game_id", "away_team_id" = "team_id")) %>%
  dplyr::left_join(team_ratings %>%
                     dplyr::select(team_id, home_games_played = games_played,
                                   home_ridge_points = ridge_points,
                                   home_off_lme4_ppp = off_lme4_ppp, home_def_lme4_ppp = def_lme4_ppp),
                   by = c("home_team_id" = "team_id")) %>%
  dplyr::left_join(team_ratings %>%
                     dplyr::select(team_id, away_games_played = games_played,
                                   away_ridge_points = ridge_points,
                                   away_off_lme4_ppp = off_lme4_ppp, away_def_lme4_ppp = def_lme4_ppp),
                   by = c("away_team_id" = "team_id")) %>%
  # Matchup adjustment predictions
  dplyr::left_join(dplyr::rename(matchup_preds, home_matchup_adj = matchup_adj), by = c("home_team_id" = "team_id", "away_team_id" = "opp_id")) %>%
  dplyr::left_join(dplyr::rename(matchup_preds, away_matchup_adj = matchup_adj), by = c("away_team_id" = "team_id", "home_team_id" = "opp_id")) %>%
  dplyr::mutate(postseason = 1L,
                home_rest_diff = 0L,
                home_rest = 4L,
                home_streak = 1L,
                away_streak = 1L,
                home_pred_ppp_diff = home_pred_ppp - away_pred_ppp,
                home_matchup_adj_diff = home_matchup_adj - away_matchup_adj,
                home_ridge_points_diff = home_ridge_points - away_ridge_points,
                home_off_lme4_ppp_diff = home_off_lme4_ppp + away_def_lme4_ppp,
                home_def_lme4_ppp_diff = home_def_lme4_ppp + away_off_lme4_ppp)

# Home games
home_games <- collapse::fsubset(fake_game_data, neutral == 0L & home == 1L, game_id, home_kaggle_id, away_kaggle_id)

fake_game_data <- dplyr::bind_rows(
  collapse::fsubset(fake_game_data, !game_id %in% home_games$game_id),
  dplyr::inner_join(fake_game_data, home_games, by = c("game_id", "home_kaggle_id", "away_kaggle_id"))
)

# Most recent model to predict score differential
most_recent_fit <- list.files(here::here("womens/models/game_level/")) %>%
  stringr::str_remove(".rds$") %>%
  max()

game_fit <- readRDS(here::here(paste0("womens/models/game_level/", most_recent_fit, ".rds")))

pred_scores <- data.frame(fake_game_data[, c("game_id", "round", "home_kaggle_id", "away_kaggle_id")],
                          pred = predict(game_fit, newdata = as.matrix(fake_game_data[, game_fit$feature_names]))) %>%
  dplyr::transmute(game_id, round,
                   home_kaggle_id, away_kaggle_id,
                   kaggle_team_id = ifelse(home_kaggle_id < away_kaggle_id, home_kaggle_id, away_kaggle_id),
                   kaggle_opp_id = ifelse(home_kaggle_id < away_kaggle_id, away_kaggle_id, home_kaggle_id),
                   pred_score_diff = pred,
                   type = ifelse(home_kaggle_id < away_kaggle_id, "home", "away")) %>%
  dplyr::select(game_id, round, kaggle_team_id, kaggle_opp_id, pred_score_diff, type) %>%
  tidyr::pivot_wider(names_from = type, values_from = pred_score_diff) %>%
  dplyr::mutate(away = -1*away,
                home = dplyr::coalesce(home, away),
                away = dplyr::coalesce(away, home),
                pred_score_diff = 0.5*(home + away)) %>%
  dplyr::select(game_id, round, kaggle_team_id, kaggle_opp_id, pred_score_diff) %>%
  dplyr::mutate(pred_wp = predict(wp_model, newdata = ., type = "response")) %>%
  dplyr::left_join(mm_teams[, !colnames(mm_teams) %in% c("team_id")], by = c("kaggle_team_id" = "kaggle_id")) %>%
  dplyr::left_join(mm_teams[, !colnames(mm_teams) %in% c("team_id")] %>%
                     dplyr::rename(opp_seed = seed,
                                   opp_region = region,
                                   opp_team = kaggle_name,
                                   opp_kaggle_id = kaggle_id),
                   by = c("kaggle_opp_id" = "opp_kaggle_id")) %>%
  dplyr::select(game_id, round, kaggle_id = kaggle_team_id, opp_kaggle_id = kaggle_opp_id,
                team_name = kaggle_name, region, seed, opp_team_name = opp_team, opp_region, opp_seed,
                pred_score_diff, pred_wp)

# Write output to desktop
readr::write_csv(pred_scores, "~/Desktop/w_kaggle_preds.csv")
