# Process team effects

# Load libraries
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--gender", type = "character", default = "mens", help = "mens or womens, default mens")
)
opt <- parse_args(OptionParser(option_list = option_list))

cat("---Processing all", opt$gender, "team ratings---\n")

# Load in data
load(file = here::here(opt$gender, "/data/loaded.RData"))

# Read in preseason AP poll data
preseason_ap <- purrr::map_df(list.files(here::here(opt$gender, "/data/ap/")), function(path) {
  readr::read_csv(here::here(paste0(opt$gender, "/data/ap/", path)), col_types = readr::cols()) %>%
    collapse::fsubset(type == "preseason", season, team_id, preseason_ap = ap_rank)
})

# Source helpers
source(here::here("utils.R"))
source(here::here("config.R"))

# Games charted by team-season
season_games <- dplyr::bind_rows(
  dplyr::distinct(schedule, season, game_date, team_id = home_id),
  dplyr::distinct(schedule, season, game_date, team_id = away_id)
) %>%
  dplyr::distinct() %>%
  collapse::roworder(game_date) %>%
  dplyr::group_by(season, team_id) %>%
  dplyr::mutate(games_played = dplyr::row_number()-1L) %>%
  dplyr::ungroup()

# Total games charted
games_charted <- dplyr::count(season_games, season, team_id, name = "games_played")

# Read in rolled metrics
rolled_metrics <- readRDS(here::here(opt$gender, "/data/roll_metrics.rds"))

# Read in ridge regression predictions
ridge_game_preds <- readRDS(here::here(opt$gender, "/data/ridge_preds_game_level.rds")) %>%
  collapse::fselect(game_date, team_id, ridge_points)

ridge_poss_preds <- readRDS(here::here(opt$gender, "/data/ridge_preds_poss_level.rds")) %>%
  collapse::fselect(-n, -season) %>%
  tidyr::pivot_wider(names_from = metric, values_from = pred)

# Read in LME4 random effects
lme4_preds <- readRDS(here::here(opt$gender, "/data/lme4_preds_poss_level.rds")) %>%
  collapse::fmutate(off_def = ifelse(stringr::str_detect(type, "off"), "off", "def"),
                    rating = ifelse(off_def == "off",
                                    (rating*n + params[[opt$gender]]$lme4$fill_o*params[[opt$gender]]$lme4$pad_o)/(n + params[[opt$gender]]$lme4$pad_o),
                                    (rating*n + params[[opt$gender]]$lme4$fill_d*params[[opt$gender]]$lme4$pad_d)/(n + params[[opt$gender]]$lme4$pad_d))) %>%
  collapse::fselect(season, game_date, team_id, season_eff, type, rating) %>%
  tidyr::pivot_wider(names_from = type, values_from = rating) %>%
  collapse::fsubset(season == season_eff, -season_eff, -season)

# All team metrics by date
team_preds <- dplyr::full_join(
  collapse::fsubset(rolled_metrics$metrics, game_date >= "2018-11-06"),
  ridge_game_preds,
  by = c("game_date", "team_id")
) %>%
  dplyr::full_join(ridge_poss_preds, by = c("game_date", "team_id")) %>%
  dplyr::full_join(lme4_preds, by = c("game_date", "team_id")) %>%
  collapse::roworder(game_date) %>%
  dplyr::group_by(team_id) %>%
  tidyr::fill(dplyr::everything(), .direction = "down") %>%
  dplyr::ungroup() %>%
  tidyr::replace_na(list(ridge_points = params[[opt$gender]]$game$fill,
                         ridge_off_poss_length = params[[opt$gender]]$poss_length$fill_o,
                         ridge_def_poss_length = params[[opt$gender]]$poss_length$fill_d,
                         ridge_off_ppp = params[[opt$gender]]$ppp$fill_o,
                         ridge_def_ppp = params[[opt$gender]]$ppp$fill_d,
                         ridge_off_pps = params[[opt$gender]]$pps$fill_o,
                         ridge_def_pps = params[[opt$gender]]$pps$fill_d,
                         off_lme4_epps = params[[opt$gender]]$lme4$fill_o,
                         def_lme4_epps = params[[opt$gender]]$lme4$fill_d,
                         off_lme4_ppp = params[[opt$gender]]$lme4$fill_o,
                         def_lme4_ppp = params[[opt$gender]]$lme4$fill_d))

saveRDS(team_preds, here::here(opt$gender, "/data/team_metrics.rds"))

# All current team metrics
team_preds_curr <- collapse::fsubset(team_preds, game_date == max(game_date), -game_date) %>%
  collapse::fmutate(season = current_season) %>%
  dplyr::inner_join(games_charted, by = c("season", "team_id")) %>%
  dplyr::relocate(c(season, games_played), .before = 0) %>%
  collapse::roworder(-ridge_points)

# team_preds_curr %>%
#   left_join(teams) %>%
#   select(team_name, games_played, cum_off_poss, cum_def_poss, contains("ridge")) %>% View()

# team_preds_curr %>%
#   pivot_longer(cols = starts_with("ridge_")) %>%
#   mutate(side = ifelse(stringr::str_detect(name, "off"), "off", "def"),
#          name = stringr::str_remove(name, "off_|def_")) %>%
#   ggplot(aes(value, fill = side)) +
#   geom_histogram(position = "identity", alpha = 0.5) +
#   facet_wrap(~ name, scales = "free_x")

# Matchup prediction adjustments
lme4_matchup_preds <- readRDS(here::here(opt$gender, "/data/lme4_preds_matchup.rds")) %>%
  collapse::fselect(game_id, team_id, matchup_adj)

### Create efficiency and possession model datasets
cat("Creating efficiency and possession model datasets...\n")

# Efficiency model data
ppp_model_dataset <- collapse::fsubset(game_stats, season >= 2019L & completed == 1L, season, game_date, game_id,
                                       off_id, def_id, off_location,
                                       postseason, off_conf, def_conf, off_streak, def_streak,
                                       off_rest, def_rest, off_b2b, def_b2b, ppp, completed, train) %>%
  # Offensive metrics
  dplyr::left_join(collapse::fselect(season_games, game_date, team_id, off_games_played = games_played),
                   by = c("game_date", "off_id" = "team_id")) %>%
  dplyr::left_join(dplyr::select(team_preds, game_date, team_id, dplyr::contains("off_"), dplyr::contains("_points")),
                   by = c("game_date", "off_id" = "team_id")) %>%
  dplyr::left_join(dplyr::rename(lme4_matchup_preds, off_matchup_adj = matchup_adj),
                   by = c("game_id", "off_id" = "team_id")) %>%
  # Defensive metrics
  dplyr::left_join(collapse::fselect(season_games, game_date, team_id, def_games_played = games_played),
                   by = c("game_date", "def_id" = "team_id")) %>%
  dplyr::left_join(dplyr::select(team_preds, game_date, team_id, def_ridge_off_ppp = ridge_off_ppp, dplyr::contains("def_"), dplyr::contains("_points")),
                   by = c("game_date", "def_id" = "team_id"),
                   suffix = c("_off", "_def")) %>%
  dplyr::left_join(dplyr::rename(lme4_matchup_preds, def_matchup_adj = matchup_adj),
                   by = c("game_id", "def_id" = "team_id"),
                   suffix = c("_off", "_def")) %>%
  # Preseason AP poll
  dplyr::left_join(collapse::fselect(preseason_ap, season, team_id, off_preseason_ap = preseason_ap), by = c("season", "off_id" = "team_id")) %>%
  dplyr::left_join(collapse::fselect(preseason_ap, season, team_id, def_preseason_ap = preseason_ap), by = c("season", "def_id" = "team_id")) %>%
  collapse::fmutate(off_rest_diff = off_rest - def_rest,
                    off_matchup_adj_diff = off_matchup_adj - def_matchup_adj,
                    ridge_points_off_diff = ridge_points_off - ridge_points_def,
                    ridge_off_ppp_diff = ridge_off_ppp + ridge_def_ppp,
                    ridge_off_pps_diff = ridge_off_pps + ridge_def_pps,
                    off_lme4_ppp_diff = off_lme4_ppp + def_lme4_ppp,
                    off_lme4_epps_diff = off_lme4_epps + def_lme4_epps,
                    conf_game = as.integer(off_conf == def_conf)) %>%
  dplyr::select(season, game_date, game_id, off_id, def_id, off_location, postseason,
                off_conf, def_conf, conf_game, off_streak, def_streak,
                off_rest_diff, off_rest, def_rest, off_b2b, def_b2b,
                off_preseason_ap, def_preseason_ap,
                cum_off_poss, cum_def_poss, off_games_played, def_games_played,
                off_ppp, off_pps, off_epps,
                off_lme4_ppp, off_lme4_ppp_diff,
                off_lme4_epps, off_lme4_epps_diff,
                ridge_off_ppp, ridge_off_ppp_diff,
                ridge_off_pps, ridge_off_pps_diff,
                ridge_points_off, ridge_points_off_diff,
                off_matchup_adj, off_matchup_adj_diff,
                def_ppp, def_pps, def_epps,
                ppp, completed, train)

# Possession model data
poss_model_dataset <- collapse::fsubset(game_stats, season >= 2019L & completed == 1L, season, game_date, game_id,
                                        off_id, def_id, off_location,
                                        postseason, off_conf, def_conf, off_streak, def_streak,
                                        off_rest, def_rest, off_b2b, def_b2b, poss, completed, train) %>%
  # Offensive metrics
  dplyr::left_join(collapse::fselect(season_games, game_date, team_id, off_games_played = games_played),
                   by = c("game_date", "off_id" = "team_id")) %>%
  dplyr::left_join(dplyr::select(team_preds, game_date, team_id, dplyr::contains("off_")),
                   by = c("game_date", "off_id" = "team_id")) %>%
  # Defensive metrics
  dplyr::left_join(collapse::fselect(season_games, game_date, team_id, def_games_played = games_played),
                   by = c("game_date", "def_id" = "team_id")) %>%
  dplyr::left_join(dplyr::select(team_preds, game_date, team_id, def_ridge_off_poss_length = ridge_off_poss_length, dplyr::contains("def_")),
                   by = c("game_date", "def_id" = "team_id")) %>%
  # Preseason AP poll
  dplyr::left_join(collapse::fselect(preseason_ap, season, team_id, off_preseason_ap = preseason_ap), by = c("season", "off_id" = "team_id")) %>%
  dplyr::left_join(collapse::fselect(preseason_ap, season, team_id, def_preseason_ap = preseason_ap), by = c("season", "def_id" = "team_id")) %>%
  collapse::fmutate(off_rest_diff = off_rest - def_rest,
                    conf_game = as.integer(off_conf == def_conf)) %>%
  dplyr::select(season, game_date, game_id, off_id, def_id, off_location, postseason,
                off_conf, def_conf, conf_game, off_streak, def_streak,
                off_rest_diff, off_rest, def_rest, off_b2b, def_b2b,
                off_games_played, def_games_played,
                off_ppp, off_poss_min, off_poss_length, ridge_off_poss_length,
                def_ppp, def_poss_min, def_poss_length, ridge_def_poss_length, def_ridge_off_poss_length,
                poss, completed, train)

# Next game ppp model data
new_ppp_model_dataset <- new_games_off_def %>%
  dplyr::select(-def_location, -neutral, -hour, -dplyr::contains("distance")) %>%
  dplyr::left_join(team_preds_curr %>%
                     dplyr::select(season, team_id, cum_off_poss, off_games_played = games_played,
                                   off_ppp, off_pps, off_epps, off_lme4_epps, off_lme4_ppp,
                                   dplyr::starts_with("ridge_off"), dplyr::contains("_points"), -dplyr::contains("_poss_length")),
                   by = c("season", "off_id" = "team_id")) %>%
  dplyr::left_join(dplyr::rename(lme4_matchup_preds, off_matchup_adj = matchup_adj),
                   by = c("game_id", "off_id" = "team_id")) %>%
  dplyr::left_join(team_preds_curr %>%
                     dplyr::select(season, team_id, cum_def_poss, def_games_played = games_played,
                                   def_ppp, def_pps, def_epps, def_lme4_epps, def_lme4_ppp,
                                   def_ridge_off_ppp = ridge_off_ppp,
                                   dplyr::starts_with("ridge_def"), dplyr::contains("_points"), -dplyr::contains("_poss_length")),
                   by = c("season", "def_id" = "team_id"),
                   suffix = c("_off", "_def")) %>%
  dplyr::left_join(dplyr::rename(lme4_matchup_preds, def_matchup_adj = matchup_adj),
                   by = c("game_id", "def_id" = "team_id")) %>%
  # Preseason AP poll
  dplyr::left_join(collapse::fselect(preseason_ap, season, team_id, off_preseason_ap = preseason_ap), by = c("season", "off_id" = "team_id")) %>%
  dplyr::left_join(collapse::fselect(preseason_ap, season, team_id, def_preseason_ap = preseason_ap), by = c("season", "def_id" = "team_id")) %>%
  dplyr::mutate(conf_game = as.integer(off_conf == def_conf),
                off_matchup_adj_diff = off_matchup_adj - def_matchup_adj,
                ridge_points_off_diff = ridge_points_off - ridge_points_def,
                ridge_off_ppp_diff = ridge_off_ppp + ridge_def_ppp,
                ridge_off_pps_diff = ridge_off_pps + ridge_def_pps,
                off_lme4_ppp_diff = off_lme4_ppp + def_lme4_ppp,
                off_lme4_epps_diff = off_lme4_epps + def_lme4_epps,
                completed = 0L,
                train = 0L) %>%
  dplyr::select(dplyr::any_of(colnames(ppp_model_dataset)))

# Next game possession model data
new_poss_model_dataset <- new_games_off_def %>%
  dplyr::select(-def_location, -neutral, -hour, -dplyr::contains("distance")) %>%
  dplyr::left_join(team_preds_curr %>%
                     dplyr::select(season, team_id, off_games_played = games_played,
                                   off_ppp, off_poss_min, off_poss_length, ridge_off_poss_length),
                   by = c("season", "off_id" = "team_id")) %>%
  dplyr::left_join(team_preds_curr %>%
                     dplyr::select(season, team_id, def_games_played = games_played,
                                   def_ppp, def_poss_min, def_poss_length, ridge_def_poss_length,
                                   def_ridge_off_poss_length = ridge_off_poss_length),
                   by = c("season", "def_id" = "team_id")) %>%
  dplyr::mutate(conf_game = as.integer(off_conf == def_conf), completed = 0L, train = 0L)

cat("Saving efficiency and possession model datasets...\n")

# Save model data
saveRDS(group_conferences(dplyr::bind_rows(poss_model_dataset, new_poss_model_dataset)),
        here::here(opt$gender, "/data/poss_model_dataset.rds"))
saveRDS(group_conferences(dplyr::bind_rows(ppp_model_dataset, new_ppp_model_dataset)),
        here::here(opt$gender, "/data/ppp_model_dataset.rds"))

saveRDS(team_preds_curr, here::here(opt$gender, "/data/current_metrics.rds"))

cat("Saved efficiency and possession model datasets.\n")

# Use schedule to put data in right format for game level predictions
game_model_dataset <- collapse::fsubset(schedule, season >= 2019L) %>%
  # Games with PBP
  dplyr::left_join(dplyr::summarise(dplyr::group_by(game_stats, game_id), train = max(train), .groups = "drop"), by = "game_id") %>%
  # Join in games played
  dplyr::inner_join(collapse::fselect(season_games, game_date, team_id, home_games_played = games_played),
                    by = c("game_date", "home_id" = "team_id")) %>%
  dplyr::inner_join(collapse::fselect(season_games, game_date, team_id, away_games_played = games_played),
                    by = c("game_date", "away_id" = "team_id")) %>%
  # Join in conferences
  dplyr::left_join(collapse::fselect(conferences, season, team_id, home_conf = conf),
                   by = c("season", "home_id" = "team_id")) %>%
  dplyr::left_join(collapse::fselect(conferences, season, team_id, away_conf = conf),
                   by = c("season", "away_id" = "team_id")) %>%
  tidyr::replace_na(list(home_conf = "UNK", away_conf = "UNK")) %>%
  group_conferences(home_away = TRUE) %>%
  # Join in lme4 predictions
  dplyr::left_join(team_preds %>%
                     dplyr::select(game_date, team_id, off_lme4_ppp, def_lme4_ppp) %>%
                     dplyr::rename_with(.cols = dplyr::ends_with("_ppp"), ~ paste0("home_", .)),
                   by = c("game_date", "home_id" = "team_id")) %>%
  dplyr::left_join(team_preds %>%
                     dplyr::select(game_date, team_id, off_lme4_ppp, def_lme4_ppp) %>%
                     dplyr::rename_with(.cols = dplyr::ends_with("_ppp"), ~ paste0("away_", .)),
                   by = c("game_date", "away_id" = "team_id")) %>%
  # Join in rest and back-to-back games data
  dplyr::left_join(rest, by = c("game_id", "home_id", "away_id")) %>%
  # Join in ridge adjusted point differentials
  dplyr::left_join(team_preds %>%
                     dplyr::select(game_date, team_id, dplyr::ends_with("_points")) %>%
                     dplyr::rename_with(.cols = dplyr::starts_with("ridge_"), ~ paste0("home_", .)),
                   by = c("game_date", "home_id" = "team_id")) %>%
  dplyr::left_join(team_preds %>%
                     dplyr::select(game_date, team_id, dplyr::ends_with("_points")) %>%
                     dplyr::rename_with(.cols = dplyr::starts_with("ridge_"), ~ paste0("away_", .)),
                   by = c("game_date", "away_id" = "team_id")) %>%
  # Matchup adjustment predictions
  dplyr::left_join(dplyr::rename(lme4_matchup_preds, home_matchup_adj = matchup_adj), by = c("game_id", "home_id" = "team_id")) %>%
  dplyr::left_join(dplyr::rename(lme4_matchup_preds, away_matchup_adj = matchup_adj), by = c("game_id", "away_id" = "team_id")) %>%
  collapse::fmutate(home_rest_diff = home_rest - away_rest,
                    home_matchup_adj_diff = home_matchup_adj - away_matchup_adj,
                    home_ridge_points_diff = home_ridge_points - away_ridge_points,
                    home_off_lme4_ppp_diff = home_off_lme4_ppp + away_def_lme4_ppp,
                    home_def_lme4_ppp_diff = home_def_lme4_ppp + away_off_lme4_ppp,
                    completed = as.integer(!is.na(score_diff))) %>%
  # Get rid of games in the past with no score information
  collapse::fsubset(!(completed == 0L & game_date < last_game - 5L) & game_date <= Sys.Date() + 2L)

cat("Saving game model dataset...\n")

saveRDS(game_model_dataset, here::here(opt$gender, "/data/game_model_dataset.rds"))

cat("Saved game model dataset.\n")
