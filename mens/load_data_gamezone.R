### Load gamezoneR data

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(collapse)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(xgboost)))

option_list <- list(
  make_option("--start_season", type = "character", default = "2017-18", help = "season of data to start from")
)
opt <- parse_args(OptionParser(option_list = option_list))
# Rscript load_data.R --season 2017-18

# Source helpers
source(here::here("utils.R"))
source(here::here("espn_processing.R"))

cat("Loading in all play-by-play data since", opt$start_season, "...\n")

avail_sns <- gamezoneR:::available_seasons()

# Load play-by-play
gamezone_pbp <- gamezoneR::load_gamezone_pbp(avail_sns[which(opt$start_season == avail_sns):length(avail_sns)])

# Only keep days up until data feed is cut out (1/12/24)
valid_days <- dplyr::distinct(gamezone_pbp, season, date) %>%
  dplyr::group_by(season) %>%
  dplyr::mutate(daynum = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(max_daynum = max(daynum[season == max(season)])) %>%
  collapse::fsubset(daynum <= max_daynum, season, date)

gamezone_pbp <- dplyr::inner_join(gamezone_pbp, valid_days, by = c("season", "date"))

cat("Wrangling play-by-play data...\n")

# Dictionary of home and away IDs from play-by-play
pbp_home_away <- gamezone_pbp %>%
  dplyr::distinct(season, game_date = date, game_id, home, away, team_id, event_team) %>%
  collapse::fsubset(!is.na(team_id)) %>%
  collapse::fmutate(type = ifelse(event_team == home, "home_id", "away_id")) %>%
  collapse::fselect(-event_team) %>%
  tidyr::pivot_wider(names_from = type, values_from = team_id) %>%
  collapse::fselect(season, game_date, game_id, home_id, away_id)

# Possessions level points
gamezone_possessions <- gamezone_pbp %>%
  dplyr::inner_join(pbp_home_away, by = c("season", "date" = "game_date", "game_id")) %>%
  collapse::fmutate(
    points = dplyr::case_when(
      is.na(shot_outcome) ~ 0L,
      shot_outcome == "made" & free_throw ~ 1L,
      shot_outcome == "made" & three_pt ~ 3L,
      shot_outcome == "made" ~ 2L,
      TRUE ~ 0L)) %>%
  process_possessions()

# Minutes played each game
gamezone_game_minutes <- gamezone_pbp %>%
  dplyr::distinct(game_id, half) %>%
  collapse::fmutate(minutes = ifelse(half <= 2L, 20L, 5L)) %>%
  dplyr::group_by(game_id) %>%
  dplyr::summarise(minutes = sum(minutes),
                   .groups = "drop") %>%
  collapse::fsubset(minutes >= 40L)

# Load shot probability model
shot_model <- readRDS(here::here("shot_model/m_shot_modelGZ.rds"))

# Filter for just shots
gamezone_shots <- gamezone_pbp %>%
  collapse::fsubset(!is.na(loc_x)) %>%
  prepare_for_shot_prob(randomize = TRUE, gamezone = TRUE) %>%
  collapse::fmutate(season = season_num,
                    winning = as.integer(score_diff > 0L),
                    off_home = ifelse(neutral == 1L, 0L, ifelse(home_shot == 1L, 1L, -1L)),
                    shot_desc = ifelse(shot_desc %in% c("ThreePointJumpShot", "BlockShot"), "JumpShot", shot_desc),
                    shot_desc_DunkShot = as.integer(shot_desc == "DunkShot"),
                    shot_desc_HookShot = as.integer(shot_desc == "HookShot"),
                    shot_desc_JumpShot = as.integer(shot_desc == "JumpShot"),
                    shot_desc_LayupShot = as.integer(shot_desc == "LayupShot"),
                    shot_desc_TipShot = as.integer(shot_desc == "TipShot")) %>%
  dplyr::select(dplyr::all_of(c(
    "game_id", "team_id", shot_model$feature_names, "shot_value", "score_diff", "shot_outcome", "game_secs_remaining"
  )))

cat("Shot predictions...\n")

# Get shot probability predictions
gamezone_shots$pred_make <- predict(shot_model, newdata = as.matrix(gamezone_shots[, shot_model$feature_names]), type = "prob")

# Clean shot level data
gamezone_shots <- gamezone_shots %>%
  dplyr::transmute(game_id, team_id, shot_outcome, shot_prob = pred_make,
                   points = shot_value * as.integer(shot_outcome == "made"),
                   epps = shot_value * shot_prob,
                   game_secs_remaining, score_diff,
                   sdv_time = pmin(15, abs(score_diff/sqrt(1 + game_secs_remaining))),
                   weight = 1/(sdv_time + 1))

# Dictionary of teams
team_dictionary <- gamezone_pbp %>%
  collapse::fsubset(!is.na(team_id)) %>%
  dplyr::group_by(team_id) %>%
  dplyr::summarise(team_name = collapse::fmode(event_team),
                   .groups = "drop")

# Dictionary of all possible team names
gamezone_teams <- gamezone_pbp %>%
  collapse::fsubset(!is.na(team_id)) %>%
  dplyr::distinct(team_name = event_team, team_id) %>%
  collapse::fselect(gamezone_id = team_id, team_name)

# Dictionary of neutral games from play-by-play
neutral_games <- dplyr::distinct(gamezone_pbp, game_id, neutral)

# Scores from play-by-play to fill in missing schedule values
gamezone_pbp_totals <- gamezone_pbp %>%
  dplyr::group_by(game_id) %>%
  dplyr::summarise(home_total = max(home_score),
                   away_total = max(away_score),
                   .groups = "drop") %>%
  dplyr::inner_join(pbp_home_away, by = "game_id")

double_gamezone_pbp_totals <- dplyr::bind_rows(
  collapse::fselect(gamezone_pbp_totals, game_id, team_id = home_id, score = home_total),
  collapse::fselect(gamezone_pbp_totals, game_id, team_id = away_id, score = away_total)
)

cat("Loading in schedule since", opt$start_season, "...\n")

# Read in team page schedules
team_schedule_pages <- readRDS(here::here("mens/data/schedule_pages.rds")) %>%
  collapse::fsubset(!is.na(game_id)) %>%
  collapse::fmutate(season = as.integer(stringr::str_sub(game_date, end = 4)),
                    month = as.integer(stringr::str_sub(game_date, start = 6, end = 7)),
                    season = ifelse(month >= 10,
                                    paste0(season, "-", stringr::str_sub(season + 1, start = 3)),
                                    paste0(season - 1, "-", stringr::str_sub(season, start = 3))),
                    neutral = as.integer(location == "neutral"),
                    team = dplyr::case_when(
                      team == "App State" ~ "Appalachian State",
                      team == "Long State Beach" ~ "Long Beach State",
                      TRUE ~ team
                    )) %>%
  cbbdbR::join_cbb_team_info(join_name = "team")

# Scores from team schedule pages
team_schedule_totals <- collapse::fselect(team_schedule_pages, game_id, team_id = game_zone_id, score = team_score) %>%
  dplyr::group_by(game_id, team_id) %>%
  dplyr::slice_max(score, n = 1L, with_ties = FALSE) %>%
  dplyr::ungroup()

# Full join total scores
all_totals <- dplyr::full_join(double_gamezone_pbp_totals, team_schedule_totals, by = c("game_id", "team_id")) %>%
  # Default is scraped from team page schedule, else pbp
  collapse::fmutate(score = ifelse(is.na(score.y), score.x, score.y)) %>%
  collapse::fselect(game_id, team_id, score)

schedule_pages <- team_schedule_pages %>%
  dplyr::distinct(season, game_id, game_date, team_id = game_zone_id, location, page_neutral = neutral) %>%
  collapse::roworder(game_id, location) %>%
  dplyr::group_by(game_id) %>%
  dplyr::mutate(num = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  collapse::fmutate(type = ifelse(num == 1, "away_id", "home_id")) %>%
  collapse::fselect(-c(location, num)) %>%
  tidyr::pivot_wider(names_from = type, values_from = team_id)

# Read in schedule
gamezone_schedule <- dplyr::bind_rows(
  readr::read_csv("https://raw.githubusercontent.com/JackLich10/gamezoneR-data/main/data/schedules/master_schedule_17_21.csv",
                  col_types = readr::cols()),
  readr::read_csv("https://raw.githubusercontent.com/JackLich10/gamezoneR-data/main/data/schedules/master_schedule_21_22.csv",
                  col_types = readr::cols()),
  readr::read_csv("https://raw.githubusercontent.com/JackLich10/gamezoneR-data/main/data/schedules/master_schedule_22_23.csv",
                  col_types = readr::cols()),
  readr::read_csv("https://raw.githubusercontent.com/JackLich10/gamezoneR-data/main/data/schedules/master_schedule_23_24.csv",
                  col_types = readr::cols())
) %>%
  # COVID cancellations
  collapse::fsubset(!as.character(game_date) %in% c("2020-03-12", "2020-03-13", "2020-03-14")) %>%
  dplyr::mutate(dplyr::across(c(home, away), ~ ifelse(. == "TBA", NA_character_, .))) %>%
  collapse::fselect(season, game_id, game_date, home_id, away_id,
                    home, away, home_abbr, away_abbr, home_total, away_total) %>%
  dplyr::full_join(pbp_home_away, by = c("season", "game_date", "game_id")) %>%
  dplyr::mutate(home_id = ifelse(is.na(home_id.y), home_id.x, home_id.y),
                away_id = ifelse(is.na(away_id.y), away_id.x, away_id.y)) %>%
  dplyr::select(-c(dplyr::ends_with(".x"), dplyr::ends_with(".y"))) %>%
  dplyr::full_join(schedule_pages, by = c("season", "game_date", "game_id")) %>%
  dplyr::mutate(home_id = ifelse(is.na(home_id.x), home_id.y, home_id.x),
                away_id = ifelse(is.na(away_id.x), away_id.y, away_id.x)) %>%
  dplyr::select(-c(dplyr::ends_with(".x"), dplyr::ends_with(".y"))) %>%
  dplyr::left_join(all_totals, by = c("game_id", "home_id" = "team_id")) %>%
  dplyr::left_join(all_totals, by = c("game_id", "away_id" = "team_id")) %>%
  dplyr::mutate(home_total2 = ifelse(!is.na(score.x), score.x, home_total),
                away_total2 = ifelse(!is.na(score.y), score.y, away_total),
                home_score = pmax(home_total, home_total2, na.rm = TRUE),
                away_score = pmax(away_total, away_total2, na.rm = TRUE)) %>%
  dplyr::left_join(neutral_games, by = "game_id") %>%
  tidyr::replace_na(list(neutral = 0L, page_neutral = 0L)) %>%
  collapse::fmutate(score_diff = home_score - away_score,
                    neutral = as.integer(neutral + page_neutral > 0L)) %>%
  dplyr::select(-c(dplyr::ends_with(".x"), dplyr::ends_with(".y"), dplyr::ends_with("2"), page_neutral)) %>%
  collapse::fsubset(!is.na(game_id) & !is.na(season) & !(is.na(score_diff) & game_date < Sys.Date() - 5)) %>%
  dplyr::inner_join(valid_days, by = c("season", "game_date" = "date")) %>%
  collapse::roworder(game_date)

cat("Saving GameZone data...\n")

# Clean environment
rm(list = setdiff(ls(), c("gamezone_schedule",
                          "gamezone_shots",
                          "gamezone_possessions",
                          "gamezone_game_minutes",
                          "gamezone_teams")))

# Save .RData
save(gamezone_schedule, gamezone_shots,
     gamezone_possessions, gamezone_game_minutes,
     gamezone_teams,
     file = here::here("mens/data/loadedGZ.RData"))

cat("Done with data load.\n")
