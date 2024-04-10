### Matchup team effects (playing up/down to opponent)

# Load libraries
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(tidyr))

option_list <- list(
  make_option("--rerun", type = "logical", default = TRUE, help = "re-run all training?"),
  make_option("--gender", type = "character", default = "mens", help = "mens or womens, default mens")
)
opt <- parse_args(OptionParser(option_list = option_list))

# Source helpers
source(here::here("utils.R"))
source(here::here("config.R"))

cat(paste0("---", tools::toTitleCase(opt$gender), " opponent matchup mixed effects (lme4) adjustment---\n"))

# Load in data
load(file = here::here(opt$gender, "/data/loaded.RData"))
rm(home_away_dict, game_stats, conferences, new_games, new_games_off_def, possessions_clean, shots_clean)
invisible(gc(verbose = FALSE))

# Read in ridge regression predictions
ridge_game_preds <- readRDS(here::here(opt$gender, "/data/ridge_preds_game_level.rds")) %>%
  collapse::fselect(game_date, team_id, ridge_points)

# Process schedule
schedule_proc <- schedule %>%
  collapse::fsubset(game_date >= min(ridge_game_preds$game_date)) %>%
  dplyr::inner_join(daynumbers, by = "game_date") %>%
  dplyr::left_join(ridge_game_preds, by = c("game_date", "home_id" = "team_id")) %>%
  dplyr::left_join(ridge_game_preds, by = c("game_date", "away_id" = "team_id"), suffix = c("_h", "_a")) %>%
  dplyr::left_join(rest, by = c("game_id", "home_id", "away_id")) %>%
  tidyr::replace_na(list(ridge_points_h = params[[opt$gender]]$game$fill,
                         ridge_points_a = params[[opt$gender]]$game$fill)) %>%
  collapse::fmutate(home_rest_diff = home_rest - away_rest,
                    home_travel_adv = away_distance_from_home - home_distance_from_home)

metric_grid <- collapse::fsubset(date_grid, game_date >= "2018-02-01")

# Baseline expected score modeling
expected_scores <- data.frame()
for (sn in unique(metric_grid$season)) {
  for (day in collapse::fsubset(metric_grid, season == sn, game_date)$game_date) {

    test <- collapse::fsubset(schedule_proc, game_date == day)
    if (sn == min(schedule_proc$season)) {
      train <- collapse::fsubset(schedule_proc, (game_date < day | (season != sn & season <= min(schedule$season)+4L & daynum < unique(test$daynum))) & !is.na(score_diff))
    } else {
      train <- collapse::fsubset(schedule_proc, game_date < day & !is.na(score_diff))
    }

    if (nrow(train) < 250L) next

    set.seed(234)
    mod <- lm(score_diff ~ factor(neutral) + ridge_points_a + ridge_points_h + home_rest_diff + home_travel_adv, data = train)

    test$pred <- unname(predict(mod, newdata = test))

    expected_scores <- dplyr::bind_rows(expected_scores, test[, c("season", "neutral", "game_id", "game_date", "daynum", "home_id", "away_id", "ridge_points_h", "ridge_points_a", "score_diff", "pred")])
  }
}

# Make one row per team
double_expected_scores <- dplyr::bind_rows(
  dplyr::transmute(expected_scores, season, neutral, home = 1L, game_date, daynum, game_id, team_id = home_id, opp_ridge_points = ridge_points_a, baseline_game_pred = pred, score_diff),
  dplyr::transmute(expected_scores, season, neutral, home = 0L, game_date, daynum, game_id, team_id = away_id, opp_ridge_points = ridge_points_h, baseline_game_pred = -pred, score_diff = -score_diff),
) %>%
  collapse::roworder(game_id, game_date) %>%
  collapse::fmutate(off_location = ifelse(neutral == 1L, "neutral", ifelse(home == 1L, "home", "away")),
                    score_diff_oe = pmin(40, pmax(-40, score_diff - baseline_game_pred)),
                    completed = as.integer(!is.na(score_diff)))

predict_lme4 <- function(rerun = TRUE, day = NULL) {

  if (!is.null(day) & (day %in% start_dates | day >= Sys.Date() - 2)) {
    cat("Date:", as.character(day), "lme4 matchup...\n")
  }

  train <- collapse::fsubset(double_expected_scores, game_date < day & completed == 1L)
  test <- collapse::fsubset(double_expected_scores, game_date == day)

  if (nrow(train) < 1000L) return(data.frame())

  # Count rows for team-season
  n <- dplyr::count(collapse::fsubset(train, season == max(season)), team_id)

  form <- as.formula(paste0("score_diff_oe ~ off_location + ",
                            ifelse(length(unique(train$season)) == 1L,
                                   "(1 + opp_ridge_points | team_id)",
                                   "(1 + opp_ridge_points | team_id:season)")))

  if (isTRUE(rerun) | day >= Sys.Date()) {
    set.seed(345)
    fit <- lme4::lmer(formula = form,
                      data = train,
                      # weights = train$weight,
                      control = lme4::lmerControl(calc.derivs = FALSE))

    saveRDS(fit, here::here(paste0(opt$gender, "/models/team_ratings/lme4/matchup/", day, ".rds")))
  } else {
    fit <- suppressWarnings(try(readRDS(here::here(paste0(opt$gender, "/models/team_ratings/lme4/matchup/", day, ".rds"))), silent = TRUE))

    if ("try-error" %in% class(fit)) {
      set.seed(345)
      fit <- lme4::lmer(formula = form,
                        data = train,
                        # weights = train$weight,
                        control = lme4::lmerControl(calc.derivs = FALSE))

      saveRDS(fit, here::here(paste0(opt$gender, "/models/team_ratings/lme4/matchup/", day, ".rds")))
    }
  }
  test$matchup_adj <- unname(predict(fit, newdata = test, allow.new.levels = TRUE))
  test <- dplyr::left_join(test, n, by = "team_id")
  test$n <- dplyr::coalesce(test$n, 0L)

  return(test[, c("game_id", "team_id", "n", "opp_ridge_points", "baseline_game_pred", "matchup_adj", "score_diff_oe")])
}

set.seed(234)
team_matchup_predictions <- date_grid %>%
  collapse::fsubset(game_date >= "2018-11-06") %>%
  dplyr::mutate(preds = purrr::map(game_date,
                                   ~ predict_lme4(rerun = opt$rerun,
                                                  day = .x))) %>%
  tidyr::unnest(preds)

team_matchup_predictions$matchup_adj <- (team_matchup_predictions$matchup_adj*(team_matchup_predictions$n + params[[opt$gender]]$matchup$pad_n) + params[[opt$gender]]$matchup$pad*params[[opt$gender]]$matchup$fill) / (team_matchup_predictions$n + params[[opt$gender]]$matchup$pad_n + params[[opt$gender]]$matchup$pad)

# Save predicted lme4 team matchup adjustments
saveRDS(team_matchup_predictions, here::here(opt$gender, "/data/lme4_preds_matchup.rds"))

cat("Done with lme4 team level matchup adjustment predictions.\n")

quit(status = 0)
