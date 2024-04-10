### Predict game level score differential

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(xgboost)))

option_list <- list(
  make_option("--rerun", type = "logical", default = TRUE, help = "re-run all training?"),
  make_option("--gender", type = "character", default = "mens", help = "mens or womens, default mens")
)
opt <- parse_args(OptionParser(option_list = option_list))

cat(paste0("---", tools::toTitleCase(opt$gender), " game predictions---\n"))

# Load data
load(file = here::here(opt$gender, "/data/loaded.RData"))

# Source helpers
source(here::here("utils.R"))
source(here::here("config.R"))

# Possession model dataset
model_data <- readRDS(here::here(opt$gender, "/data/game_model_dataset_with_team_preds.rds")) %>%
  dplyr::left_join(daynumbers, by = "game_date")

# Grid of date-metric
metric_grid <- date_grid %>%
  collapse::fsubset(game_date >= "2019-11-05") %>%
  collapse::roworder(game_date)

# Model information
feats <- c("season", "postseason", "neutral",
           "home_streak", "away_streak",
           "home_rest", "home_rest_diff",
           "home_games_played", "away_games_played",
           "home_ridge_points", "home_ridge_points_diff",
           "home_off_lme4_ppp", "home_off_lme4_ppp_diff",
           "home_def_lme4_ppp", "home_def_lme4_ppp_diff",
           "home_matchup_adj", "home_matchup_adj_diff",
           "home_pred_ppp", "home_pred_ppp_diff",
           "home_pred_poss", "away_pred_poss")
target <- "score_diff"
monotone <- make_xgb_monotone(feats = feats)

predict_game <- function(rerun = TRUE, day = NULL) {

  if (!is.null(day) & (day %in% start_dates | day >= Sys.Date() - 2)) {
    cat("Date:", as.character(day), "\n")
  }

  # Train is all dates before date of interest
  train <- collapse::fsubset(model_data, game_date < day & completed == 1L & train == 1L)
  # Test is current date
  test <- collapse::fsubset(model_data, game_date == day)

  # Params
  params <- list(booster = "gbtree",
                 eta = xgb_params[[opt$gender]]$game$lr,
                 max_depth = xgb_params[[opt$gender]]$game$md,
                 eval_metric = "mae",
                 objective = "reg:squarederror",
                 monotone_constraints = monotone)

  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train[, feats]), label = train[[target]])

  # If rerun or current games, then fit model
  if (isTRUE(rerun) | day >= Sys.Date()) {
    # Fit model
    set.seed(123)
    fit <- xgboost::xgboost(data = dtrain,
                            params = params,
                            nrounds = xgb_params[[opt$gender]]$game$rnd,
                            nthread = 4L,
                            verbose = FALSE)

    saveRDS(fit, here::here(paste0(opt$gender, "/models/game_level/", day, ".rds")))
  } else {
    fit <- suppressWarnings(try(readRDS(here::here(paste0(opt$gender, "/models/game_level/", day, ".rds"))), silent = TRUE))

    if ("try-error" %in% class(fit)) {
      # Fit model
      set.seed(123)
      fit <- xgboost::xgboost(data = dtrain,
                              params = params,
                              nrounds = xgb_params[[opt$gender]]$game$rnd,
                              nthread = 4L,
                              verbose = FALSE)

      saveRDS(fit, here::here(paste0(opt$gender, "/models/game_level/", day, ".rds")))
    }
  }

  if (nrow(test) > 0L) {
    # Predict on holdout
    test$pred_score_diff <- predict(fit, newdata = as.matrix(test[, feats]))
    preds <- test[, c("game_id", "home_id", "away_id", target, "pred_score_diff")]
  } else {
    preds <- data.frame()
  }

  return(preds)
}

# Predicted expected metrics for each matchup
predictions <- metric_grid %>%
  dplyr::mutate(preds = purrr::pmap(list(game_date),
                                    ~ predict_game(rerun = opt$rerun,
                                                   day = ..1))) %>%
  tidyr::unnest(preds)

saveRDS(predictions, here::here(opt$gender, "/data/game_preds.rds"))

# Mens: 9.40771/11.88228 (9.391954/12.40975)
# Womens: 10.06729/14.03879 (10.21079/15.46242)
predictions %>%
  collapse::fsubset(!is.na(score_diff)) %>%
  dplyr::summarise(games = dplyr::n(),
                   mae = mean(abs(pred_score_diff - score_diff)),
                   sd = sd(pred_score_diff)) %>%
  as.data.frame()

cat("Done with out-of-sample game-level predictions.\n")

quit(status = 0)
