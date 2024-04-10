### Predict team level offensive efficiency and offensive possessions

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

cat(paste0("---", tools::toTitleCase(opt$gender), " team efficiency and pace predictions---\n"))

# Load data
load(file = here::here(opt$gender, "/data/loaded.RData"))

# Source helpers
source(here::here("utils.R"))
source(here::here("config.R"))

# Possession model dataset
poss_model_dataset <- readRDS(here::here(opt$gender, "/data/poss_model_dataset.rds")) %>%
  dplyr::left_join(daynumbers, by = "game_date")

# Efficiency model dataset
ppp_model_dataset <- readRDS(here::here(opt$gender, "/data/ppp_model_dataset.rds")) %>%
  dplyr::left_join(daynumbers, by = "game_date")

# Make dummies
ppp_model_dataset <- make_off_location_dummies(dat = ppp_model_dataset)
poss_model_dataset <- make_off_location_dummies(dat = poss_model_dataset)

ppp_model_dataset <- make_conf_dummies(dat = ppp_model_dataset)
poss_model_dataset <- make_conf_dummies(dat = poss_model_dataset)

# Grid of date-metric
metric_grid <- date_grid %>%
  collapse::fsubset(game_date >= "2019-01-01") %>%
  tidyr::crossing(metric = c("ppp", "poss")) %>%
  collapse::roworder(game_date)

# Model information
base_feats <- c("season", "postseason", "off_home",
                "conf_game", "off_conf_NonPower", "def_conf_NonPower",
                "off_games_played", "def_games_played",
                "off_rest", "off_rest_diff",
                "off_ppp", "def_ppp")
ppp_feats <- c("off_preseason_ap", "def_preseason_ap",
               "off_b2b", "def_b2b",
               "off_pps", "off_epps",
               "off_lme4_epps", "off_lme4_epps_diff",
               "off_lme4_ppp", "off_lme4_ppp_diff",
               "off_matchup_adj", "off_matchup_adj_diff",
               "ridge_off_ppp", "ridge_off_ppp_diff",
               "ridge_off_pps", "ridge_off_pps_diff",
               "ridge_points_off", "ridge_points_off_diff",
               "def_pps", "def_epps")
poss_feats <- c("off_streak", "def_streak",
                "off_poss_min", "off_poss_length", "ridge_off_poss_length",
                "def_poss_min", "def_poss_length", "ridge_def_poss_length",
                "def_ridge_off_poss_length")

# Predict team level efficiency/pace
predict_eff_poss <- function(rerun = TRUE, metric = c("ppp", "poss"), day = NULL) {

  if (!is.null(day) & (day %in% start_dates | day >= Sys.Date() - 2L)) {
    cat("Date:", as.character(day), metric, "\n")
  }

  # Use right model data
  if (metric == "ppp") {
    model_data <- ppp_model_dataset
    feats <- c(base_feats, ppp_feats)
    monotone <- make_xgb_monotone(feats = feats)
  } else if (metric == "poss") {
    model_data <- poss_model_dataset
    feats <- c(base_feats, poss_feats)
    monotone <- rep(0, length(feats))
  }

  # Train/test split
  test <- collapse::fsubset(model_data, game_date == day)
  if (unique(test$season) == min(model_data$season)) {
    train <- collapse::fsubset(model_data, (game_date < day | (season != unique(test$season) & season <= min(model_data$season)+2L & daynum < unique(test$daynum))) & completed == 1L & train == 1L)
  } else {
    train <- collapse::fsubset(model_data, game_date < day & completed == 1L & train == 1L)
  }

  # Params
  params <- list(booster = "gbtree",
                 eta = xgb_params[[opt$gender]][[metric]]$lr,
                 max_depth = xgb_params[[opt$gender]][[metric]]$md,
                 eval_metric = "mae",
                 objective = "reg:squarederror",
                 monotone_constraints = monotone)

  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train[, feats]), label = train[[metric]])

  if (isTRUE(rerun) | day >= Sys.Date()) {
    # Fit model
    set.seed(123)
    fit <- xgboost::xgboost(data = dtrain,
                            params = params,
                            nrounds = xgb_params[[opt$gender]][[metric]]$rnd,
                            nthread = 4L,
                            verbose = FALSE)

    saveRDS(fit, here::here(paste0(opt$gender, "/models/team_level/", metric, day, ".rds")))
  } else {

    fit <- suppressWarnings(try(readRDS(here::here(paste0(opt$gender, "/models/team_level/", metric, day, ".rds"))), silent = TRUE))

    if ("try-error" %in% class(fit)) {
      # Fit model
      set.seed(123)
      fit <- xgboost::xgboost(data = dtrain,
                              params = params,
                              nrounds = xgb_params[[opt$gender]][[metric]]$rnd,
                              nthread = 4L,
                              verbose = FALSE)

      saveRDS(fit, here::here(paste0(opt$gender, "/models/team_level/", metric, day, ".rds")))
    }
  }

  if (nrow(test) > 0L) {
    # Predict on holdout
    test$pred <- predict(fit, newdata = as.matrix(test[, feats]))
    test$target <- test[[metric]]
    preds <- test[, c("game_id", "off_id", "def_id", "pred", "target")]
  } else {
    preds <- data.frame()
  }

  return(preds)
}

set.seed(234)
predictions <- metric_grid %>%
  dplyr::mutate(preds = purrr::pmap(list(metric, game_date),
                                    ~ predict_eff_poss(rerun = opt$rerun,
                                                       metric = ..1,
                                                       day = ..2))) %>%
  tidyr::unnest(preds)

# Save predicted efficiency/possession metrics
saveRDS(predictions, here::here(opt$gender, "/data/ppp_poss_preds.rds"))

cat("Done with out-of-sample team-level efficiency/pace predictions...\n")

predictions <- readRDS(here::here(opt$gender, "/data/ppp_poss_preds.rds"))

# Mens:
## poss: 3.9703299/3.55758488
## ppp: 0.1109288/0.09948666
# Womens:
## poss: 3.8735022/3.9541799
## ppp: 0.1089437/0.1132077
predictions %>%
  collapse::fsubset(!is.na(target)) %>%
  dplyr::group_by(metric) %>%
  dplyr::summarise(team_games = dplyr::n(),
                   mae = mean(abs(pred - target)),
                   sd = sd(pred)) %>%
  as.data.frame()

cat("Creating game-level model dataset...\n")

# Pivot predictions wide
predictions_wide <- predictions %>%
  dplyr::select(game_id, team_id = off_id, metric, pred) %>%
  tidyr::pivot_wider(names_from = metric, values_from = pred) %>%
  dplyr::rename(pred_poss = poss, pred_ppp = ppp)

# game model dataset
game_model_dataset <- readRDS(here::here(opt$gender, "/data/game_model_dataset.rds"))

game_model_dataset <- game_model_dataset %>%
  collapse::fsubset(game_date >= "2019-01-01") %>%
  # Join in efficiency and possession predictions
  dplyr::left_join(predictions_wide %>%
                     dplyr::rename_with(.cols = dplyr::starts_with("pred_"), ~ paste0("home_", .)),
                   by = c("game_id", "home_id" = "team_id")) %>%
  dplyr::left_join(predictions_wide %>%
                     dplyr::rename_with(.cols = dplyr::starts_with("pred_"), ~ paste0("away_", .)),
                   by = c("game_id", "away_id" = "team_id")) %>%
  collapse::fmutate(home_pred_ppp_diff = home_pred_ppp - away_pred_ppp)

# Save game level model data
saveRDS(game_model_dataset, here::here(opt$gender, "/data/game_model_dataset_with_team_preds.rds"))

cat("Done with team-level predictions and final game model dataset creation.\n")

quit(status = 0)
