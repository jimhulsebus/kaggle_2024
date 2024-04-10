# Men's shot quality model (gamezoneR/ESPN)

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(collapse)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(xgboost)))

option_list <- list(
  make_option("--start_season", type = "character", default = "2017-18", help = "season of data to start from"),
  make_option("--gamezone", type = "logical", default = TRUE, help = "gamezoneR data? default TRUE"),
  make_option("--locations", type = "logical", default = TRUE, help = "shots with charted coordinates only? default TRUE")
)
opt <- parse_args(OptionParser(option_list = option_list))

if (isFALSE(opt$gamezone)) {
  opt$start_season <- as.integer(substr(opt$start_season, start = 1, stop = 4))
}

# Source helpers
source(here::here("utils.R"))
source(here::here("espn_processing.R"))

cat("Loading in all shot data since", opt$start_season, "...\n")

feats <- c("season", "neutral", "three_pt", "play_length",
           "off_home", "winning", "half", "half_secs_remaining",
           "shot_desc_DunkShot", "shot_desc_JumpShot",
           "shot_desc_LayupShot", "shot_desc_TipShot")
target <- "made"

if (isTRUE(opt$gamezone)) {
  avail_sns <- gamezoneR:::available_seasons()

  # Neutral sites
  neutral_sites <- readRDS(here::here("mens/data/joined_schedule.rds")) %>%
    collapse::fsubset(!is.na(gamezone_game_id), game_id = gamezone_game_id, neutral)

  # Load shots
  model_dataset <- gamezoneR::load_gamezone_pbp(avail_sns[which(opt$start_season == avail_sns):length(avail_sns)]) %>%
    collapse::fsubset(!is.na(loc_x), -neutral) %>%
    dplyr::inner_join(neutral_sites, by = "game_id") %>%
    prepare_for_shot_prob(randomize = TRUE, gamezone = TRUE) %>%
    collapse::fselect(season = season_num, game_id, shot_outcome, shot_desc, shot_distance, shot_angle,
                      three_pt, play_length, neutral, home_shot, score_diff,
                      half_secs_remaining, half) %>%
    collapse::fmutate(winning = ifelse(score_diff == 0L, 0L, ifelse(score_diff > 0L, 1L, -1L)),
                      off_home = ifelse(neutral == 1L, 0L, ifelse(home_shot == 1L, 1L, -1L)),
                      made = as.integer(shot_outcome == "made"),
                      shot_desc_DunkShot = as.integer(shot_desc == "DunkShot"),
                      shot_desc_HookShot = as.integer(shot_desc == "HookShot"),
                      shot_desc_JumpShot = as.integer(shot_desc == "JumpShot"),
                      shot_desc_LayupShot = as.integer(shot_desc == "LayupShot"),
                      shot_desc_TipShot = as.integer(shot_desc == "TipShot"))

  feats <- c(feats, "shot_distance", "shot_angle", "shot_desc_HookShot")
} else {

  # Neutral sites
  neutral_sites <- hoopR::load_mbb_schedule(seasons = opt$start_season:hoopR::most_recent_mbb_season()) %>%
    dplyr::transmute(game_id, neutral = as.integer(neutral_site))

  # Load shots
  model_dataset <- hoopR::load_mbb_pbp(seasons = opt$start_season:hoopR::most_recent_mbb_season()) %>%
    process_espn_pbp(womens = FALSE) %>%
    dplyr::inner_join(neutral_sites, by = "game_id") %>%
    prepare_for_shot_prob(randomize = FALSE, gamezone = FALSE) %>%
    collapse::fsubset(!is.na(shot_outcome) & !free_throw) %>%
    collapse::fselect(season, game_id, shot_outcome, shot_desc, shot_distance, shot_angle,
                      three_pt, play_length, neutral, home_shot, score_diff,
                      half_secs_remaining, half) %>%
    collapse::fmutate(winning = ifelse(score_diff == 0L, 0L, ifelse(score_diff > 0L, 1L, -1L)),
                      off_home = ifelse(neutral == 1L, 0L, ifelse(home_shot == 1L, 1L, -1L)),
                      shot_desc = ifelse(shot_desc %in% c("ThreePointJumpShot", "BlockShot"), "JumpShot", shot_desc),
                      made = as.integer(shot_outcome == "made"),
                      shot_desc_DunkShot = as.integer(shot_desc == "DunkShot"),
                      shot_desc_JumpShot = as.integer(shot_desc == "JumpShot"),
                      shot_desc_LayupShot = as.integer(shot_desc %in% c("LayUpShot", "Layup")),
                      shot_desc_TipShot = as.integer(shot_desc == "TipShot"))

  if (isTRUE(opt$locations)) {
    model_dataset <- collapse::fsubset(model_dataset, !is.na(shot_distance))
    feats <- c(feats, "shot_distance", "shot_angle")
  } else {
    model_dataset <- collapse::fsubset(model_dataset, is.na(shot_distance))
  }
}

# Split the data into train and test
set.seed(123)
train <- collapse::fsubset(model_dataset, season < 2022)
test <- collapse::fsubset(model_dataset, season == 2022)

# Make folds
train <- stratified_game_folds(data = train, folds = 10, strata = "season")

folds.list <- list()
for (i in seq_len(length(unique(train$fold)))) {
  folds.list[[i]] <- which(train$fold == i)
}

dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train[, feats]), label = train[[target]])

cv_res <- data.frame()
for (lr in c(0.03, 0.04, 0.05)) {

  for (md in c(6)) {

    cat("LR:", lr, "MD:", md, "...\n")

    params <- list(booster = "gbtree",
                   eta = lr,
                   max_depth = md,
                   eval_metric = "logloss",
                   objective = "binary:logistic")

    # Tune to find best trees
    set.seed(234)
    cv_fit <- xgboost::xgb.cv(params = params,
                              data = dtrain,
                              nrounds = 1000,
                              folds = folds.list,
                              verbose = FALSE,
                              early_stopping_rounds = 20,
                              nthread = 4)

    # Fit with best trees
    xg_fit <- xgboost::xgboost(data = dtrain,
                               params = params,
                               nrounds = cv_fit$best_iteration,
                               nthread = 4,
                               verbose = FALSE)

    # Predict on holdout
    test$pred <- predict(xg_fit, newdata = as.matrix(test[, feats]))
    # MAE
    err <- MLmetrics::LogLoss(y_pred = test$pred, y_true = test[[target]])

    res <- data.frame(lr = lr, md = md, metric = err, rnd = cv_fit$best_iteration)

    cv_res <- dplyr::bind_rows(cv_res, res)
  }
}

# cv_res %>%
# ggplot(aes(lr, metric, color = factor(md))) +
# geom_point(aes(size = rnd)) +
# geom_line()

print(dplyr::arrange(cv_res, metric))

# Best parameters
best <- dplyr::slice_min(cv_res, metric)
# best <- data.frame(lr = 0.03, md = 5, rnd = 814)
train <- collapse::fsubset(model_dataset, season < 2023)
dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train[, feats]), label = train[[target]])

params <- list(booster = "gbtree",
               eta = best$lr,
               max_depth = best$md,
               eval_metric = "logloss",
               objective = "binary:logistic")

# Fit with best trees
xg_fit <- xgboost::xgboost(data = dtrain,
                           params = params,
                           nrounds = best$rnd,
                           nthread = 4,
                           verbose = FALSE)

# Save model fit
saveRDS(xg_fit, here::here(paste0("shot_model/m_shot_model", ifelse(isTRUE(opt$gamezone), "GZ", ifelse(isTRUE(opt$locations), "ESPNLOC", "ESPN")), ".rds")))

quit(status = 0)

# Gamezone
# lr md    metric  rnd
# 0.020  5 0.6255565 1000
# 0.030  5 0.6256783 1000
# 0.030  4 0.6259805 1000
# 0.010  5 0.6261324 1000
# 0.009  5 0.6263747 1000
# 0.020  4 0.6265373 1000
# 0.010  4 0.6274348 1000
# 0.009  4 0.6276659 1000
# 0.030  3 0.6290881 1000
# 0.020  3 0.6295092 1000
# 0.010  3 0.6314804 1000
# 0.009  3 0.6317898 1000

# ESPN no locations
# lr md    metric  rnd
# 0.03  5 0.6305802  999
# 0.04  6 0.6305858 504
# 0.03  6 0.6305878 714
# 0.02  5 0.6306775 1000
# 0.05  6 0.6307183 415
# 0.03  4 0.6313726  999
# 0.02  4 0.6315524 1000
# 0.01  5 0.6319749 1000
# 0.01  4 0.6332384 1000
# 0.03  3 0.6342583 1000
# 0.02  3 0.6349159 1000
# 0.01  3 0.6369156 1000

# ESPN locations
# lr md    metric  rnd
# 0.03  5 0.6232361  814
# 0.03  6 0.6232460 602
# 0.02  5 0.6233120  999
# 0.03  4 0.6234917 1000
# 0.05  6 0.6235158 324
# 0.02  4 0.6236064 1000
# 0.04  6 0.6237638 405
# 0.03  3 0.6250921  999
# 0.01  5 0.6251685 1000
# 0.01  4 0.6266119 1000
# 0.02  3 0.6268584 1000
# 0.01  3 0.6290499 1000

