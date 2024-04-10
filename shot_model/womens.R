# Women's shot quality model

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(collapse)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(tidyr)))

option_list <- list(
  make_option("--start_season", type = "integer", default = 2017, help = "season of data to start from"),
  make_option("--locations", type = "logical", default = TRUE, help = "shots with charted coordinates only? default TRUE")
)
opt <- parse_args(OptionParser(option_list = option_list))

# Source helpers
source(here::here("utils.R"))
source(here::here("espn_processing.R"))

feats <- c("season", "neutral", "three_pt", "play_length",
           "off_home", "winning", "qtr", "qtr_secs_remaining",
           "shot_desc_DunkShot", "shot_desc_JumpShot",
           "shot_desc_LayupShot", "shot_desc_TipShot")
target <- "made"

cat("Loading in all shot data since", opt$start_season, "...\n")

sns <- opt$start_season:wehoop::most_recent_wbb_season()

# Neutral site games
neutral_sites <- wehoop::load_wbb_schedule(seasons = sns) %>%
  dplyr::transmute(game_id, neutral = as.integer(neutral_site))

# Load shots
shots <- wehoop::load_wbb_pbp(seasons = sns) %>%
  process_espn_pbp(womens = TRUE) %>%
  dplyr::inner_join(neutral_sites, by = "game_id") %>%
  prepare_for_shot_prob(randomize = FALSE) %>%
  collapse::fsubset(!is.na(shot_outcome) & !free_throw) %>%
  collapse::fselect(season, game_id, shot_outcome, shot_desc, shot_distance, shot_angle,
                    three_pt, play_length, neutral, home_shot, score_diff,
                    qtr_secs_remaining, half_secs_remaining, qtr)

if (isTRUE(opt$locations)) {
  shots <- collapse::fsubset(shots, !is.na(shot_distance))
  feats <- c(feats, "shot_distance", "shot_angle")
} else {
  shots <- collapse::fsubset(shots, is.na(shot_distance))
}

model_dataset <- shots %>%
  collapse::fmutate(winning = ifelse(score_diff == 0L, 0L, ifelse(score_diff > 0L, 1L, -1L)),
                    off_home = ifelse(neutral == 1L, 0L, ifelse(home_shot == 1L, 1L, -1L)),
                    half = ifelse(qtr <= 2L, 1L, ifelse(qtr <= 4L, 2L, 3L)),
                    shot_desc = ifelse(shot_desc %in% c("ThreePointJumpShot", "BlockShot"), "JumpShot", shot_desc),
                    made = as.integer(shot_outcome == "made"),
                    shot_desc_DunkShot = as.integer(shot_desc == "DunkShot"),
                    shot_desc_JumpShot = as.integer(shot_desc == "JumpShot"),
                    shot_desc_LayupShot = as.integer(shot_desc %in% c("LayUpShot", "Layup")),
                    shot_desc_TipShot = as.integer(shot_desc == "TipShot"))

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
saveRDS(xg_fit, here::here(paste0("shot_model/w_shot_model", ifelse(isTRUE(opt$locations), "LOC", ""), ".rds")))

quit(status = 0)

# ESPN no locations
# lr md    metric  rnd
# 0.04  6 0.6305889 500
# 0.03  6 0.6306049 705
# 0.05  6 0.6306353 395
# 0.03  5 0.6307623  812
# 0.02  5 0.6309266 1000
# 0.03  4 0.6314247  999
# 0.02  4 0.6319770 1000
# 0.01  5 0.6324699 1000
# 0.01  4 0.6339532 1000
# 0.03  3 0.6343539 1000
# 0.02  3 0.6356619 1000
# 0.01  3 0.6368308 1000

# ESPN locations
# lr md    metric  rnd
# 0.03  3 0.6372134  452
# 0.02  3 0.6372991  618
# 0.01  3 0.6375908 1000
# 0.03  4 0.6376148  289
# 0.01  4 0.6378416  961
# 0.02  4 0.6381277  511
# 0.05  2 0.6382741 355
# 0.03  2 0.6383666 593
# 0.04  2 0.6385342 531
# 0.03  5 0.6396752  191
# 0.01  5 0.6400511  537
# 0.02  5 0.6402322  298

# best <- data.frame(lr = 0.03, md = 3, rnd = 452)
