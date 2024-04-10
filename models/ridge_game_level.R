# Load libraries
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(tidyr))

option_list <- list(
  make_option("--rerun", type = "logical", default = TRUE, help = "re-run all training?"),
  make_option("--gender", type = "character", default = "mens", help = "mens or womens, default mens")
)
opt <- parse_args(OptionParser(option_list = option_list))

cat(paste0("---", tools::toTitleCase(opt$gender), " game-level ridge regression modeling---\n"))

# Load in data
load(file = here::here(opt$gender, "/data/loaded.RData"))
rm(home_away_dict, game_stats, conferences, new_games, new_games_off_def, lg_avgs)
invisible(gc(verbose = FALSE))

# Source helpers
source(here::here("utils.R"))
source(here::here("config.R"))

model_data <- dplyr::inner_join(schedule, rest, by = c("game_id", "home_id", "away_id")) %>%
  collapse::fmutate(h_id = home_id,
                    a_id = away_id,
                    home_rest_diff = home_rest - away_rest,
                    home_rest_sq = home_rest^2,
                    away_rest_sq = away_rest^2,
                    score_diff_cap = pmin(80, pmax(-60, score_diff)),
                    completed = as.integer(!is.na(score_diff))) %>%
  clean_travel_distance(home_away = TRUE)
model_data$home_distance_adv_neutral <- model_data$away_distance_neutral - model_data$home_distance_neutral
model_data$row <- 1:nrow(model_data)

make_team_dummies <- function(dat) {
  dat %>%
    tidyr::pivot_longer(cols = c(h_id, a_id), names_to = "type", values_to = "team_id") %>%
    collapse::fmutate(value = ifelse(type == "h_id", 1L, -1L)) %>%
    collapse::fselect(-type) %>%
    tidyr::pivot_wider(names_from = team_id, values_from = value, values_fill = 0L, names_prefix = "team_id")
}

model_data_proc <- model_data %>%
  dplyr::select(row, game_date, season, neutral, b2b_home, b2b_road,
                home_rest, home_rest_sq, away_rest, away_rest_sq, home_rest_diff,
                home_distance_adv_neutral, away_distance_away,
                home_no_travel, away_no_travel, h_id, a_id, score_diff_cap)

# Grid of date-metric
metric_grid <- date_grid %>%
  collapse::fsubset(game_date >= "2018-01-01") %>%
  collapse::roworder(game_date)

# Parameters
target <- "score_diff_cap"

# Adjust efficiency metrics by opponent using ridge model
predict_ridge <- function(rerun = TRUE, day = NULL) {

  if (!is.null(day) & (day %in% start_dates | day >= Sys.Date() - 2)) {
    cat("Date:", as.character(day), "ridge game...\n")
  }

  # day = "2018-11-18"

  # Get weights for each date
  weights <- get_recency_weights(df = date_grid, day = day, sn_decay = params[[opt$gender]]$game$sn_decay, day_decay = params[[opt$gender]]$game$day_decay)[, c("game_date", "weight")]

  # Join in weights
  train <- dplyr::inner_join(
    # Only keep games within days_back
    collapse::fsubset(model_data, game_date < day & game_date > as.Date(day)-params[[opt$gender]]$game$days_back & completed == 1L),
    weights,
    by = "game_date"
  )

  # Team dummies
  train <- make_team_dummies(dat = train)

  # Season dummies (ladder)
  for (sn in unique(train$season)) {
    if (sn == min(train$season)) next
    train[[paste0("sn_", sn)]] <- as.integer(train$season >= sn)
  }

  # Count rows for home/away
  home_n <- dplyr::count(train, team_id = as.integer(home_id), name = "home_games")
  away_n <- dplyr::count(train, team_id = as.integer(away_id), name = "away_games")

  # Features
  feats <- c("neutral", "b2b_home", "b2b_road",
             "home_distance_adv_neutral", "away_distance_away",
             "home_no_travel", "away_no_travel", "home_rest_diff",
             "home_rest", "away_rest", "home_rest_sq", "away_rest_sq",
             stringr::str_subset(colnames(train), "sn_"),
             stringr::str_subset(colnames(train), "team_id"))

  # Lower/Upper limits
  lower_limits <- unname(low_limits[feats]) %>% replace(is.na(.), -Inf)
  upper_limits <- unname(up_limits[feats]) %>% replace(is.na(.), Inf)

  if (isTRUE(rerun) | day >= Sys.Date() - 1L) {
    # Fit best model via CV parameters
    set.seed(123)
    fit <- glmnet::cv.glmnet(x = Matrix::Matrix(as.matrix(train[, feats]), sparse = TRUE),
                             y = train[[target]],
                             lower.limits = lower_limits,
                             upper.limits = upper_limits,
                             alpha = 0,
                             weights = train$weight)

    coefs <- collapse::fsubset(broom::tidy(fit$glmnet.fit), lambda == fit$lambda.min)

    saveRDS(coefs, here::here(paste0(opt$gender, "/models/team_ratings/ridge/game/", day, ".rds")))
  } else {

    coefs <- suppressWarnings(try(readRDS(here::here(paste0(opt$gender, "/models/team_ratings/ridge/game/", day, ".rds"))), silent = TRUE))

    if ("try-error" %in% class(coefs)) {
      # Fit best model via CV parameters
      set.seed(123)
      fit <- glmnet::cv.glmnet(x = Matrix::Matrix(as.matrix(train[, feats]), sparse = TRUE),
                               y = train[[target]],
                               lower.limits = lower_limits,
                               upper.limits = upper_limits,
                               alpha = 0,
                               weights = train$weight)

      coefs <- collapse::fsubset(broom::tidy(fit$glmnet.fit), lambda == fit$lambda.min)

      saveRDS(coefs, here::here(paste0(opt$gender, "/models/team_ratings/ridge/game/", day, ".rds")))
    }
  }

  # Get team coefficients
  ridge_coefs <- get_ridge_ceofs(coefs = coefs, metric = "points") %>%
    tidyr::pivot_wider(names_from = metric, values_from = pred, values_fill = 0) %>%
    dplyr::left_join(home_n, by = "team_id") %>%
    dplyr::left_join(away_n, by = "team_id") %>%
    replace(is.na(.), 0)

  # Pad and fill
  ridge_coefs$ridge_points <- (ridge_coefs$ridge_points * (ridge_coefs$home_games + ridge_coefs$away_games) + (params[[opt$gender]]$game$fill * params[[opt$gender]]$game$pad))/(ridge_coefs$home_games + ridge_coefs$away_games + params[[opt$gender]]$game$pad)

  # ridge_coefs %>%
  #   left_join(teams) %>% View()

  return(ridge_coefs)
}

set.seed(234)
predictions <- metric_grid %>%
  dplyr::mutate(preds = purrr::map(game_date,
                                   ~ predict_ridge(rerun = opt$rerun,
                                                   day = .))) %>%
  tidyr::unnest(preds)

# Save predicted ridge team coeficients
saveRDS(predictions, here::here(opt$gender, "/data/ridge_preds_game_level.rds"))

cat("Done with game-level ridge predictions.\n")

quit(status = 0)
