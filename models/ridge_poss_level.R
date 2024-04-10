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

cat(paste0("---", tools::toTitleCase(opt$gender), " possession/shots level ridge regression modeling---\n"))

# Load in data
load(file = here::here(opt$gender, "/data/loaded.RData"))
rm(home_away_dict, game_stats, conferences, new_games, new_games_off_def, lg_avgs, rest)
invisible(gc(verbose = FALSE))

# Source helpers
source(here::here("utils.R"))
source(here::here("config.R"))

possessions_clean$row <- 1:nrow(possessions_clean)
shots_clean$row <- 1:nrow(shots_clean)
for (col in c("season", "off_id", "def_id")) {
  possessions_clean[[col]] <- as.character(possessions_clean[[col]])
  shots_clean[[col]] <- as.character(shots_clean[[col]])
}

# Cap possession length
possessions_clean$poss_length <- pmin(possessions_clean$poss_length, 100L)

# Clean travel distance
possessions_clean <- clean_travel_distance(data = possessions_clean)
shots_clean <- clean_travel_distance(data = shots_clean)

add_interactions <- function(data) {

  data$weight_winning <- data$weight * as.integer(data$score_diff > 0L)
  data$weight_losing <- data$weight * as.integer(data$score_diff < 0L)
  data$tied <- as.integer(data$score_diff == 0L)

  return(as.data.frame(data))
}

possessions_clean <- add_interactions(data = possessions_clean)
shots_clean <- add_interactions(data = shots_clean)
invisible(gc(verbose = FALSE))

cat("Making sparse matrices...\n")

form <- as.formula(~ -1 + season + off_location*off_b2b*def_b2b +
                     off_rest + poly(off_rest, 2) + def_rest + poly(def_rest, 2) +
                     weight_winning + weight_losing + tied +
                     off_distance_away + off_distance_neutral + off_no_travel +
                     def_distance_away + def_distance_neutral + def_no_travel +
                     off_rest_diff +
                     off_id + def_id)

first <- possessions_clean[1:(floor(nrow(possessions_clean)/2L)), ]
second <- dplyr::anti_join(possessions_clean, first[, c("game_id", "off_id", "row")], by = c("game_id", "off_id", "row"))

# Cast into sparse matrix
poss_sparse1 <- Matrix::Matrix(model.matrix(form, first), sparse = TRUE)
poss_sparse2 <- Matrix::Matrix(model.matrix(form, second), sparse = TRUE)
poss_sparse <- suppressWarnings(merge.sparse(list(poss_sparse1, poss_sparse2)))

rm(first, second, poss_sparse1, poss_sparse2)
invisible(gc(verbose = FALSE))

form <- as.formula(~ -1 + season + off_location*off_b2b*def_b2b +
                     weight_winning + weight_losing + tied +
                     off_distance_away + off_distance_neutral + off_no_travel +
                     def_distance_away + def_distance_neutral + def_no_travel +
                     off_rest_diff +
                     off_id + def_id)

first <- shots_clean[1:(floor(nrow(shots_clean)/2L)), ]
second <- dplyr::anti_join(shots_clean, first[, c("game_id", "off_id", "row")], by = c("game_id", "off_id", "row"))

# Cast into sparse matrix
shots_sparse1 <- Matrix::Matrix(model.matrix(form, first), sparse = TRUE)
shots_sparse2 <- Matrix::Matrix(model.matrix(form, second), sparse = TRUE)
shots_sparse <- suppressWarnings(merge.sparse(list(shots_sparse1, shots_sparse2)))

rm(first, second, shots_sparse1, shots_sparse2)
invisible(gc(verbose = FALSE))

# Grid of date-metric
metric_grid <- date_grid %>%
  collapse::fsubset(game_date >= "2018-11-06") %>%
  tidyr::crossing(metric = c("ppp", "pps", "poss_length")) %>%
  collapse::roworder(game_date)

# Adjust efficiency metrics by opponent using ridge model
predict_ridge <- function(rerun = TRUE, metric = c("ppp", "pps", "poss_length"), day = NULL) {

  if (!is.null(day) & (day %in% start_dates | day >= Sys.Date() - 2)) {
    cat("Date:", as.character(day), metric, "ridge poss...\n")
  }

  # Parameters
  metric_params <- params[[opt$gender]][[metric]]

  # Get weights for each date
  weights <- get_recency_weights(df = date_grid, day = day, sn_decay = metric_params$sn_decay, day_decay = metric_params$day_decay)[, c("game_date", "weight")]

  if (metric %in% c("ppp", "poss_length")) {
    model_data <- possessions_clean
    sparse <- poss_sparse
  } else {
    model_data <- shots_clean
    sparse <- shots_sparse
  }

  # Join in weights
  train <- dplyr::left_join(
    collapse::fsubset(model_data, game_date < day & game_date > as.Date(day)-metric_params$days_back),
    dplyr::rename(weights, time_weight = weight),
    by = "game_date"
  ) %>%
    collapse::fmutate(weight_interaction = weight*time_weight) %>%
    collapse::roworder(row)

  wts <- switch(metric,
                "poss_length" = train$time_weight,
                "ppp" = train$time_weight,
                "pps" = train$time_weight)

  # Count rows for off/def
  off_n <- dplyr::count(train, team_id = as.integer(off_id), name = "n")
  off_n$metric <- paste0("ridge_off_", metric)
  def_n <- dplyr::count(train, team_id = as.integer(def_id), name = "n")
  def_n$metric <- paste0("ridge_def_", metric)
  team_n <- collapse::rowbind(off_n, def_n)

  target <- switch (metric,
                    "ppp" = "points",
                    "pps" = "points",
                    "poss_length" = "poss_length"
  )

  # Lower/Upper limits
  lower_limits <- unname(low_limits[colnames(sparse)]) %>% replace(is.na(.), -Inf)
  upper_limits <- unname(up_limits[colnames(sparse)]) %>% replace(is.na(.), Inf)

  # No limits for possession length
  if (metric == "poss_length") {
    lower_limits <- rep(-Inf, length(colnames(sparse)))
    upper_limits <- rep(Inf, length(colnames(sparse)))
  }

  if (isTRUE(rerun) | day >= Sys.Date() - 1L) {
    # Fit model
    set.seed(123)
    fit <- glmnet::glmnet(x = sparse[min(train$row):max(train$row), ],
                          y = train[[target]],
                          lower.limits = lower_limits,
                          upper.limits = upper_limits,
                          alpha = 0,
                          lambda = 1e-7,
                          weights = wts)

    coefs <- broom::tidy(fit)

    saveRDS(coefs, here::here(paste0(opt$gender, "/models/team_ratings/ridge/poss/", metric, day, ".rds")))
  } else {

    coefs <- suppressWarnings(try(readRDS(here::here(paste0(opt$gender, "/models/team_ratings/ridge/poss/", metric, day, ".rds"))), silent = TRUE))

    if ("try-error" %in% class(coefs)) {
      # Fit model
      set.seed(123)
      fit <- glmnet::glmnet(x = sparse[min(train$row):max(train$row), ],
                            y = train[[target]],
                            lower.limits = lower_limits,
                            upper.limits = upper_limits,
                            alpha = 0,
                            lambda = 1e-7,
                            weights = wts)

      coefs <- broom::tidy(fit)

      saveRDS(coefs, here::here(paste0(opt$gender, "/models/team_ratings/ridge/poss/", metric, day, ".rds")))
    }
  }

  # Get team coefficients
  ridge_coefs <- dplyr::bind_rows(
    get_ridge_ceofs(coefs = coefs, side = "off", metric = metric),
    get_ridge_ceofs(coefs = coefs, side = "def", metric = metric)
  ) %>%
    dplyr::left_join(team_n, by = c("team_id", "metric")) %>%
    replace(is.na(.), 0)

  # Fill and pad
  ridge_coefs$pred <- ifelse(stringr::str_detect(ridge_coefs$metric, "off"),
                             ((ridge_coefs$pred * ridge_coefs$n) + (metric_params$fill_o * metric_params$pad_o))/(ridge_coefs$n + metric_params$pad_o),
                             ((ridge_coefs$pred * ridge_coefs$n) + (metric_params$fill_d * metric_params$pad_d))/(ridge_coefs$n + metric_params$pad_d))

  return(ridge_coefs)
}

set.seed(234)
predictions <- metric_grid %>%
  dplyr::mutate(preds = purrr::map2(metric, game_date,
                                    ~ predict_ridge(rerun = opt$rerun,
                                                    metric = .x,
                                                    day = .y))) %>%
  dplyr::select(-c(metric)) %>%
  tidyr::unnest(preds)

# Save predicted ridge regression adjustments
saveRDS(predictions, here::here(opt$gender, "/data/ridge_preds_poss_level.rds"))

cat("Done with possession/shots level ridge predictions.\n")

quit(status = 0)

