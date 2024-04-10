# Load libraries
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(tidyr))

option_list <- list(
  make_option("--rerun", type = "logical", default = TRUE, help = "re-run all training?"),
  make_option("--gender", type = "character", default = "mens", help = "mens or womens, default mens"),
  make_option("--season", type = "integer", default = 0L, help = "only do one season?")
)
opt <- parse_args(OptionParser(option_list = option_list))

cat(paste0("---", tools::toTitleCase(opt$gender), " possession level mixed effects (lme4) modeling---\n"))

# Load in data
load(file = here::here(opt$gender, "/data/loaded.RData"))
rm(home_away_dict, game_stats, conferences, new_games, new_games_off_def, rest)
invisible(gc(verbose = FALSE))

# Source helpers
source(here::here("utils.R"))
source(here::here("config.R"))

possessions_clean <- dplyr::inner_join(possessions_clean, daynumbers, by = "game_date")
shots_clean <- dplyr::inner_join(shots_clean, daynumbers, by = "game_date")

if (opt$season == 0L) {
  sns <- unique(date_grid$season)
} else {
  sns <- opt$season
}

# Grid of date-metric
metric_grid <- date_grid %>%
  collapse::fsubset(game_date >= "2018-11-06" & season %in% sns) %>%
  tidyr::crossing(metric = c("ppp", "epps")) %>%
  collapse::roworder(game_date)

cat("LME4 possession level modeling...\n")

# Adjust efficiency metrics by opponent using lme4 model
predict_lme4 <- function(rerun = TRUE, metric, day = NULL) {

  if (!is.null(day) & (day %in% start_dates | day >= Sys.Date() - 2)) {
    cat("Date:", as.character(day), metric, "lme4 poss...\n")
  }

  # metric = "ppp"
  # day = "2020-01-29"
  # day = "2018-11-06"

  model_data <- switch (metric,
                        "ppp" = collapse::fsubset(possessions_clean, game_date < day & game_date > as.Date(day)-params[[opt$gender]]$lme4$days_back),
                        "epps" = collapse::fsubset(shots_clean, game_date < day & game_date > as.Date(day)-params[[opt$gender]]$lme4$days_back)
  )

  unique_seasons <- length(unique(model_data$season))

  if (unique_seasons == 1L) {
    eff_name <- ""
  } else {
    eff_name <- ":season"
  }

  target <- switch (metric,
                    "ppp" = "points",
                    "epps" = "epps"
  )

  form <- as.formula(paste0(target , " ~
                              off_location*off_b2b*def_b2b + off_location*off_distance_from_home + off_location*def_distance_from_home +
                              splines::ns(off_rest, df = 2) + splines::ns(def_rest, df = 2) + splines::ns(weight, df = 2) + ",
                            ifelse(unique_seasons == 1L,
                                   "(1 | off_id) + (1 | def_id)",
                                   "(splines::ns(daynum, df = 2) | season) + (1 | off_id:season) + (1 | def_id:season)")))

  # Count rows for offense/defense
  off_n <- dplyr::count(model_data, season, team_id = off_id)
  def_n <- dplyr::count(model_data, season, team_id = def_id)

  if (isTRUE(rerun) | day >= Sys.Date()) {
    # Fit model
    set.seed(123)
    fit <- suppressMessages(lme4::lmer(formula = form,
                                       data = model_data,
                                       # weights = model_data$weight,
                                       control = lme4::lmerControl(calc.derivs = FALSE)))

    effects <- lme4::ranef(fit)

    off_ratings <- effects[[paste0("off_id", eff_name)]] %>%
      dplyr::rename(rating = `(Intercept)`) %>%
      dplyr::mutate(id = rownames(.),
                    type = paste0("off_lme4_", metric)) %>%
      tidyr::separate(id, into = c("team_id", "season"), sep = ":", convert = TRUE) %>%
      dplyr::mutate(season = as.integer(season)) %>%
      dplyr::left_join(off_n, by = c("season", "team_id")) %>%
      collapse::fselect(rating, team_id, season_eff = season, type, n)

    def_ratings <- effects[[paste0("def_id", eff_name)]] %>%
      dplyr::rename(rating = `(Intercept)`) %>%
      dplyr::mutate(id = rownames(.),
                    type = paste0("def_lme4_", metric)) %>%
      tidyr::separate(id, into = c("team_id", "season"), convert = TRUE) %>%
      dplyr::mutate(season = as.integer(season)) %>%
      dplyr::left_join(def_n, by = c("season", "team_id")) %>%
      collapse::fselect(rating, team_id, season_eff = season, type, n)

    ratings <- dplyr::bind_rows(off_ratings, def_ratings)

    saveRDS(ratings, here::here(paste0(opt$gender, "/models/team_ratings/lme4/poss/", metric, day, ".rds")))
  } else {

    ratings <- suppressWarnings(try(readRDS(here::here(paste0(opt$gender, "/models/team_ratings/lme4/poss/", metric, day, ".rds"))), silent = TRUE))

    if ("try-error" %in% class(ratings)) {
      # Fit best model via CV parameters
      set.seed(123)
      fit <- suppressMessages(lme4::lmer(formula = form,
                                         data = model_data,
                                         # weights = model_data$weight,
                                         control = lme4::lmerControl(calc.derivs = FALSE)))

      effects <- lme4::ranef(fit)

      off_ratings <- effects[[paste0("off_id", eff_name)]] %>%
        dplyr::rename(rating = `(Intercept)`) %>%
        dplyr::mutate(id = rownames(.),
                      type = paste0("off_lme4_", metric)) %>%
        tidyr::separate(id, into = c("team_id", "season"), sep = ":", convert = TRUE) %>%
        dplyr::mutate(season = as.integer(season)) %>%
        dplyr::left_join(off_n, by = c("season", "team_id")) %>%
        collapse::fselect(rating, team_id, season_eff = season, type, n)

      def_ratings <- effects[[paste0("def_id", eff_name)]] %>%
        dplyr::rename(rating = `(Intercept)`) %>%
        dplyr::mutate(id = rownames(.),
                      type = paste0("def_lme4_", metric)) %>%
        tidyr::separate(id, into = c("team_id", "season"), convert = TRUE) %>%
        dplyr::mutate(season = as.integer(season)) %>%
        dplyr::left_join(def_n, by = c("season", "team_id")) %>%
        collapse::fselect(rating, team_id, season_eff = season, type, n)

      ratings <- dplyr::bind_rows(off_ratings, def_ratings)

      saveRDS(ratings, here::here(paste0(opt$gender, "/models/team_ratings/lme4/poss/", metric, day, ".rds")))
    }
  }

  return(ratings)
}

set.seed(234)
predictions <- metric_grid %>%
  dplyr::mutate(preds = purrr::map2(metric, game_date,
                                    ~ predict_lme4(rerun = opt$rerun,
                                                   metric = .x,
                                                   day = .y))) %>%
  dplyr::select(-c(metric)) %>%
  tidyr::unnest(preds)

# Save lme4 regression random effects
saveRDS(predictions, here::here(opt$gender, "/data/lme4_preds_poss_level.rds"))

cat("Done with lme4 possession level predictions.\n")

quit(status = 0)
