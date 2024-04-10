# Load libraries
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option("--gender", type = "character", default = "mens", help = "mens or womens, default mens")
)
opt <- parse_args(OptionParser(option_list = option_list))

cat("---Calculating rolling lagged averages of team metrics---\n")

# Load in data
load(file = here::here(opt$gender, "/data/loaded.RData"))

# Source helpers
source(here::here("utils.R"))
source(here::here("config.R"))

roll_metrics <- function(game_stats, pad = 65, mov_type = "s", w_length = 10) {

  cat("Window:", w_length, "\nMoving Avg. Type:", mov_type, "\nLeague Avg. Padding:", pad, "...\n")

  # Offensive metrics
  off_metrics <- game_stats %>%
    collapse::fsubset(completed == 1L & train == 1L) %>%
    collapse::roworder(game_date) %>%
    dplyr::group_by(season, team_id = off_id) %>%
    dplyr::mutate(game_number = dplyr::row_number(),
                  cum_off_poss = cumsum(poss)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(team_id) %>%
    dplyr::mutate(window = pmax(w_length, game_number),
                  off_ppp_cur = jacklich::wt_mov_avg(var = ppp,
                                                     weight = poss,
                                                     window = window,
                                                     type = mov_type,
                                                     moving = TRUE),
                  off_pps_cur = jacklich::wt_mov_avg(var = pps,
                                                     weight = fga,
                                                     window = window,
                                                     type = mov_type,
                                                     moving = TRUE),
                  off_epps_cur = jacklich::wt_mov_avg(var = epps,
                                                      weight = fga,
                                                      window = window,
                                                      type = mov_type,
                                                      moving = TRUE),
                  off_poss_min_cur = jacklich::wt_mov_avg(var = poss_min,
                                                          weight = 1,
                                                          window = window,
                                                          type = mov_type,
                                                          moving = TRUE),
                  off_poss_length_cur = jacklich::wt_mov_avg(var = poss_length,
                                                             weight = 1,
                                                             window = window,
                                                             type = mov_type,
                                                             moving = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(season, game_date, game_id, team_id, game_number, cum_off_poss,
                  off_pps = pps, off_epps = epps, off_ppp = ppp, off_poss = poss,
                  off_poss_min = poss_min, off_poss_length = poss_length,
                  off_ppp_cur:off_poss_length_cur)

  # Defensive metrics
  def_metrics <- game_stats %>%
    collapse::fsubset(completed == 1L & train == 1L) %>%
    collapse::roworder(game_date) %>%
    dplyr::group_by(season, team_id = def_id) %>%
    dplyr::mutate(game_number = dplyr::row_number(),
                  cum_def_poss = cumsum(poss)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(team_id) %>%
    dplyr::mutate(window = pmax(w_length, game_number),
                  def_ppp_cur = jacklich::wt_mov_avg(var = ppp,
                                                     weight = poss,
                                                     window = window,
                                                     type = mov_type,
                                                     moving = TRUE),
                  def_pps_cur = jacklich::wt_mov_avg(var = pps,
                                                     weight = fga,
                                                     window = window,
                                                     type = mov_type,
                                                     moving = TRUE),
                  def_epps_cur = jacklich::wt_mov_avg(var = epps,
                                                      weight = fga,
                                                      window = window,
                                                      type = mov_type,
                                                      moving = TRUE),
                  def_poss_min_cur = jacklich::wt_mov_avg(var = poss_min,
                                                          weight = 1,
                                                          window = window,
                                                          type = mov_type,
                                                          moving = TRUE),
                  def_poss_length_cur = jacklich::wt_mov_avg(var = poss_length,
                                                             weight = 1,
                                                             window = window,
                                                             type = mov_type,
                                                             moving = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(season, game_date, game_id, team_id, game_number, cum_def_poss,
                  def_pps = pps, def_epps = epps, def_ppp = ppp, def_poss = poss,
                  def_poss_min = poss_min, def_poss_length = poss_length,
                  def_ppp_cur:def_poss_length_cur)

  # All metrics
  metrics <- dplyr::inner_join(off_metrics, def_metrics, by = c("season", "game_date", "game_id", "team_id", "game_number"))

  # Game result team metrics from each game (this is what happened, not predictive)
  game_results <- collapse::fselect(metrics, season, game_date, game_id, team_id, game_number,
                                    off_poss, off_poss_min, off_ppp, off_pps, off_epps,
                                    def_poss, def_poss_min, def_ppp, def_pps, def_epps) %>%
    dplyr::left_join(daynumbers, by = "game_date")

  # Lagged predictors
  lag_metrics <- metrics %>%
    dplyr::select(season, game_date, team_id,
                  cum_off_poss, cum_def_poss, dplyr::ends_with("_cur")) %>%
    dplyr::group_by(season, team_id) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("cum_"), dplyr::lag)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(team_id) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_cur"), dplyr::lag),
                  lag_game_date = dplyr::lag(game_date) + 1L) %>%
    dplyr::ungroup() %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_cur"), ~ stringr::str_remove(., "_cur"))

  # Most recent predictors for each team
  cur_metrics <- metrics %>%
    dplyr::group_by(team_id) %>%
    dplyr::slice_max(game_date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(season, game_date, team_id,
                  cum_off_poss, cum_def_poss,
                  dplyr::ends_with("_cur")) %>%
    collapse::fmutate(lag_game_date = game_date + 1L) %>%
    dplyr::rename_with(.cols = dplyr::ends_with("_cur"), ~ stringr::str_remove(., "_cur"))

  # Combine lagged and current metrics for each team
  all_lag_metrics <- collapse::fselect(dplyr::bind_rows(lag_metrics, cur_metrics), -season, -game_date) %>%
    tidyr::replace_na(list(cum_off_poss = 0L, cum_def_poss = 0L))

  pred_metrics <- data.frame(game_date = (start_dates[1]+1L):(Sys.Date()+5L)) %>%
    collapse::fmutate(game_date = as.Date(game_date, origin = "1970-01-01"),
                      month = as.integer(substr(game_date, start = 6, stop = 7))) %>%
    collapse::fsubset(!month %in% c(5L, 6L, 7L, 8L, 9L, 10L), -month) %>%
    tidyr::crossing(team_id = unique(game_stats$off_id)) %>%
    dplyr::left_join(all_lag_metrics, by = c("game_date" = "lag_game_date", "team_id")) %>%
    dplyr::group_by(team_id) %>%
    tidyr::fill(dplyr::everything(), .direction = "down") %>%
    dplyr::ungroup() %>%
    # Pad metrics
    dplyr::left_join(lg_avgs %>%
                       dplyr::select(-c(season, games, dplyr::starts_with("cur_"))) %>%
                       dplyr::rename_with(.cols = dplyr::starts_with("lag_"), ~ stringr::str_remove(., "lag_")),
                     by = "game_date") %>%
    collapse::roworder(game_date, team_id) %>%
    tidyr::fill(dplyr::starts_with("lg_mean_"), .direction = "down") %>%
    pad_metrics(pad = pad)

  return(list(metrics = pred_metrics, game_results = game_results))
}

rolled_metrics <- roll_metrics(game_stats = game_stats,
                               pad = params[[opt$gender]]$roll$pad,
                               mov_type = params[[opt$gender]]$roll$type,
                               w_length = params[[opt$gender]]$roll$len)

# Save data
saveRDS(rolled_metrics, here::here(opt$gender, "/data/roll_metrics.rds"))

cat("Completed lagged data manipulation.\n")

quit(status = 0)
