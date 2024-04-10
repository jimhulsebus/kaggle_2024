# Utility functions

get_current_season <- function() {
  season <- ifelse(as.integer(substr(Sys.Date(), 6, 7)) >= 11,
                   as.integer(substr(Sys.Date(), 1, 4)) + 1,
                   as.integer(substr(Sys.Date(), 1, 4)))
  return(season)
}

prepare_for_shot_prob <- function(shots, randomize = FALSE, gamezone = FALSE) {

  if (isTRUE(randomize)) {
    set.seed(123)
    shots <- shots %>%
      # randomize shot location at the rim for GameZone shot location data
      dplyr::mutate(random = as.integer(.data$loc_x == 25 & .data$loc_y == 5.25 |
                                          .data$loc_x == 26 & .data$loc_y == 6.25),
                    loc_x = ifelse(.data$random == 1L,
                                   rnorm(.data$loc_x, mean = .data$loc_x, sd = 1.25),
                                   .data$loc_x),
                    loc_y = ifelse(.data$random == 1L,
                                   .data$loc_y + rgamma(.data$loc_y, shape = 0.75, rate = 2),
                                   .data$loc_y)) %>%
      collapse::fselect(-random)
  }

  if ("prev_play" %in% colnames(shots)) {
    shots <- shots %>%
      dplyr::mutate(
        putback = dplyr::case_when(
          stringr::str_detect(.data$prev_play, "(O|o)ffensive (R|r)ebound") ~ 1L,
          TRUE ~ 0L)
      )
  }

  shots <- shots %>%
    dplyr::mutate(shot_distance = ifelse(!is.na(.data$loc_x),
                                         distance_to_hoop(.data$loc_x, .data$loc_y),
                                         NA_real_),
                  shot_angle = ifelse(!is.na(loc_x),
                                      angle_to_hoop(.data$loc_x, .data$loc_y),
                                      NA_real_),
                  shot_angle = ifelse(.data$shot_distance == 0 & is.na(.data$shot_angle),
                                      0, .data$shot_angle),
                  shot_made_numeric = dplyr::case_when(
                    is.na(.data$shot_outcome) ~ NA_real_,
                    .data$shot_outcome == "made" ~ 1,
                    .data$shot_outcome == "missed" ~ 0),
                  shot_value = dplyr::case_when(
                    is.na(.data$shot_outcome) ~ NA_real_,
                    .data$free_throw == 1 ~ 1,
                    .data$three_pt == 1 ~ 3,
                    TRUE ~ 2),
                  points = dplyr::case_when(
                    .data$shot_made_numeric == 0 ~ 0,
                    .data$shot_made_numeric == 1 & .data$free_throw == 1 ~ 1,
                    .data$shot_made_numeric == 1 & .data$three_pt == 1 ~ 3,
                    .data$shot_made_numeric == 1 & .data$three_pt == 0 & .data$free_throw == 0 ~ 2),
                  play_length = pmin(40L, .data$play_length),
                  shot_angle = abs(.data$shot_angle),
                  shot_desc = stringr::str_remove_all(.data$shot_desc, " "),
                  shot_desc = dplyr::case_when(
                    .data$shot_desc %in% c("SlamDunkShot") ~ "DunkShot",
                    is.na(.data$shot_desc) ~ "JumpShot",
                    TRUE ~ .data$shot_desc
                  ))

  if (isTRUE(gamezone)) {
    shots$home_shot <- as.integer(!is.na(shots$shot_outcome) & shots$event_team == shots$home)
    shots$season_num <- as.integer(paste0("20", substr(shots$season, start = 6, stop = 7)))
  } else {
    shots$home_shot <- as.integer(!is.na(shots$shot_outcome) & shots$team_id == shots$home_id)
  }

  return(shots)
}

# Function to calculate distance to hoop
distance_to_hoop <- function(x, y) {
  x <- (x - 25)^2
  y <- (y - 5.25)^2
  sum <- x + y
  return(sqrt(sum))
}

# Function to calculate angle to hoop
angle_to_hoop <- function(x, y) {
  return(atan(abs(x - 25)/(y - 5.25)) * (180/pi))
}

clean_travel_distance <- function(data, home_away = FALSE) {

  if (isTRUE(home_away)) {

    # Create interactions
    data$away_distance_away <- data$away_distance_from_home * as.integer(data$neutral == 0L)
    data$home_distance_neutral <- data$home_distance_from_home * as.integer(data$neutral == 1L)
    data$away_distance_neutral <- data$away_distance_from_home * as.integer(data$neutral == 1L)

    data$home_no_travel <- as.integer(data$home_distance_from_home == 0L)
    data$away_no_travel <- as.integer(data$away_distance_from_home == 0L)
  } else {

    # Create interactions
    data$off_distance_away <- data$off_distance_from_home * as.integer(data$off_location == "away")
    data$off_distance_neutral <- data$off_distance_from_home * as.integer(data$off_location == "neutral")

    data$def_distance_away <- data$def_distance_from_home * as.integer(data$off_location == "home")
    data$def_distance_neutral <- data$def_distance_from_home * as.integer(data$off_location == "neutral")

    data$off_no_travel <- as.integer(data$off_distance_from_home == 0L)
    data$def_no_travel <- as.integer(data$def_distance_from_home == 0L)
  }

  return(data)
}

make_team_conference_dicts <- function(schedule, conference_dictionary) {
  # Dictionary of team-season-conferences
  conferences <- dplyr::bind_rows(
    collapse::fselect(schedule, season, team_id = home_id, conference_id = home_conference_id),
    collapse::fselect(schedule, season, team_id = away_id, conference_id = away_conference_id)
  ) %>%
    collapse::fsubset(team_id > 0L) %>%
    dplyr::distinct() %>%
    dplyr::left_join(conference_dictionary, by = "conference_id")

  # Dictionary of teams
  teams <- dplyr::bind_rows(
    collapse::fselect(schedule, team_id = home_id, team_name = home),
    collapse::fselect(schedule, team_id = away_id, team_name = away)
  ) %>%
    collapse::fsubset(team_id > 0L) %>%
    dplyr::group_by(team_id) %>%
    dplyr::summarise(team_name = collapse::fmode(team_name),
                     .groups = "drop")

  return(list(conferences = conferences, teams = teams))
}

# Make folds with strata
stratified_game_folds <- function(data, folds, strata, seed = 123) {

  set.seed(seed)
  f <- rsample::vfold_cv(data = dplyr::distinct(data, game_id, season), v = folds, strata = !!strata)

  # Find players within each fold
  g_folds <- purrr::map_df(seq_len(length(f$splits)), function(spl) {
    gs <- dplyr::distinct(rsample::assessment(f$splits[[spl]]), game_id)
    gs$fold <- spl
    return(gs)
  })

  # Join fold Ids
  data <- dplyr::inner_join(data, g_folds, by = "game_id")

  return(data)
}

# Takes a list of sparse matrixes with different columns and adds them row wise
merge.sparse <- function(listMatrixes) {
  allColnames <- sort(unique(unlist(lapply(listMatrixes,colnames))))
  for (currentMatrix in listMatrixes) {
    newColLocations <- match(colnames(currentMatrix),allColnames)
    indexes <- which(currentMatrix!=0, arr.ind = TRUE)
    newColumns <- newColLocations[indexes[,2]]
    rows <- indexes[,1]
    newMatrix <- Matrix::sparseMatrix(i=rows,j=newColumns, x=currentMatrix@x,
                                      dims=c(max(rows),length(allColnames)))
    if (!exists("matrixToReturn")) {
      matrixToReturn <- newMatrix
    }
    else {
      matrixToReturn <- rbind2(matrixToReturn,newMatrix)
    }
  }
  colnames(matrixToReturn) <- allColnames
  matrixToReturn
}

# Extract team Id ridge regression coefficients
get_ridge_ceofs <- function(coefs, side = c("off", "def"), metric = c("points", "ppp", "pps", "epps")) {

  if (metric == "points") {
    coefs <- collapse::fsubset(coefs, stringr::str_detect(term, "team_id")) %>%
      collapse::fmutate(team_id = as.integer(stringr::str_remove(term, "team_id"))) %>%
      dplyr::transmute(team_id,
                       pred = estimate,
                       metric = paste0("ridge_", metric))

  } else {
    coefs <- coefs %>%
      collapse::fsubset(stringr::str_detect(term, paste0(side, "_id"))) %>%
      collapse::fmutate(team_id = as.integer(stringr::str_remove(term, paste0(side, "_id")))) %>%
      dplyr::transmute(team_id,
                       pred = estimate,
                       metric = paste0("ridge_", side, "_", metric))
  }

  return(coefs)
}

get_recency_weights <- function(df, day, day_decay = 0.0025, sn_decay = 0.5) {

  df <- collapse::fsubset(df, game_date < day)

  df <- df %>%
    dplyr::distinct(season, game_date) %>%
    collapse::roworder(season, game_date) %>%
    dplyr::mutate(row = dplyr::row_number(),
                  days_ago = max(row) - row,
                  seasons_ago = max(season) - season,
                  weight = exp(-day_decay*days_ago) * exp(-sn_decay*seasons_ago)) %>%
    collapse::fselect(season, game_date, weight)

  return(df)
}

pad_metrics <- function(df, pad = 65) {
  df %>%
    tidyr::replace_na(list(cum_off_poss = 0L, cum_def_poss = 0L)) %>%
    dplyr::mutate(off_ppp = (off_ppp*pmax(25L, cum_off_poss) + pad*lg_mean_ppp)/(pmax(25L, cum_off_poss) + pad),
                  off_pps = (off_pps*pmax(25L, cum_off_poss) + pad*lg_mean_pps)/(pmax(25L, cum_off_poss) + pad),
                  off_epps = (off_epps*pmax(25L, cum_off_poss) + pad*lg_mean_epps)/(pmax(25L, cum_off_poss) + pad),
                  off_poss_min = (off_poss_min*pmax(25L, cum_off_poss) + pad*lg_mean_poss)/(pmax(25L, cum_off_poss) + pad),
                  off_poss_length = (off_poss_length*pmax(25L, cum_off_poss) + pad*lg_mean_poss_length)/(pmax(25L, cum_off_poss) + pad),

                  def_ppp = (def_ppp*pmax(25L, cum_def_poss) + pad*lg_mean_ppp)/(pmax(25L, cum_def_poss) + pad),
                  def_pps = (def_pps*pmax(25L, cum_def_poss) + pad*lg_mean_pps)/(pmax(25L, cum_def_poss) + pad),
                  def_epps = (def_epps*pmax(25L, cum_def_poss) + pad*lg_mean_epps)/(pmax(25L, cum_def_poss) + pad),
                  def_poss_min = (def_poss_min*pmax(25L, cum_def_poss) + pad*lg_mean_poss)/(pmax(25L, cum_def_poss) + pad),
                  def_poss_length = (def_poss_length*pmax(25L, cum_def_poss) + pad*lg_mean_poss_length)/(pmax(25L, cum_def_poss) + pad),

                  # If no rating, make league average
                  dplyr::across(.cols = dplyr::ends_with("_ppp"), ~ dplyr::coalesce(., lg_mean_ppp)),
                  dplyr::across(.cols = dplyr::ends_with("_pps"), ~ dplyr::coalesce(., lg_mean_pps)),
                  dplyr::across(.cols = dplyr::ends_with("_epps"), ~ dplyr::coalesce(., lg_mean_epps)),
                  dplyr::across(.cols = dplyr::ends_with("_poss_min"), ~ dplyr::coalesce(., lg_mean_poss)),
                  dplyr::across(.cols = dplyr::ends_with("_poss_length"), ~ dplyr::coalesce(., lg_mean_poss_length))) %>%
    dplyr::select(-dplyr::starts_with("lg_mean"))
}

process_rest_travel <- function(schedule, home_venues) {
  dplyr::bind_rows(
    schedule %>%
      collapse::fsubset(!(is.na(score_diff) & game_date <= Sys.Date() - 5)) %>%
      dplyr::distinct(season, game_date, hour, game_id, team_id = home_id, neutral, postseason, long, lat) %>%
      dplyr::mutate(location = dplyr::if_else(neutral == 1L, "neutral", "home", missing = "home")),
    schedule %>%
      collapse::fsubset(!(is.na(score_diff) & game_date <= Sys.Date() - 5)) %>%
      dplyr::distinct(season, game_date, hour, game_id, team_id = away_id, neutral, postseason, long, lat) %>%
      dplyr::mutate(location = dplyr::if_else(neutral == 1L, "neutral", "away", missing = "away"))
  ) %>%
    collapse::roworder(game_date) %>%
    dplyr::group_by(team_id) %>%
    dplyr::mutate(rest = as.numeric(game_date) - dplyr::lag(as.numeric(game_date))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(home_venues, by = "team_id") %>%
    dplyr::group_by(season, team_id) %>%
    dplyr::mutate(prev_long = dplyr::lag(long),
                  prev_lat = dplyr::lag(lat),
                  streak_num = 1L + cumsum(location != dplyr::lag(location, default = dplyr::first(location))),
                  b2b_road = as.integer(dplyr::lag(location == "away", default = 0) & location == "away"),
                  b2b_home = as.integer(dplyr::lag(location == "home", default = 0) & location == "home"),
                  triple_road = as.integer(dplyr::lag(b2b_road == 1, default = 0) & location == "away"),
                  triple_home = as.integer(dplyr::lag(b2b_home == 1, default = 0) & location == "home")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(season, team_id, streak_num) %>%
    dplyr::mutate(streak = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rest = pmin(10L, rest),
                  prev_long = dplyr::coalesce(prev_long, long),
                  prev_lat = dplyr::coalesce(prev_lat, lat)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(distance = as.numeric(geosphere::distm(c(long, lat), c(prev_long, prev_lat), fun = geosphere::distHaversine)),
                  distance_from_home = as.numeric(geosphere::distm(c(long, lat), c(home_long, home_lat), fun = geosphere::distHaversine))) %>%
    dplyr::ungroup() %>%
    collapse::fmutate(distance_from_home = log(dplyr::coalesce(distance_from_home, distance) + 1),
                      distance_from_home = ifelse(location == "home", 0, distance_from_home)) %>%
    dplyr::left_join(collapse::fselect(conferences, season, team_id, conf), by = c("season", "team_id")) %>%
    tidyr::replace_na(list(rest = 10L, conf = "UNK")) %>%
    dplyr::select(-c(neutral, season, streak_num, long, lat, prev_long, prev_lat, home_long, home_lat, distance))
}

group_conferences <- function(df, home_away = FALSE) {

  if (isTRUE(home_away)) {
    df$home_conf <- ifelse(df$home_conf %in% main_confs, df$home_conf, "Non Power")
    df$away_conf <- ifelse(df$away_conf %in% main_confs, df$away_conf, "Non Power")
  } else {
    df$off_conf <- ifelse(df$off_conf %in% main_confs, df$off_conf, "Non Power")
    df$def_conf <- ifelse(df$def_conf %in% main_confs, df$def_conf, "Non Power")
  }

  return(df)
}

# Make XGB monotone constraints
make_xgb_monotone <- function(feats) {

  lower <- unname(low_limits[feats]) %>% replace(. == 0, 1) %>% replace(is.na(.), 0)
  upper <- unname(up_limits[feats]) %>% replace(. == 0, -1) %>% replace(is.na(.), 0)

  # Monotone constraints
  monotone <- rbind(lower, upper) %>%
    as.data.frame() %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = sum)) %>%
    as.numeric()

  return(monotone)
}

make_conf_dummies <- function(dat) {
  conferences <- c("Non Power") # c("ACC", "Big 12", "Big East", "BigTen", "Pac-12", "SEC")
  for (conf in conferences) {
    dat[[paste0("off_conf_", stringr::str_remove_all(conf, " "))]] <- as.integer(dat$off_conf == conf)
    dat[[paste0("def_conf_", stringr::str_remove_all(conf, " "))]] <- as.integer(dat$def_conf == conf)
  }
  return(dat)
}

make_off_location_dummies <- function(dat) {
  # dat$off_location_home <- as.integer(dat$off_location == "home")
  # dat$off_location_neutral <- as.integer(dat$off_location == "neutral")
  #
  dat$off_home <- ifelse(dat$off_location == "neutral", 0L, ifelse(dat$off_location == "home", 1L, -1L))

  return(dat)
}

make_team_dummies <- function(dat) {
  dat %>%
    tidyr::pivot_longer(cols = c(h_id, a_id), names_to = "type", values_to = "team_id") %>%
    collapse::fmutate(value = ifelse(type == "h_id", 1L, -1L)) %>%
    collapse::fselect(-type) %>%
    tidyr::pivot_wider(names_from = team_id, values_from = value, values_fill = 0L, names_prefix = "team_id")
}

second_round_matches <- function(data) {
  data$second_round <- as.integer((data$team1_seed == 1 & data$team2_seed == 8) |
                                    (data$team1_seed == 1 & data$team2_seed == 9) |
                                    (data$team1_seed == 2 & data$team2_seed == 7) |
                                    (data$team1_seed == 2 & data$team2_seed == 10) |
                                    (data$team1_seed == 3 & data$team2_seed == 6) |
                                    (data$team1_seed == 3 & data$team2_seed == 11) |
                                    (data$team1_seed == 4 & data$team2_seed == 5) |
                                    (data$team1_seed == 4 & data$team2_seed == 12) |
                                    (data$team1_seed == 5 & data$team2_seed == 4) |
                                    (data$team1_seed == 5 & data$team2_seed == 13) |
                                    (data$team1_seed == 6 & data$team2_seed == 3) |
                                    (data$team1_seed == 6 & data$team2_seed == 14) |
                                    (data$team1_seed == 7 & data$team2_seed == 2) |
                                    (data$team1_seed == 7 & data$team2_seed == 15) |
                                    (data$team1_seed == 8 & data$team2_seed == 1) |
                                    (data$team1_seed == 8 & data$team2_seed == 16) |
                                    (data$team1_seed == 9 & data$team2_seed == 1) |
                                    (data$team1_seed == 9 & data$team2_seed == 16) |
                                    (data$team1_seed == 10 & data$team2_seed == 2) |
                                    (data$team1_seed == 10 & data$team2_seed == 15) |
                                    (data$team1_seed == 11 & data$team2_seed == 3) |
                                    (data$team1_seed == 11 & data$team2_seed == 14) |
                                    (data$team1_seed == 12 & data$team2_seed == 4) |
                                    (data$team1_seed == 12 & data$team2_seed == 13) |
                                    (data$team1_seed == 13 & data$team2_seed == 5) |
                                    (data$team1_seed == 13 & data$team2_seed == 12) |
                                    (data$team1_seed == 14 & data$team2_seed == 6) |
                                    (data$team1_seed == 14 & data$team2_seed == 11) |
                                    (data$team1_seed == 15 & data$team2_seed == 7) |
                                    (data$team1_seed == 15 & data$team2_seed == 10) |
                                    (data$team1_seed == 16 & data$team2_seed == 8) |
                                    (data$team1_seed == 16 & data$team2_seed == 9))

  data$second_round <- ifelse(data$team1_region == data$team2_region, data$second_round, 0L)
  return(data)
}

clean_kaggle_names <- function(dat, col) {

  dat[[col]] <- stringr::str_replace(dat[[col]], " St$", " State")
  dat[[col]] <- stringr::str_replace(dat[[col]], " So$", " Southern")
  dat[[col]] <- stringr::str_replace(dat[[col]], "^St ", "Saint ")
  dat[[col]] <- stringr::str_replace(dat[[col]], "^TX ", "Texas ")
  dat[[col]] <- stringr::str_replace(dat[[col]], "^FL ", "Florida ")
  dat[[col]] <- stringr::str_replace(dat[[col]], "^E ", "Eastern ")
  dat[[col]] <- stringr::str_replace(dat[[col]], "^N ", "North ")
  dat[[col]] <- stringr::str_replace(dat[[col]], "^S ", "Southern ")
  dat[[col]] <- stringr::str_replace(dat[[col]], "^C ", "Central ")
  dat[[col]] <- stringr::str_replace(dat[[col]], "^W ", "West ")
  dat[[col]] <- dplyr::case_when(
    dat[[col]] == "Ark Little Rock" ~ "Little Rock",
    dat[[col]] == "Cal Baptist" ~ "California Baptist",
    dat[[col]] == "Col Charleston" ~ "Charleston",
    dat[[col]] == "Connecticut" ~ "UConn",
    dat[[col]] == "CS Sacramento" ~ "Sacramento State",
    dat[[col]] == "Gardner Webb" ~ "Gardner-Webb",
    dat[[col]] == "Grambling State" ~ "Grambling",
    dat[[col]] == "Hawaii" ~ "Hawai'i",
    dat[[col]] == "Kent" ~ "Kent State",
    dat[[col]] == "McNeese State" ~ "McNeese",
    dat[[col]] == "Miami FL" ~ "Miami",
    dat[[col]] == "Mississippi" ~ "Ole Miss",
    dat[[col]] == "Monmouth NJ" ~ "Monmouth",
    dat[[col]] == "MTSU" ~ "Middle Tennessee",
    dat[[col]] == "Saint John's" ~ "St. John's",
    dat[[col]] == "Saint Mary's CA" ~ "Saint Mary's",
    dat[[col]] == "Sam Houston State" ~ "Sam Houston",
    dat[[col]] == "Southern Dakota State" ~ "South Dakota State",
    dat[[col]] == "TAM C. Christi" ~ "Texas A&M-Corpus Christi",
    dat[[col]] == "TN Martin" ~ "UT Martin",
    dat[[col]] == "WI Green Bay" ~ "Green Bay",
    dat[[col]] == "WKU" ~ "Western Kentucky",
    TRUE ~ dat[[col]]
  )

  return(dat)
}
