# Simulate a portfolio of brackets

# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(collapse)))
suppressMessages(suppressWarnings(library(foreach)))
suppressMessages(suppressWarnings(library(optparse)))

option_list <- list(
  make_option("--sims", type = "integer", default = 20000, help = "simulations to run, default 10000"),
  make_option("--chalk", type = "logical", default = FALSE, help = "chalk bracket, default FALSE"),
  make_option("--override", type = "logical", default = FALSE, help = "override bracket, default FALSE"),
  make_option("--gender", type = "character", default = "mens", help = "mens or womens, default mens")
)
opt <- parse_args(OptionParser(option_list = option_list))

cat(paste0("---", tools::toTitleCase(opt$gender), " bracket simulations---\n"))

# Source helpers
source(here::here("utils.R"))

gen <- switch (opt$gender, "mens" = "M", "womens" = "W")

# Team maps
team_map <- readr::read_csv(paste0("~/Downloads/march-machine-learning-mania-2024/", gen, "Teams.csv"), col_types = readr::cols()) %>%
  collapse::fselect(TeamID, TeamName) %>%
  clean_kaggle_names(col = "TeamName")

# Slots for each game
round_slots <- readr::read_csv(paste0("~/Downloads/march-machine-learning-mania-2024/", gen, "NCAATourneySlots.csv"), col_types = readr::cols()) %>%
  # Filter out First Four
  collapse::fsubset(Season == 2024 & stringr::str_detect(Slot, "R"))

# Teams in tournament
seeds <- readr::read_csv("~/Downloads/march-machine-learning-mania-2024/2024_tourney_seeds.csv", col_types = readr::cols())

# Predictions
preds <- readr::read_csv(paste0("~/Desktop/", tolower(gen), "_kaggle_preds.csv"), col_types = readr::cols())

# Overrides
if (isTRUE(opt$override) & opt$gender == "mens") {
  preds <- preds %>%
    mutate(pred_wp = ifelse(team_name == "Connecticut", 1, ifelse(opp_team_name == "Connecticut", 0, pred_wp)))
}

preds <- dplyr::transmute(preds, ID = game_id, Pred = pred_wp)

# Function preparing the data for the simulation
prepare_data <- function(seeds, preds) {

  seed_dict <- setNames(seeds$TeamID, seeds$Seed)
  inverted_seed_dict <- setNames(names(seed_dict), seed_dict)
  probas_dict <- list()

  for (i in seq_along(preds$ID)) {
    teams <- stringr::str_split(preds$ID[i], "_")[[1]]
    proba <- preds$Pred[i]
    team1 <- teams[2]
    team2 <- teams[3]

    probas_dict[[team1]][[team2]] <- proba
    probas_dict[[team2]][[team1]] <- 1L - proba
  }

  return(list(seed_dict = seed_dict, inverted_seed_dict = inverted_seed_dict, probas_dict = probas_dict))
}

# Simulates each round of the tournament
simulate <- function(round_slots, seeds, inverted_seeds, probas, sim = TRUE) {

  num_slots <- nrow(round_slots)
  winners <- vector("list", length = num_slots)
  losers <- vector("list", length = num_slots)
  slots <- character(num_slots)

  for (i in seq_len(num_slots)) {
    slot <- round_slots$Slot[i]
    strong <- round_slots$StrongSeed[i]
    weak <- round_slots$WeakSeed[i]

    team_1 <- unname(seeds[strong])
    team_2 <- unname(seeds[weak])

    # Get the probability of team_1 winning
    proba <- probas[[as.character(team_1)]][[as.character(team_2)]]

    if (sim) {
      # Randomly determine the winner based on the probability
      winner <- sample(c(team_1, team_2), size = 1, prob = c(proba, 1 - proba))
    } else {
      # Determine the winner based on the higher probability
      winner <- ifelse(proba > (1 - proba), team_1, team_2)
    }

    if (winner == team_1) {
      loser <- team_2
    } else {
      loser <- team_1
    }

    # Store the winner and corresponding slot
    winners[[i]] <- winner
    losers[[i]] <- loser
    slots[i] <- slot

    # Update the seeds directly
    seeds[[as.character(slot)]] <- winner
  }

  # Convert winners to original seeds using the inverted_seeds dictionary
  orig_seeds <- inverted_seeds[as.character(unlist(winners))]
  loser_seeds <- inverted_seeds[as.character(unlist(losers))]

  return(list(orig_seeds, loser_seeds, slots))
}

# Runs a simulation of bracket tournaments
run_simulation <- function(brackets = 1, seeds = NULL, preds = NULL, round_slots = NULL, sim = TRUE) {

  # Get relevant data for the simulation
  data <- prepare_data(seeds = seeds, preds = preds)
  seed_dict <- data$seed_dict
  inverted_seed_dict <- data$inverted_seed_dict
  probas_dict <- data$probas_dict

  # Create a parallel backend using doParallel
  cl <- parallel::makeCluster(4L)
  doParallel::registerDoParallel(cl)

  # Use foreach for parallel execution
  result_df <- foreach::foreach(b = 1:brackets, .inorder = FALSE, .verbose = FALSE, .combine = "rbind", .export = c("simulate", "prepare_data")) %dopar% {
    if (b %% (brackets / 10L) == 0L) {
      print(paste("Sim", b, "..."))
      Sys.sleep(0.1)
    }

    # Run single simulation
    res <- simulate(round_slots = round_slots,
                    seeds = seed_dict,
                    inverted_seeds = inverted_seed_dict,
                    probas = probas_dict,
                    sim = sim)

    # Create a data frame for the results
    data.frame(Bracket = rep(b, length(res[[1]])),
               Slot = res[[3]],
               Team = res[[1]],
               TeamId = as.integer(names(res[[1]])),
               LTeam = res[[2]],
               LTeamId = as.integer(names(res[[2]])))
  }

  # Stop the parallel backend
  parallel::stopCluster(cl)
  closeAllConnections()

  return(result_df)
}

set.seed(234)
result <- run_simulation(brackets = opt$sims,
                         seeds = seeds,
                         preds = preds,
                         round_slots = round_slots,
                         sim = !opt$chalk)
result$Tournament <- gen

# Write output to desktop
readr::write_csv(result, paste0("~/Desktop/", tolower(gen), "_kaggle_sims", ifelse(isTRUE(opt$override), "_override"), ".csv"))

quit(status = 0)

# Final submission
submission <- collapse::rowbind(
  readr::read_csv(paste0("~/Desktop/m_kaggle_sims_override.csv"), col_types = readr::cols()),
  readr::read_csv(paste0("~/Desktop/w_kaggle_sims.csv"), col_types = readr::cols())
) %>%
  collapse::fsubset(stringr::str_detect(Slot, "R"))
submission$RowId <- 1:nrow(submission)
readr::write_csv(submission, paste0("~/Desktop/kaggle_submission_uconn.csv"))
