# Load libraries
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(optparse)))
suppressMessages(suppressWarnings(library(purrr)))
suppressMessages(suppressWarnings(library(readr)))
suppressMessages(suppressWarnings(library(rvest)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(wehoop)))

option_list <- list(
  make_option("--min_season", type = "integer", default = 2015L, help = "minumum season to scrape, default 2015"),
  make_option("--current", type = "logical", default = FALSE, help = "only scrape current season?, default FALSE")
)
opt <- parse_args(OptionParser(option_list=option_list))

# Current season
cur_sn <- wehoop::most_recent_wbb_season()

if (isTRUE(opt$current)) {
  sns <- cur_sn
} else {
  sns <- opt$min_season:cur_sn
}

espn_preseason_ap <- function(season, womens = FALSE) {

  cat("Season:", season, "...\n")

  # Construct URL
  url <- paste0("https://www.espn.com/", ifelse(isTRUE(womens), "womens", "mens"), "-college-basketball/rankings/_/week/1/year/", season, "/seasontype/2")
  # Read HTML
  html <- rvest::read_html(url)

  team_ids <- html %>%
    rvest::html_elements(".InnerLayout__child--dividers:nth-child(1) .InnerLayout__child--dividers .underline-hover .AnchorLink") %>%
    rvest::html_attr("href") %>%
    unique() %>%
    stringr::str_extract("\\d{1,5}") %>%
    as.integer()

  html %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    dplyr::transmute(season = season,
                     type = "preseason",
                     ap_rank = dplyr::row_number(),
                     team_name = stringr::str_remove(Team, "\\(.*"),
                     team_id = team_ids) %>%
    collapse::fsubset(ap_rank <= 25L)
}

# Men's
mens_preseason_ap <- purrr::map_df(sns, espn_preseason_ap)

# Women's
womens_preseason_ap <- purrr::map_df(sns, espn_preseason_ap, womens = TRUE)

# Save preseason AP poll
dplyr::group_split(mens_preseason_ap, season) %>%
  purrr::walk(function(x) {
    readr::write_csv(x, here::here(paste0("mens/data/ap/preseason_", unique(x$season),".csv")))
  })

dplyr::group_split(womens_preseason_ap, season) %>%
  purrr::walk(function(x) {
    readr::write_csv(x, here::here(paste0("womens/data/ap/preseason_", unique(x$season),".csv")))
  })

quit(status = 0)
