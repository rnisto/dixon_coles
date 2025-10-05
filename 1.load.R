library("tidyverse")
library("dplyr")
library("stringr")
library("worldfootballR")
library("zoo")
library("poisbinom")
library("regista")
library("distributions3")

# import amended worldfootballR functions
source("./functions/wsl_fix.R")

#replacing bugged urls
read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv") %>%
  mutate(
    fixtures_url = replace(fixtures_url, season_end_year == 2017 & competition_name == "FA Women's Super League", 
                           "https://fbref.com/en/comps/189/2017/schedule/2017-Womens-Super-League-Scores-and-Fixtures"
    ),
    fixtures_url = replace(fixtures_url, season_end_year == 2025 & competition_name == "FA Women's Super League", 
                           "https://fbref.com/en/comps/189/2024-2025/schedule/2024-2025-Womens-Super-League-Scores-and-Fixtures"
    )
  ) %>%
  write.csv(file = "./data/comps_urls.csv")

# starting in 2019 when we have xg data
wsl_results <- fb_match_results(country = "ENG", gender = "F", season_end_year = seq(2019,2026), tier = c("1st")) %>%
  filter(Date <= today(), !is.na(HomeGoals))

saveRDS(wsl_results, file = "./data/wsl_results.rds")

# function to take the url from match results, and gets the detailed shot / xg data
get_shots <- function(url){
  x <- fb_match_shooting(url)
  
  if(length(x) > 0){
    x <- dplyr::select(x, Home_Away, xG)
  } else {
    x <- NA_real_
  }
  return(x)
}

# pulls new data for wsl
#don't run unless you want to refresh all the data - will take ages!
# wsl_shots <- wsl_results %>%
#   # nesting shots data inside fixtures data
#   mutate(shots = map(MatchURL, ~ {Sys.sleep(10)
#     message(paste0("Accessing:",.x))
#     get_shots(.x)
#   } #multiline function to incorporate gap between requests.
#   ))
# 
# saveRDS(wsl_shots, file = "./data/wsl_shots.rds")

# returns shots data for fixtures I don't already have data for. 
wsl_shots_new <- read_rds(file = "./data/wsl_shots_test.rds") %>%
  select(MatchURL, shots) %>%
  right_join(wsl_results, by = c("MatchURL")) %>%
  rowwise() %>%
  filter(is.null(shots)) %>%
  # nesting shots data inside fixtures data
  mutate(shots = map(MatchURL, ~ {Sys.sleep(10)
    message(paste0("Accessing:",.x))
    get_shots(.x)
  } #multiline function to incorporate gap between requests. 
  )) 

read_rds(file = "./data/wsl_shots_test.rds") %>%
  rbind(wsl_shots_new) %>%
  #mutate(shots = map(shots, ~ bind_rows(.x))) %>%
  saveRDS(file = "./data/wsl_shots_test.rds")