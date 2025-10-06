library("tidyverse")
library("dplyr")
library("stringr")
library("worldfootballR")
library("zoo")
library("poisbinom")
library("regista")
library("distributions3")

rm(list = ls())

wsl_shots <- read_rds(file = "./data/wsl_shots_test.rds") %>%
  # removing games with no shots recorded.
  filter(!is.na(shots))

wsl_results <- read_rds(file = "./data/wsl_results.rds")

simulated_games <- readRDS("./data/wsl_simulated_games.rds")

#loading in functions to simulate games
source("./functions/simulate_game.R")
# takes xg per shot data, and produces simulated games 
simulated_games <-
  wsl_shots %>%
  filter(!MatchID %in% unique(simulated_games)$MatchID) %>%
  mutate(simulated_probabilities = map(shots, simulate_game)) %>%
  select(Home, Away, Date, Season_End_Year, simulated_probabilities, MatchID) %>%
  unnest(cols = c(simulated_probabilities)) %>%
  filter(prob > 0.001) %>% # Keep the number of rows vaguely reasonable
  rbind(readRDS("./data/wsl_simulated_games.rds"))

saveRDS(simulated_games, "./data/wsl_simulated_games.rds")