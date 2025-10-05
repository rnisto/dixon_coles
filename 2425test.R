library("tidyverse")
library("dplyr")
library("stringr")
library("worldfootballR")
library("zoo")
library("poisbinom")
library("regista")
library("distributions3")

wsl_shots <- read_rds(file = "./data/wsl_shots_test.rds") %>%
  # removing games with no shots recorded.
  filter(!is.na(shots)) %>%
  filter(Season_End_Year < 2026)

wsl_results <- read_rds(file = "./data/wsl_results.rds") %>%
  filter(Season_End_Year < 2026)

# function to simulate no. of goals based on xg values for each shot
simulate_shots <- function(xgs) {
  tibble::tibble(goals = 0:length(xgs),
                 prob  = poisbinom::dpoisbinom(0:length(xgs), xgs))
}

# function to take simulated no. of goals and convert to score prob
simulate_game <- function(shot_xgs) {
  home_xgs <- shot_xgs %>% mutate(xG = as.numeric(xG), Home_Away = as.character(Home_Away)) %>% dplyr::filter(Home_Away == "Home") %>% pull(xG)
  away_xgs <- shot_xgs %>% mutate(xG = as.numeric(xG), Home_Away = as.character(Home_Away)) %>% dplyr::filter(Home_Away == "Away") %>% pull(xG)
  
  if(length(home_xgs) == 0){
    home_probs <- tibble(
      hgoals = 0,
      hprob = 1
    )
  } else{
    home_probs <- simulate_shots(home_xgs) %>% dplyr::rename_all(function(x) paste0("h", x))
  }
  
  if(length(away_xgs) == 0){
    away_probs <- tibble(
      agoals = 0,
      aprob = 1
    )
  } else{
    away_probs <- simulate_shots(away_xgs) %>% dplyr::rename_all(function(x) paste0("a", x))
  }
  
  tidyr::crossing(home_probs, away_probs) %>%
    dplyr::mutate(prob = .data$hprob * .data$aprob)
}

# takes score prob and merges with   
simulated_games <-
  wsl_shots_test %>%
  mutate(simulated_probabilities = map(shots, simulate_game)) %>%
  select(Home, Away, Date, simulated_probabilities) %>%
  unnest(cols = c(simulated_probabilities)) %>%
  filter(prob > 0.001)  # Keep the number of rows vaguely reasonable

xg_weight <- 0.7

wsl_xresults <- wsl_results %>%
  select(Home, Away, HomeGoals, AwayGoals, Date) %>%
  mutate(prob_ = 1 - xg_weight) %>%
  right_join(simulated_games, by = c("Home", "Away", "HomeGoals" = "hgoals", "AwayGoals" = "agoals", "Date")) %>%
  mutate(prob_ = replace(prob_, is.na(prob_), 0),
         prob = xg_weight*prob + prob_,
         time_diff = as.numeric(max(Date) - Date),
         time_weight = regista::discount_exponential(time_diff, 0.003),
         weight = time_weight * prob
  ) %>%
  select(Home, Away, hgoals = HomeGoals, agoals = AwayGoals, Date, weight) %>%
  arrange(Date,Home,Away,weight)

fit_simulated <- regista::dixoncoles(
  hgoal = hgoals,
  agoal = agoals,
  hteam = Home,
  ateam = Away,
  data = factor_teams(wsl_xresults, c("Home", "Away")),
  weights = weight
)

estimates <- regista::tidy.dixoncoles(fit_simulated) %>%
  filter(!is.na(team)) %>%
  mutate(value = exp(value)) %>%
  pivot_wider(values_from = value, names_from = "parameter")

hfa <- regista::tidy.dixoncoles(fit_simulated) %>%
  filter(parameter == "hfa") %>%
  pull(value) + 1

forecast <- tibble(
  home = estimates$team,
  away = estimates$team
) %>%
  expand(home,away) %>%
  filter(!home == away) %>%
  left_join(estimates, by = c("home" = "team")) %>%
  rename("off_h" = "off", "def_h" = "def") %>%
  left_join(estimates, by = c("away" = "team")) %>%
  rename("off_a" = "off", "def_a" = "def") %>%
  mutate(
    h_lambda = off_h * def_a * hfa,
    a_lambda = def_h * off_a
  )

forecast_goals <- function(lambda){
  dist <- Poisson(lambda)
  tibble(goals = 0:10,
         prob = pdf(dist,seq(0,10,1))
  )
}

forecast_scores <- forecast %>%
  mutate(h_pred = map(h_lambda, forecast_goals),
         a_pred = map(a_lambda, forecast_goals)
  ) %>%
  unnest(c(h_pred, a_pred), names_sep = ".") %>%
  select(home, away, h_pred.goals, h_pred.prob, a_pred.goals, a_pred.prob) %>%
  group_by(home,away) %>%
  complete(h_pred.goals, a_pred.goals) %>%
  group_by(home,away,h_pred.goals) %>%
  fill(h_pred.prob, .direction = "updown") %>%
  group_by(home,away,a_pred.goals) %>%
  fill(a_pred.prob, .direction = "updown") %>%
  mutate(prob = h_pred.prob * a_pred.prob) %>%
  filter(prob > 0.001) %>%
  select(home, away, hgoals = h_pred.goals, agoals = a_pred.goals, prob)

wsl_25_agg <- filter(wsl_results,Season_End_Year == 2025) %>% 
  group_by(HomeGoals, AwayGoals) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  mutate(prob_outturn = count / sum(count)) %>%
  rename("hgoals" = "HomeGoals", "agoals" = "AwayGoals") %>%
  select(!count)

comp <- forecast_scores %>%
  filter(home %in% filter(wsl_results, Season_End_Year == 2025)$Home &
           away %in% filter(wsl_results, Season_End_Year == 2025)$Away
  ) %>%
  group_by(hgoals, agoals) %>%
  summarise(
    prob = sum(prob)
  ) %>%
  ungroup() %>%
  mutate(prob_model = prob / sum(prob)) %>%
  full_join(wsl_25_agg) %>%
  mutate_at(c("prob_model", "prob_outturn"), ~replace(., is.na(.), 0)) %>%
  mutate(diff = prob_model - prob_outturn)

sum(comp$prob_model)

plot <- ggplot(data = filter(comp, hgoals <6 & agoals <6), aes(x = hgoals, y = agoals)) +
  geom_tile(aes(alpha = abs(diff), fill = diff > 0)) +
  geom_label(aes(label = format_percent(diff),
                 colour = diff < 0.01)) +
  scale_fill_manual(values = c("red", "green")) +
  scale_alpha_continuous(range = c(0, 1)) +
  scale_colour_manual(values = c("black", "gray50")) +
  scale_x_continuous(breaks = 0:10, minor_breaks = NULL) +
  scale_y_continuous(breaks = 0:10, minor_breaks = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_line(linetype = "dotted"),
        legend.position = "none") +
  theme(plot.caption = element_text(hjust = 0)) + # set the left align here
  labs(title = "Simulated scoreline probabilities",
       subtitle = "Expected vs Outturn for 24/25 WSL season",
       x = "Home goals",
       y = "Away goals",
       caption = "Predictions are based on aDixon-Coles model estimated on shot-level xG data from 2018/19 to the end of the 2024/25 season.\nGames are exponentially weighted by the number of days from the end of the 2024/25 season."
  )

ggsave(plot, filename = "./outputs/plots/2425_exp_v_outturn.jpeg")
