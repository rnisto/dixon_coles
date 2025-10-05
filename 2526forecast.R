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
  filter(!is.na(shots))

wsl_results <- read_rds(file = "./data/wsl_results.rds")

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

# produces a table of score probabilities based on shot-level xG data  
simulated_games <-
  wsl_shots %>%
  mutate(simulated_probabilities = map(shots, simulate_game)) %>%
  select(Home, Away, Date, simulated_probabilities) %>%
  unnest(cols = c(simulated_probabilities)) %>%
  filter(prob > 0.001)  # Keep the number of rows vaguely reasonable

# the model uses both xG and goals. This is the typical ratio used for mens foot
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
  arrange(Date,Home,Away,weight) %>%
  filter(Date < as_date("2025/10/27"))

# helper function to format percentages for the plot
format_percent <- function(p) {
  rounded <- round(100 * p)
  ifelse(abs(p) < 0.005, "0%", paste0(rounded, "%")) 
}

# example plot of probabilities
ggplot(data = slice(simulated_games,1:28), aes(x = hgoals, y = agoals)) +
  geom_tile(aes(alpha = prob), fill = "#08519c") +
  geom_label(aes(label = format_percent(prob),
                 colour = prob < 0.01)) +
  scale_alpha_continuous(range = c(0, 1)) +
  scale_colour_manual(values = c("black", "gray50")) +
  scale_x_continuous(breaks = 0:10, minor_breaks = NULL) +
  scale_y_continuous(breaks = 0:10, minor_breaks = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_line(linetype = "dotted"),
        legend.position = "none") +
  labs(title = "Simulated scoreline probabilities",
       subtitle = "Chelsea vs Manchester City",
       x = "Home goals",
       y = "Away goals")

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

# fit_simulated <- regista::dixoncoles(
#   hgoal = HomeGoals,
#   agoal = AwayGoals,
#   hteam = Home,
#   ateam = Away,
#   data = factor_teams(wsl_2025_results, c("Home", "Away"))
# )


forecast <- tibble(
  home = estimates$team, 
  away = estimates$team
) %>%
  filter(home %in% filter(wsl_results,Season_End_Year == 2026)$Home,
         away %in% filter(wsl_results,Season_End_Year == 2026)$Away
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
  ) %>%
  mutate(homeaway = paste0(home,away)) %>%
  filter(
    !homeaway %in% filter(mutate(wsl_results, homeaway = paste0(Home,Away)), Season_End_Year == 2026)$homeaway
  ) %>%
  select(!homeaway)


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


result_to_points <- function(result, homeaway){
  as.numeric(
    if_else(homeaway == "home",
            case_when(
              result == "home_win" ~ 3,
              result == "draw" ~ 1,
              result == "away_win" ~ 0,
              TRUE ~ NA_real_
            ), 
            if_else(homeaway == "away",
                    case_when(
                      result == "away_win" ~ 3,
                      result == "draw" ~ 1,
                      result == "home_win" ~ 0,
                      TRUE ~ NA_real_
                    ),
                    NA_real_        
            )))
}

forecast_results <- forecast_scores %>%
  group_by(home,away) %>%
  group_modify(~ regista::scorelines_to_outcomes(
    .x,
    hgoal = hgoals,
    agoal = agoals,
    prob = prob
  )) %>%
  rowwise() %>%
  mutate(hpoints = as.numeric(case_when(
    outcome == "home_win" ~ 3,
    outcome == "away_win" ~ 0,
    outcome == "draw" ~ 1,
    TRUE ~ NA_real_
  )),
  apoints = as.numeric(case_when(
    outcome == "away_win" ~ 3,
    outcome == "home_win" ~ 0,
    outcome == "draw" ~ 1,
    TRUE ~ NA_real_
  )),
  hxpts = hpoints * prob,
  axpts = apoints * prob
  )

forecast_table_h <- forecast_results %>%
  group_by(home) %>%
  summarise(hxpts = sum(hxpts))

forecast_table_a <- forecast_results %>%
  group_by(away) %>%
  summarise(axpts = sum(axpts))

forecast_table <- cbind(forecast_table_h, forecast_table_a$axpts) %>%
  mutate(xpts = hxpts + forecast_table_a$axpts) %>%
  select("team" = "home", xpts) %>%
  arrange(-xpts)

get_table <- function(.data = wsl_results, season, date){
  scores <- .data  
  
  results <- scores %>%
    filter(Season_End_Year == season & Date <= date) %>%
    rowwise() %>%
    mutate(hpoints = as.numeric(case_when(
      HomeGoals > AwayGoals ~ 3,
      HomeGoals < AwayGoals ~ 0,
      HomeGoals == AwayGoals ~ 1,
      TRUE ~ NA_real_
    )),
    apoints = as.numeric(case_when(
      HomeGoals > AwayGoals ~ 0,
      HomeGoals < AwayGoals ~ 3,
      HomeGoals == AwayGoals ~ 1,
      TRUE ~ NA_real_
    ))
    )
  
  table_h <- results %>%
    group_by(Home) %>%
    summarise(hpoints = sum(hpoints))
  
  table_a <- results %>%
    group_by(Away) %>%
    summarise(apoints = sum(apoints))
  
  table <- cbind(table_h, table_a$apoints) %>%
    rename("apoints" = "table_a$apoints") %>%
    mutate(pts = hpoints + apoints) %>%
    select("team" = "Home", pts) %>%
    arrange(-pts)
  
  table
}

get_table(season = 2026, date = today())

table <- forecast_table %>%
  left_join(get_table(season = 2026, date = today()), by = "team") %>%
  mutate(points = pts + xpts) %>%
  select(team, points) %>%
  mutate(pos = row_number())

montecarlo <- function(data, homeaway, runs = 20000){
  points <- data %>% pull(hpoints)
  prob <- pull(data,prob)
  hpoints <- sample(points, size = runs, replace = TRUE, prob = prob)
  hpoints <- tibble(
    hpoints = hpoints
  ) 
  hpoints %>%
    mutate(apoints = case_when(
      hpoints == 3 ~ 0,
      hpoints == 0 ~ 3,
      TRUE ~ hpoints
    )) %>%
    mutate(run = row_number())
}

# example of how mc can be used. needs updating for away fixtures and to
# include already played results
x <- forecast_results %>%
  select(home, away, prob, hpoints, apoints) %>%
  group_by(home,away) %>%
  nest() %>%
  rename("probs" = "data")

set.seed(100)
results <- x %>%
  mutate(
    points = map(probs,~montecarlo(.x))
  ) %>%
  select(!probs) %>%
  unnest(cols = c(points))

home <- results %>%
  group_by(home,run) %>%
  summarise(
    hpoints = sum(hpoints),
    run = first(run)
  ) 

away <- results %>%
  group_by(away,run) %>%
  summarise(
    apoints = sum(apoints),
    run = first(run)
  )
  
table <- full_join(home,away, by = c("home" = "away", "run")) %>%
  rename("team" = "home") %>%
  left_join(get_table(season = 2026, date = as_date("2024/10/26")), by = "team") %>%
  mutate(points = pts + hpoints + apoints) %>%
  arrange(run,-hpoints) %>%
  group_by(run) %>%
  mutate(pos = row_number()) %>%
  group_by(team, pos) %>%
  summarise(prop = n() / 20000) %>%
  left_join(estimates, by = "team") %>%
  mutate(strength = off - def) %>%
  filter(prop > 0.001)

format_percent <- function(p) {
  rounded <- round(100 * p)
  ifelse(abs(p) < 0.005, "", paste0(rounded, "%")) 
}

ggplot(data = table, aes(x = reorder(team,strength), y = pos)) +
  geom_tile(fill = "#08519c", aes(alpha = prop)) +
  geom_text(aes(label = format_percent(prop))) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(breaks = 1:12, minor_breaks = NULL, position = "right") +
  theme(panel.grid.major = element_line(linetype = "dotted"),
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
        ) 
   