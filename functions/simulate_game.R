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