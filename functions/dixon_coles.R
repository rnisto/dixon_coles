library("tidyverse")
library("dplyr")
# a function to create and store dixon-coles model runs

# model_runs <- tibble(
#   date_run = character(),
#   model_start_date = character(),
#   model_end_date = character(),
#   n = numeric(),
#   shot_level_xG = logical(),
#   parameters = list(),
#   model = list(),
#   estimates = list(),
#   notes = character()
# )
# 
# saveRDS(model_runs, "./data/dixon_coles_runs.rds")

dixon_coles_run <- function(
  .data,
  parameters_table = parameters,
  notes = "",
  shot_level_xG = TRUE,
  force_rerun = FALSE
){
  existing_runs <- readRDS("./data/dixon_coles_runs.rds")
  input_data <- .data
  params <- parameters_table
  xg <- shot_level_xG
  nts <- notes
  
  match <- filter(existing_runs,
                  model_start_date == min(input_data$Date),
                  model_start_date == min(input_data$Date),
                  n == nrow(input_data),
                  shot_level_xG == shot_level_xG,
                  parameters == parameters
  )
  
  if(nrow(match) == 1){
    output <- match
  } else if(nrow(match) == 0){
    fit_simulated <- regista::dixoncoles(
      hgoal = hgoals,
      agoal = agoals,
      hteam = Home,
      ateam = Away,
      data = factor_teams(input_data, c("Home", "Away")),
      weights = weight
    )
    
    strengths <- regista::tidy.dixoncoles(fit_simulated) %>%
      filter(!is.na(team)) %>%
      mutate(value = exp(value)) %>%
      pivot_wider(values_from = value, names_from = "parameter")
    
    existing_runs <- existing_runs %>% add_row(
      date_run = today(),
      model_start_date = min(input_data$Date),
      model_end_date = max(input_data$Date),
      n = nrow(input_data),
      shot_level_xG = shot_level_xG,
      parameters = params,
      model = fit_simulated,
      estimates = strengths,
      notes = nts
    )
    
    saverds(existing_runs, "./data/dixon_coles_runs.rds")
    output <- tail(existing_runs,1)
  } else{
    stop("Specificaiton returned multiple model matches")
  }
  output
}

x <- dixon_coles_run(
  .data = filter(wsl_xresults, year(Date) > 2023)
)
