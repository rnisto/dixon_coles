# a function to create and store dixon-coles model runs

model_runs <- tibble(
  date_run = character(),
  model_end_date = character(),
  n = numeric(),
  parameters = list(),
  estimates = list(),
  notes = character()
)
