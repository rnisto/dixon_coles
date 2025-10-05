forecast_goals <- function(lambda){
  dist <- Poisson(lambda)
  tibble(goals = 0:10,
         prob = pdf(dist,seq(0,10,1))
  )
}
