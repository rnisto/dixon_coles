library("tidyverse")
library("dplyr")
library("stringr")
library("worldfootballR")
library("zoo")

rm(list = ls())

# to get the EPL results:
epl_results <- understat_league_match_results(league = "EPL", season_start_year = c(2014))
for (i in 2015:2025){
  epl_results <- rbind(epl_results, understat_league_match_results(league = "EPL", season_start_year = c(`i`)))
}

epl_results <- mutate(epl_results, datetime = date(datetime))
saveRDS(epl_results, file = "./data/epl_results.rds")

elo <- tibble(
  date = Date(),
  team = character(),
  xAt = numeric(),
  xDf = numeric()
)

starting_values <- filter(epl_results, season == "2014/2015") %>%
  select(home_abbr, away_abbr, home_goals, away_goals, home_xG, away_xG)

xg_weight = 0.3

start_date <- min(filter(epl_results, season == "2014/2015")$datetime)

# for (team in unique(starting_values$home_abbr)){
#   home <- filter(starting_values, home_abbr == `team`) %>%
#     summarise(xAt = (1 - xg_weight) * mean(home_goals) + xg_weight * mean(home_xG),
#               xDf = (1 - xg_weight) * mean(away_goals) + xg_weight * mean(away_xG)
#     )
#   
#   away <- filter(starting_values, away_abbr == `team`) %>%
#     summarise(xAt = (1 - xg_weight) * mean(away_goals) + xg_weight * mean(away_xG),
#               xDf = (1 - xg_weight) * mean(home_goals) + xg_weight * mean(home_xG)
#     )
#   
#   elo <- add_row(elo,
#                  date = start_date,
#                  team = `team`,
#                  xAt = (home$xAt + away$xAt) / 2,
#                  xDf = (home$xDf + away$xDf) / 2,
#   )
# }

for (team in unique(epl_results$home_abbr)){
    elo <- add_row(elo,
                   date = start_date,
                   team = `team`,
                   xAt = 1,
                   xDf = 1
    )
}

# elo <- add_row(elo,
#   date = start_date,
#   team = unique(filter(epl_results, season == "2015/2016", !home_abbr %in% elo$team)$home_abbr),
#   xAt = min(elo$xAt),
#   xDf = max(elo$xDf)
# )

k <- 0.1

dates <- min(epl_results$datetime):max(epl_results$datetime)

dates <- as.Date(dates)

for (i in 1:length(dates)){
  date_i <- dates[i]
  elo_today <- filter(elo, date == as.Date(`date_i`))
  
  x <- filter(epl_results, datetime == `date_i`) %>%
    left_join(elo_today, by= c('home_abbr' = 'team', 'datetime' = 'date')) %>%
    rename(home_xAt = xAt, home_xDf = xDf) %>%
    left_join(elo_today, by= c('away_abbr' = 'team', 'datetime' = 'date')) %>%
    rename(away_xAt = xAt, away_xDf = xDf)
  
  calcs <- x %>%
    mutate(
      home_pG = home_xAt * away_xDf,
      away_pG = away_xAt * home_xDf,
      home_chg = ((1 - xg_weight)*home_goals + xg_weight*home_xG - home_pG) * k,
      away_chg = ((1 - xg_weight)*away_goals + xg_weight*away_xG - away_pG) * k,
      home_xAt = home_xAt + home_chg,
      home_xDf = home_xDf + away_chg,
      away_xAt = away_xAt + away_chg,
      away_xDf = away_xDf + home_chg
    )
  
  #print(calcs)
  
  home_elo <- select(calcs,date = datetime, team = home_abbr, xAt = home_xAt, xDf = home_xDf) %>%
                 mutate(date = date + days(1))
  
  away_elo <-  select(calcs, date = datetime, team = away_abbr, xAt = away_xAt, xDf = away_xDf) %>%
                 mutate(date = date + days(1))

  elo_tomorrow <- elo_today %>%
    mutate(date = date + days(1)) %>%
    left_join(home_elo, by = c("date", "team")) %>%
    transmute(date, team, xAt = coalesce(xAt.y, xAt.x), xDf = coalesce(xDf.y, xDf.x)) %>%
    select(!ends_with(".x") & !ends_with(".y")) %>%
    left_join(away_elo, by = c("date", "team")) %>%
    transmute(date, team, xAt = coalesce(xAt.y, xAt.x), xDf = coalesce(xDf.y, xDf.x)) %>%
    select(!ends_with(".x") & !ends_with(".y"))
    
  elo <- rbind(elo, elo_tomorrow)
  
  print(`date_i`)
}

ggplot(data = filter(elo, team == "LIV"), aes(x = date, y = xDf)) +
  geom_line(aes(colour = team))

test <- filter(elo, team == "MUN")

