library("tidyverse")
library("dplyr")
library("stringr")
library("worldfootballR")
library("zoo")

rm(list = ls())

# function to extract match results data
eng_2025 <- fb_match_results(country = "ENG", gender = "M", season_end_year = seq(2019,2026), tier = c("1st"))
saveRDS(eng_2025, "./data/epl18-25.rds")
eng_2025 <- read_rds("./data/epl18-25.rds")

elo <- tibble(
  date = Date(),
  team = character()
)

xg_weight = 0.5

start_date <- min(eng_2025$Date)

# starting elo based on elo rating calculated without xg. 
base_elo <- readRDS("./outputs/epl_0025.rds")

for (team in unique(eng_2025$Home)){
  elo <- add_row(elo,
                 date = start_date,
                 team = `team`
  )
}

elo <- left_join(elo, base_elo, by = c("date", "team"))

k <- 0.03 # tested using k_test.R minimises MSE. 

dates <- min(eng_2025$Date):max(eng_2025$Date)

dates <- as.Date(dates)

for (i in 1:length(dates)){
  date_i <- dates[i]
  elo_today <- filter(elo, date == as.Date(`date_i`))
  
  if(nrow(filter(eng_2025, Date == `date_i`)) == 0){
    elo_tomorrow <- elo_today %>%
      mutate(date = date + days(1))
    
    elo <- rbind(elo, elo_tomorrow)
    next
  }
  
  x <- filter(eng_2025, Date == `date_i`) %>%
    left_join(elo_today, by= c('Home' = 'team', 'Date' = 'date')) %>%
    rename(home_xAt = xAt, home_xDf = xDf) %>%
    left_join(elo_today, by= c('Away' = 'team', 'Date' = 'date')) %>%
    rename(away_xAt = xAt, away_xDf = xDf)
  
  calcs <- x %>%
    mutate(
      home_pG = home_xAt * away_xDf,
      away_pG = away_xAt * home_xDf,
      home_chg = ((1 - xg_weight)*HomeGoals + xg_weight*Home_xG - home_pG) * k,
      away_chg = ((1 - xg_weight)*AwayGoals + xg_weight*Away_xG - away_pG) * k,
      home_xAt = home_xAt + home_chg,
      home_xDf = home_xDf + away_chg,
      away_xAt = away_xAt + away_chg,
      away_xDf = away_xDf + home_chg
    )
  
  #print(calcs)
  
  home_elo <- select(calcs,date = Date, team = Home, xAt = home_xAt, xDf = home_xDf) %>%
    mutate(date = date + days(1))
  
  away_elo <-  select(calcs, date = Date, team = Away, xAt = away_xAt, xDf = away_xDf) %>%
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

ggplot(data = filter(elo, team %in% c("Liverpool", "Manchester City", "Arsenal")), aes(x = date, y = xAt - xDf)) +
  geom_line(aes(colour = team))

ggplot(data = filter(elo, date == date_i), aes(x = team, y = xDf)) +
  geom_col() +
  coord_flip()

testing <- eng_2025 %>%
  left_join(elo, by = c("Date" = "date", "Home" = "team")) %>%
  rename("Home_xAt" = "xAt", "Home_xDf" = "xDf") %>%
  left_join(elo, by = c("Date" = "date", "Away" = "team")) %>%
  rename("Away_xAt" = "xAt", "Away_xDf" = "xDf") %>%
  mutate(Home_Pred = Home_xAt * Away_xDf,
         Away_Pred = Away_xAt * Home_xDf,
         Error = ((Home_Pred - HomeGoals) + (Away_Pred - AwayGoals))^2
         ) %>%
  mutate(gd = HomeGoals - AwayGoals,
         xgd = Home_Pred - Away_Pred
         )
summarise(testing, Error = mean(Error))^1/2  

ggplot(data = testing, aes(x = xgd, y = gd)) +
  geom_point() +
  geom_smooth()

model <- lm(data = testing, gd ~ xgd)
summary(model)

model_int <- model$coefficients[1]
model_slope <- model$coefficients[2]
model_sd <- summary(model)$sigma

fixtures_dl <- fb_match_fixtures(country = "ENG", gender = "M", season_end_year = 2026, tier = c("1st")) 

fixtures <- fixtures_dl %>%
  filter(is.na(HomeGoals)) %>%
  select(Competition_Name:Home, Away) %>%
  distinct(.keep_all = TRUE)

predictions <- fixtures[0,] %>%
  add_column(
    home_prob = numeric(),
    away_prob = numeric()
  )

set.seed(400)
for (n in 1:nrow(fixtures)){
  #simulate error term
  e <- rnorm(n = 100, mean = 0, sd = model_sd) 
  
  calcs <- left_join(fixtures[`n`, ], elo, by = c("Date" = "date", "Home" = "team")) %>%
    rename("home_xAt" = "xAt", "home_xDf" = "xDf") %>%
    left_join(elo, by = c("Date" = "date", "Away" = "team")) %>%
    rename("away_xAt" = "xAt", "away_xDf" = "xDf") %>%
    mutate(xgd = home_xAt * away_xDf - away_xAt * home_xDf)
  
  calcs <- rbind(calcs, calcs[rep(1,99), ])
  
  calcs$e <- e
  calcs <- mutate(calcs, pred_gd = round(model_int + xgd*model_slope + e,digits = 0),
                     home_xpts = case_when(
                       pred_gd == 0 ~ 1,
                       pred_gd > 0 ~ 3,
                       pred_gd < 0 ~ 0
                     ),
                     away_xpts = case_when(
                       pred_gd == 0 ~ 1,
                       pred_gd > 0 ~ 0,
                       pred_gd < 0 ~ 3
                     )
  )
  
  predictions <- add_row(predictions,
    fixtures[`n`,],
    home_prob = sum(calcs$home_xpts == 3) / 100,
    away_prob = sum(calcs$away_xpts == 3) / 100
  )
}

summarise(filter(predictions, Home == "Manchester City"), home_prob = sum(home_prob)*3) +
  summarise(filter(predictions, Away == "Manchester City"), away_prob = sum(away_prob)*3) +
  summarise(filter(predictions, Home == "Manchester City"), draw_prob = sum(1-away_prob) - sum(home_prob)) +
  summarise(filter(predictions, Away == "Manchester City"), draw_prob = sum(1-away_prob) - sum(home_prob)) +
  7
          