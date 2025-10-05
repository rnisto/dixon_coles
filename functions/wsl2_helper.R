x <- read_rds(file = "./data/wsl2.rds")

x <- x %>%
  add_row(
    Home = readline("Home team"),
    Away = readline("Away team"),
    HomeGoals = readline("Home Goals"),
    AwayGoals = readline("Away Goals"),
    HomexG = readline("Home xG"),
    AwayxG = readline("Away xG"),
    HomeShots = readline("Home Shots"),
    AwayShots = readline("Away Shots"),
    Date = readline("Date"),
    Competition_Name = "FA Women's Super League 2"
  ) 

saveRDS(x, file = "./data/wsl2.rds")
