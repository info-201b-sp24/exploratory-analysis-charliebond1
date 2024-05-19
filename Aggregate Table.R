library(tidyverse)
library(dplyr)
library(tinytex)
library(ggplot2)


load(combined_data)
View(combined_data)


nba <- read_csv("C:/Users/RCB3/Downloads/NBA Player Salaries (2022-23 Season)_exported.csv")
wnba <- read_excel("C:/Users/RCB3/Downloads/WNBA STATS (1).xlsx")

#Trimming Datasets



simple_nba <- nba %>% 
  select(`Player Name`:GS, AST, STL, BLK, PTS, FG, FGA, `Total Minutes`)
simple_wnba <- wnba %>% 
  select(PLAYER:PTS, AST, STL, BLK, FGM, FGA)
simple_nba <- data.frame(simple_nba)
write.csv(simple_nba, "C:/Users/RCB3/Downloads/simple_nba.csv", row.names = FALSE)
simple_wnba <- data.frame(simple_wnba)
write.csv(simple_wnba, "C:/Users/RCB3/Downloads/simple_wnba.csv", row.names = FALSE)



#Combining Datasets

mod_simple_wnba <- simple_wnba %>% 
  select('Player Name', Salary, GP:BLK) %>% 
  mutate(league = "WNBA")
mod_simple_nba <- simple_nba %>% 
  select(Player.Name, Salary, GP:PTS, Total.Minutes) %>% 
  relocate(Total.Minutes, .after = GS) %>% 
  relocate(PTS, .after = Total.Minutes) %>% 
  rename('Total Minutes' = Total.Minutes, 'Player Name' = Player.Name) %>% 
  mutate(league = "NBA")

combined_data <- rbind(mod_simple_nba, mod_simple_wnba)

combined_data <- combined_data %>% 
  mutate(across(Salary:BLK, ~ as.numeric(replace_na(., 0))))


Pay_gap_summary_table <- combined_data %>% 
  group_by(league) %>% 
  summarise(
    'Average Salary' = mean(Salary, na.rm = T),
    'Average Playtime per game' = mean(as.numeric(`Total Minutes`)/mean(as.numeric(GP)), na.rm = T), 
    'Average Salary per minute' =  mean(Salary, na.rm = T)/mean(as.numeric(`Total Minutes`), na.rm = T), 
    'Proportion of Games Played out of Games Started' = mean(GP, na.rm = T)/mean(GS, na.rm = T), 
    
    
  )
View(Pay_gap_summary_table)


  