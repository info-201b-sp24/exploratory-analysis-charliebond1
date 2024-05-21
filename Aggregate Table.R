library(tidyverse)
library(dplyr)
library(tinytex)
library(ggplot2)

simple_nba <- read_csv("CSVs/simple_nba.csv")
simple_wnba <- read_csv("CSVs/simple_wnba.csv")



#Combining Datasets

mod_simple_wnba <- simple_wnba %>% 
  select('PLAYER', X2024.SALARY, G:BLK) %>% 
  mutate(league = "WNBA") %>%
  rename('Salary' = X2024.SALARY, 'GP' = G, 'Total Minutes' = MIN, 'Player Name' = PLAYER)
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
data.frame(Pay_gap_summary_table)


  