

#Loading useful Packages 

library(tidyverse)
library(dplyr)
library(tinytex)
library(readr)
library(readxl)
library(ggplot2)



#Loading Datasets


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



#Preparing Inputs

combined_data <- combined_data %>% 
  mutate(across(Salary:BLK, ~ as.numeric(replace_na(., 0))))
  
combined_data <- data.frame(combined_data)

average_performance_stats <- combined_data %>%
  group_by(league) %>% 
  filter(
    PTS > 0,
    AST > 0,
    STL > 0,
    BLK > 0
  ) %>% 
  summarise(
    avg_salary_per_point = mean(as.numeric(Salary))/mean(PTS, na.rm = T),
    avg_salary_per_assist = mean(as.numeric(Salary))/mean(AST, na.rm = T),
    avg_salary_per_steal = mean(as.numeric(Salary))/mean(STL, na.rm = T),
    avg_salary_per_block = mean(as.numeric(Salary))/mean(BLK, na.rm = T)
  )


fitted_inputs <- average_performance_stats %>% 
  pivot_longer(
    cols = starts_with("avg_salary_per"), 
    names_to = "PerformanceStats",
    values_to = "avg_salary"
  )
fitted_inputs$PerformanceStats <- gsub("avg_salary_per", "", fitted_inputs$PerformanceStats)




#The Bar Plot


performance_statistics_plot <- ggplot(fitted_inputs, aes(x = PerformanceStats, y = avg_salary, fill = league)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparing Average Salary by Performance Statistic between the NBA and the WNBA",
    x = "Performance Statistics",
    y = "Average Salary Per....",
    fill = "League"
  ) 




























