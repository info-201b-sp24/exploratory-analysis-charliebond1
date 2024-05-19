---
  title: "NBA & WNBA Summary Info"
author: "Charlie Bond"
date: "2024-05-17"
output: html_document
---


#Loading useful Packages 

library(tidyverse)
library(dplyr)
library(tinytex)
library(readr)
library(readxl)
library(ggplot2)



#Loading Datasets


nba <- read_csv("C:/Users/RCB3/Downloads/NBA Player Salaries (2022-23 Season)_exported.csv")
wnba <- read_xlsx("C:/Users/RCB3/Downloads/WNBA STATS (1).xlsx")

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
  select("Player Name", Salary, GP:BLK) %>% 
  mutate(league = "WNBA")
mod_simple_nba <- simple_nba %>% 
  select(Player.Name, Salary, GP:PTS, Total.Minutes) %>% 
  relocate(Total.Minutes, .after = GS) %>% 
  relocate(PTS, .after = Total.Minutes) %>% 
  rename('Total Minutes' = Total.Minutes, "Player Name" = Player.Name) %>% 
  mutate(league = "NBA")

combined_data <- rbind(mod_simple_nba, mod_simple_wnba)


view(mod_simple_nba)
view(mod_simple_wnba)



#Preparing Inputs

combined_data <- combined_data %>% 
  mutate(across(Salary:BLK, ~ as.numeric(replace_na(., 0))))
  
  
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
print(average_performance_stats)

fitted_inputs <- average_performance_stats %>% 
  pivot_longer(
    cols = starts_with("avg_salary_per"), 
    names_to = "PerformanceStats",
    values_to = "avg_salary"
  )
fitted_inputs$PerformanceStats <- gsub("avg_salary_per", "", fitted_inputs$PerformanceStats)



view(combined_data)


#The Bar Plot


performance_statistics_plot <- ggplot(fitted_inputs, aes(x = PerformanceStats, y = avg_salary, fill = league)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparing Average Salary by Performance Statistic between the NBA and the WNBA",
    x = "Performance Statistics",
    y = "Average Salary Per....",
    fill = "League"
  ) 

plot(performance_statistics_plot)



























