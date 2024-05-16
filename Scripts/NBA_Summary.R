# Aleah Rosner
# Summary Information

# Load Packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)

# Load Data
nba_data <- read_csv("C:/Users/aever/Documents/INFO201/exploratory-analysis-charliebond1/CSVs/nba-stats_out.csv")
wnba_data <- read_csv("C:/Users/aever/Documents/INFO201/exploratory-analysis-charliebond1/CSVs/wnba-stats_out.csv")
View(nba_data)
View(wnba_data)

# Combine datasets into one
nba_data <- nba_data  %>%
  mutate(`slry/blks` = as.character(`slry/blks`))
wnba_data <- wnba_data %>%
  mutate(`slry/blks` = as.character(`slry/blks`))
combined_basketball_data <- bind_rows(nba_data, wnba_data)
View(combined_basketball_data)

# Summary Values

# Maximum NBA salary
max_salary_NBA <- max(nba_data$salary)
print(max_salary_NBA)
# $34,682,550

# Maximum WNBA salary
max_salary_WNBA <- max(wnba_data$salary)
print(max_salary_WNBA)
# $105,000

# Minimum NBA salary
min_salary_NBA <- min(nba_data$salary)
print(min_salary_NBA)
# $1,249,920

# Minimum WNBA salary
min_salary_WNBA <- min(wnba_data$salary)
print(min_salary_WNBA)
# $105,000

# Average Salary of NBA players
average_salary_NBA <- mean(nba_data$salary)
print(average_salary_NBA)
# 8939685 dollars

# Average Salary of WNBA players
average_salary_WNBA <- mean(wnba_data$salary)
print(average_salary_WNBA)
# 105000 dollars

# Average dollars earned per minute of play (NBA)
nba_data_no_zero_minutes <- nba_data[nba_data$minutes != 0, ]
View(nba_data_no_zero_minutes)
avg_salary_per_min_NBA <- mean((nba_data_no_zero_minutes$salary)/(nba_data_no_zero_minutes$minutes))
print(avg_salary_per_min_NBA)
# $243,955.60

# Average dollars earned per minute of play (WNBA)
wnba_data_no_zero_minutes <- wnba_data[wnba_data$minutes != 0, ]
View(wnba_data_no_zero_minutes)
avg_salary_per_min_WNBA <- mean((wnba_data_no_zero_minutes$salary)/(wnba_data_no_zero_minutes$minutes))
print(avg_salary_per_min_WNBA)
# $2077.02

# Average dollars earned per point (NBA)
nba_data_no_zero_points <- nba_data[nba_data$points != 0, ]
View(nba_data_no_zero_points)
avg_salary_per_point_NBA <- mean((nba_data_no_zero_points$salary)/(nba_data_no_zero_points$points))
print(avg_salary_per_point_NBA)
# $394,636.30

# Average dollars earned per point (WNBA)
wnba_data_no_zero_points <- wnba_data[wnba_data$points != 0, ]
View(wnba_data_no_zero_points)
avg_salary_per_point_WNBA <- mean((wnba_data_no_zero_points$salary)/(wnba_data_no_zero_points$points))
print(avg_salary_per_point_WNBA)
# $2,865.88

