# Aleah Rosner
# Summary Information

# Load Packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)

# Load Data
nba_data <- read_csv("C:/Users/aever/Documents/INFO201/exploratory-analysis-charliebond1/CSVs/simple_nba.csv")
wnba_data <- read_csv("C:/Users/aever/Documents/INFO201/exploratory-analysis-charliebond1/CSVs/simple_wnba.csv")
View(nba_data)
View(wnba_data)


# Summary Values

# Average Salary of NBA players
average_salary_NBA <- mean(nba_data$Salary)
print(average_salary_NBA)
# 8,416,599 dollars

# Average Salary of WNBA players
average_salary_WNBA <- mean(wnba_data$X2024.SALARY, na.rm = TRUE)
print(average_salary_WNBA)
# 130,795.70 dollars

# Difference in Average Salary
diff_salary_avg <- abs(average_salary_NBA - average_salary_WNBA)
print(diff_salary_avg)
# $8,285,803

# Maximum NBA salary
max_salary_NBA <- max(nba_data$Salary)
print(max_salary_NBA)
# $48,070,014

# Maximum WNBA salary
max_salary_WNBA <- max(wnba_data$X2024.SALARY, na.rm = TRUE)
print(max_salary_WNBA)
# $241,984

# Difference in maximum salary
diff_salary_max <- abs(max_salary_NBA - max_salary_WNBA)
print(diff_salary_max)
# $47,878,030

# Minimum NBA salary
min_salary_NBA <- min(nba_data$Salary, na.rm = TRUE)
print(min_salary_NBA)
# $5,849

# Minimum WNBA salary
min_salary_WNBA <- min(wnba_data$X2024.SALARY, na.rm = TRUE)
print(min_salary_WNBA)
# $64,154

# Difference in Minimum Salary
diff_salary_min <- abs(min_salary_NBA - min_salary_WNBA)
print(diff_salary_min)
# 58,305

# Average dollars earned per minute of play (NBA)
nba_data_no_zero_minutes <- nba_data[nba_data$Total.Minutes > 0, ]
View(nba_data_no_zero_minutes)
avg_salary_per_min_NBA <- mean((nba_data$Salary)/(nba_data$Total.Minutes))
print(avg_salary_per_min_NBA)
# $11,637.74

# Average dollars earned per minute of play (WNBA)
wnba_data$MIN <- as.numeric(gsub("--", NA, wnba_data$MIN))
wnba_data_no_dashed_min <- na.omit(wnba_data)
avg_salary_per_min_WNBA <- mean((wnba_data_no_dashed_min$X2024.SALARY)/(wnba_data_no_dashed_min$MIN))
print(avg_salary_per_min_WNBA)
# $6,447.84

# Difference in dollars/minute
diff_dolpermin <- abs(avg_salary_per_min_NBA - avg_salary_per_min_WNBA)
print(diff_dolpermin)
# 5,189.90

# Average dollars earned per point (NBA)
nba_data_no_zero_points <- nba_data[nba_data$PTS != 0, ]
View(nba_data_no_zero_points)
avg_salary_per_point_NBA <- mean((nba_data_no_zero_points$Salary)/(nba_data_no_zero_points$PTS))
print(avg_salary_per_point_NBA)
# $850,078.4

# Average dollars earned per point (WNBA)
wnba_data$PTS <- as.numeric(gsub("--", NA, wnba_data$PTS))
wnba_data_no_na <- wnba_data[!is.na(wnba_data$PTS) & !is.na(wnba_data$X2024.SALARY) & wnba_data$PTS != 0, ]
avg_salary_per_point_WNBA <- mean((wnba_data_no_na$X2024.SALARY)/(wnba_data_no_na$PTS))
print(avg_salary_per_point_WNBA)
# $18,532.17

# Difference in Dollar/point
diff_dolperpt <- abs(avg_salary_per_point_NBA - avg_salary_per_point_WNBA)
print(diff_dolperpt)
# 831,546.30


