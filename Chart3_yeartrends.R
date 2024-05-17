# Author: Aleah Rosner
# This script plots a line graph of comparing the change in average salary from 2016 to 2024 among NBA vs WNBA players.

# Import packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

# View excel files
wnba_2024 <- read.csv("C:/Users/aever/Documents/INFO201/exploratory-analysis-charliebond1/CSVs/simple_wnba.csv")
View(wnba_2024)
wnba_2016 <- read.csv("C:/Users/aever/Documents/INFO201/exploratory-analysis-charliebond1/CSVs/wnba-stats_out.csv")
View(wnba_2016)
nba_2024 <- read.csv("C:/Users/aever/Documents/INFO201/exploratory-analysis-charliebond1/CSVs/simple_nba.csv")
View(nba_2024)
nba_2016 <- read.csv("C:/Users/aever/Documents/INFO201/exploratory-analysis-charliebond1/CSVs/nba-stats_out.csv")
View(nba_2016)

# Find the average salary in each dataframe
wnba_2024_salary_avg <- mean(wnba_2024$X2024.SALARY, na.rm = TRUE)
print(wnba_2024_salary_avg)
wnba_2016_salary_avg <- mean(wnba_2016$salary, na.rm = TRUE)
print(wnba_2016_salary_avg)
nba_2024_salary_avg <- mean(nba_2024$Salary, na.rm = TRUE)
print(nba_2024_salary_avg)
nba_2016_salary_avg <- mean(nba_2016$salary, na.rm = TRUE)
print(nba_2016_salary_avg)

# find the fold change between 2016 and 2024 for both leagues
wnba_2016_salary_foldchange <- wnba_2016_salary_avg/wnba_2016_salary_avg
print(wnba_2016_salary_foldchange)
nba_2016_salary_foldchange <- nba_2016_salary_avg/nba_2016_salary_avg
print(nba_2016_salary_foldchange)
wnba_2024_salary_foldchange <- wnba_2024_salary_avg/wnba_2016_salary_avg
print(wnba_2024_salary_foldchange)
nba_2024_salary_foldchange <- nba_2024_salary_avg/nba_2016_salary_avg
print(nba_2024_salary_foldchange)

# Create a chart using the averages
wnba_nba_avg_salary_chart <- matrix(c(wnba_2016_salary_foldchange, nba_2016_salary_foldchange, wnba_2024_salary_foldchange, nba_2024_salary_foldchange),
             nrow = 2,
             byrow = TRUE)
rownames(wnba_nba_avg_salary_chart) <- c("2016", "2024")
colnames(wnba_nba_avg_salary_chart) <- c("WNBA", "NBA")
View(wnba_nba_avg_salary_chart)

# Create a bar graph comparing the change in salaries in each year
wnba_nba_avg_salary_chart_df <- as.data.frame(as.table(wnba_nba_avg_salary_chart))
colnames(wnba_nba_avg_salary_chart_df) <- c("Year", "League", "Value")
ggplot(wnba_nba_avg_salary_chart_df, aes(x = League, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Increase in Salary from 2016 to 2024", x = "League", y = "Fold Change in Salary") +
  theme_minimal()
