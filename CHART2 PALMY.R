

#All the required packages
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)

#Downloading csv data into R
wnba_data <- read.csv("Desktop/info201/exploratory-analysis-charliebond1/CSVs/simple_wnba.csv")
nba_data <- read.csv("Desktop/info201/exploratory-analysis-charliebond1/CSVs/simple_nba.csv")
 
#Summerize data
nba_summary <- summary(nba_data$Salary)
wnba_summary <- summary(wnba_data$X2024.SALARY)

#Print data summary 
print(nba_summary)
print(wnba_summary)

#Adjust Y axis for better reading going up by 1,000,000
y_max <- max(nba_summary[6], wnba_summary[6]) * 1.1  
y_breaks <- seq(0, y_max, by = 1000000) 

#create summary label by vector
summary_labels <- data.frame(
  x = rep(c(1, 2), each = 5),
  y = c(wnba_summary[c(2, 4, 3, 6, 1)], nba_summary[c(2, 4, 3, 6, 1)]),
  label = rep(c("Q1", "Q3", "Median", "Max", "Min"), 2)
)

# Using ggplot2 to construct a box plot
p <- ggplot() +
  geom_boxplot(data = wnba_data, aes(x = factor(1), y = X2024.SALARY), width = 0.7, fill = "lightgreen", alpha = 0.7) +
  geom_boxplot(data = nba_data, aes(x = factor(2), y = Salary), width = 0.7, fill = "lightblue", alpha = 0.7) +
  ggtitle("Comparison of NBA and WNBA Player Salaries") +
  ylab("Salary ($)") +
  scale_y_continuous(labels = scales::comma, breaks = y_breaks, limits = c(0, y_max)) + # Set y-axis limits for better visibility
  scale_x_discrete(labels = c("WNBA", "NBA")) +
  geom_text(data = summary_labels, aes(x = x, y = y, label = label), vjust = -0.5, size = 3) +
  theme_minimal()

# Make it more readible by making it adjustable by convert ggplot2 plot to an interactive plotly plot so that it can be zoom and hover
interactive_plot <- ggplotly(p)

# Print the interactive plot
print(interactive_plot)

