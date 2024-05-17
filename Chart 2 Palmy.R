# Install plotly package if not already installed
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)

# Load the data
wnba_data <- read.csv("Desktop/info201/exploratory-analysis-charliebond1/CSVs/simple_wnba.csv")
nba_data <- read.csv("Desktop/info201/exploratory-analysis-charliebond1/CSVs/simple_nba.csv")

# Calculate summaries
nba_summary <- summary(nba_data$Salary)
wnba_summary <- summary(wnba_data$X2024.SALARY)

# Print summaries for debugging
print(nba_summary)
print(wnba_summary)

# Define custom y-axis breaks if needed (adjust as necessary)
y_max <- max(nba_summary[6], wnba_summary[6]) * 1.1 # Add 10% padding to the max value for better visibility
y_breaks <- seq(0, y_max, by = 1000000) # Example breaks every $1M

# Combine summary statistics into a data frame for labeling
summary_labels <- data.frame(
  x = rep(c(1, 2), each = 5),
  y = c(wnba_summary[c(2, 4, 3, 6, 1)], nba_summary[c(2, 4, 3, 6, 1)]),
  label = rep(c("Q1", "Q3", "Median", "Max", "Min"), 2)
)

# Create the ggplot2 box plot
p <- ggplot() +
  geom_boxplot(data = wnba_data, aes(x = factor(1), y = X2024.SALARY), width = 0.7, fill = "lightgreen", alpha = 0.7) +
  geom_boxplot(data = nba_data, aes(x = factor(2), y = Salary), width = 0.7, fill = "lightblue", alpha = 0.7) +
  ggtitle("Comparison of NBA and WNBA Player Salaries") +
  ylab("Salary ($)") +
  scale_y_continuous(labels = scales::comma, breaks = y_breaks, limits = c(0, y_max)) + # Set y-axis limits for better visibility
  scale_x_discrete(labels = c("WNBA", "NBA")) +
  geom_text(data = summary_labels, aes(x = x, y = y, label = label), vjust = -0.5, size = 3) +
  theme_minimal()

# Convert ggplot2 plot to an interactive plotly plot
interactive_plot <- ggplotly(p)

# Print the interactive plot
print(interactive_plot)

