install.packages("ggrepel")
library(ggplot2)
library(plotly)
library(ggrepel)
library(tidyverse)

# Alexandria by age group_1.16.24 -----------------------------------------
# Reshape the data for plotting
alexandria_Jan2024_long <- alexandria_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count")

# Reorder the levels of the variable factor to move "number_of_deaths" to the last position
alexandria_Jan2024_long$variable <- factor(alexandria_Jan2024_long$variable,
                                           levels = c("number_of_cases", "number_of_hospitalizations", "number_of_deaths"))

# Arlington by age group_1.16.24 ------------------------------------------
# Reshape the data for plotting
arlington_Jan2024_long <- arlington_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count")

# Reorder the levels of the variable factor to move "number_of_deaths" to the last position
arlington_Jan2024_long$variable <- factor(arlington_Jan2024_long$variable,
                                          levels = c("number_of_cases", "number_of_hospitalizations", "number_of_deaths"))

# Fairfax by age group_1.16.24 --------------------------------------------
# Reshape the data for plotting
fairfax_Jan2024_long <- fairfax_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count")

# Reorder the levels of the variable factor to move "number_of_deaths" to the last position
fairfax_Jan2024_long$variable <- factor(fairfax_Jan2024_long$variable,
                                        levels = c("number_of_cases", "number_of_hospitalizations", "number_of_deaths"))

# Loudoun by age group_1.16.24 --------------------------------------------
# Reshape the data for plotting
loudoun_Jan2024_long <- loudoun_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count")

# Reorder the levels of the variable factor to move "number_of_deaths" to the last position
loudoun_Jan2024_long$variable <- factor(loudoun_Jan2024_long$variable,
                                        levels = c("number_of_cases", "number_of_hospitalizations", "number_of_deaths"))

# PW by age group_1.16.24 -------------------------------------------------
# Reshape the data for plotting
pw_Jan2024_long <- prince_william_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count")

# Reorder the levels of the variable factor to move "number_of_deaths" to the last position
pw_Jan2024_long$variable <- factor(pw_Jan2024_long$variable,
                                   levels = c("number_of_cases", "number_of_hospitalizations", "number_of_deaths"))

# Function to generate plotly plots ---------------------------------------
generate_plot <- function(data, title) {
  p <- ggplot(data, aes(x = age_group, y = count, fill = variable, text = paste("Count:", count))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_log10() +  # Log scale for y-axis
    labs(title = title,
         x = "Age Group",
         y = "Count (log scale)",
         fill = "Variable") +
    scale_fill_manual(values = c("number_of_cases" = "blue", 
                                 "number_of_hospitalizations" = "orange", 
                                 "number_of_deaths" = "red")) +  # Custom fill colors
    theme_minimal()
  return(ggplotly(p, tooltip = "text"))
}

# Plotly plots for each district ------------------------------------------
alexandria_plot <- generate_plot(alexandria_Jan2024_long, "COVID-19 Counts by Age Group in Alexandria (January 2024)")
arlington_plot <- generate_plot(arlington_Jan2024_long, "COVID-19 Counts by Age Group in Arlington (January 2024)")
fairfax_plot <- generate_plot(fairfax_Jan2024_long, "COVID-19 Counts by Age Group in Fairfax (January 2024)")
loudoun_plot <- generate_plot(loudoun_Jan2024_long, "COVID-19 Counts by Age Group in Loudoun (January 2024)")
pw_plot <- generate_plot(pw_Jan2024_long, "COVID-19 Counts by Age Group in Prince William (January 2024)")
