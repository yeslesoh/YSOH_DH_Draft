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

# Combine plots into one page with dropdown menu --------------------------
combined_data <- bind_rows(
  mutate(alexandria_Jan2024_long, district = "Alexandria"),
  mutate(arlington_Jan2024_long, district = "Arlington"),
  mutate(fairfax_Jan2024_long, district = "Fairfax"),
  mutate(loudoun_Jan2024_long, district = "Loudoun"),
  mutate(pw_Jan2024_long, district = "Prince William")
)

# Create function to generate plotly plot for each district
generate_plot <- function(data, district) {
  print(data[data$district == district, ])
  p <- ggplot(data[data$district == district, ], aes(x = age_group, y = count, fill = variable, text = paste("Count:", count))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_log10() +  # Log scale for y-axis
    labs(title = paste("COVID-19 Counts by Age Group in", district, "(January 2024)"),
         x = "Age Group",
         y = "Count (log scale)",
         fill = "Variable") +
    scale_fill_manual(values = c("number_of_cases" = "blue", 
                                 "number_of_hospitalizations" = "orange", 
                                 "number_of_deaths" = "red")) +  # Custom fill colors
    theme_minimal()
  
  return(ggplotly(p, tooltip = "text"))
}

# Create plots for all districts
plots <- lapply(unique(combined_data$district), function(district) {
  generate_plot(combined_data, district)
})

# Define dropdown menu options
dropdown_options <- list(
  list(
    buttons = list(
      list(method = "update",
           args = list(list(visible = c(TRUE, FALSE, FALSE, FALSE, FALSE)),
                       list(title = "COVID-19 Counts by Age Group in Alexandria (January 2024)")),
           label = "Alexandria"),
      list(method = "update",
           args = list(list(visible = c(FALSE, TRUE, FALSE, FALSE, FALSE)),
                       list(title = "COVID-19 Counts by Age Group in Arlington (January 2024)")),
           label = "Arlington"),
      list(method = "update",
           args = list(list(visible = c(FALSE, FALSE, TRUE, FALSE, FALSE)),
                       list(title = "COVID-19 Counts by Age Group in Fairfax (January 2024)")),
           label = "Fairfax"),
      list(method = "update",
           args = list(list(visible = c(FALSE, FALSE, FALSE, TRUE, FALSE)),
                       list(title = "COVID-19 Counts by Age Group in Loudoun (January 2024)")),
           label = "Loudoun"),
      list(method = "update",
           args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, TRUE)),
                       list(title = "COVID-19 Counts by Age Group in Prince William (January 2024)")),
           label = "Prince William")
    ),
    direction = "down",
    showactive = TRUE
  )
)

# Create layout with initial plot and dropdown menu
layout <- initial_plot %>% 
  layout(
    updatemenus = dropdown_options
  )

layout