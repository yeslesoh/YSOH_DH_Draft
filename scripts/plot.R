install.packages("ggrepel")
library(ggplot2)
library(plotly)
library(ggrepel)

# Combined district cases 2023 --------------------------------------------
ggplot(
  data = cases_2023,
  mapping = aes(
    x = report_date,
    y = number_of_cases,
    color = District
  )
) +
  geom_line() +
  labs(title = "COVID-19 Cases by District (2023)",
       x = "Report Date",
       y = "Number of Cases",
       color = "District") +
  theme_minimal() 


##### cases
# Plot using ggplot with interactive features
p <- ggplot(cases_2023, aes(x = report_date, y = number_of_cases, color = District, label = number_of_cases)) +
  geom_line() +
  geom_point() +  # Add points
  geom_text_repel(data = subset(cases_2023, FALSE), aes(x = report_date, y = number_of_cases, label = number_of_cases), 
                  show.legend = FALSE) +  # Invisible initially
  labs(title = "COVID-19 Cases by District (2023)",
       x = "Report Date",
       y = "Number of Cases",
       color = "District") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend to the bottom for better visibility

# Convert ggplot to plotly object for interactivity
ggplotly(p, tooltip = c("x", "y", "color"), dynamicTicks = TRUE) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))


##### hosp
p <- ggplot(hosp_2023, aes(x = report_date, y = number_of_hospitalizations, color = District, label = number_of_hospitalizations)) +
  geom_line() +
  geom_point() +  # Add points
  geom_text_repel(data = subset(hosp_2023, FALSE), aes(x = report_date, y = number_of_hospitalizations, label = number_of_hospitalizations), 
                  show.legend = FALSE) +  # Invisible initially
  labs(title = "COVID-19 Hospitalizations by District (2023)",
       x = "Report Date",
       y = "Number of Hospitalizations",
       color = "District") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend to the bottom for better visibility

# Convert ggplot to plotly object for interactivity
ggplotly(p, tooltip = c("x", "y", "color"), dynamicTicks = TRUE) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))



# Hospitalizations PW (2023) ----------------------------------------------
p <- ggplot(hosp_pw, aes(x = report_date, y = number_of_hospitalizations, color = health_district, label = number_of_hospitalizations)) +
  geom_line() +
  geom_point() +  # Add points
  geom_text_repel(data = subset(hosp_pw, FALSE), aes(x = report_date, y = number_of_hospitalizations, label = number_of_hospitalizations), 
                  show.legend = FALSE) +  # Invisible initially
  labs(title = "COVID-19 Hospitalizations by District (2023)",
       x = "Report Date",
       y = "Number of Hospitalizations",
       color = "Health District") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend to the bottom for better visibility

# Convert ggplot to plotly object for interactivity
ggplotly(p, tooltip = c("x", "y", "color"), dynamicTicks = TRUE) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))


# Alexandria by age group_1.16.24 -----------------------------------------
# Reshape the data for plotting
alexandria_Jan2024_long <- alexandria_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count")

# Reorder the levels of the variable factor to move "number_of_deaths" to the last position
alexandria_Jan2024_long$variable <- factor(alexandria_Jan2024_long$variable,
                                           levels = c("number_of_cases", "number_of_hospitalizations", "number_of_deaths"))

# Create a ggplot object
p <- ggplot(alexandria_Jan2024_long, aes(x = age_group, y = count, fill = variable, text = paste("Count:", count))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +  # Log scale for y-axis
  labs(title = "COVID-19 Counts by Age Group in Alexandria (January 2024)",
       x = "Age Group",
       y = "Count (log scale)",
       fill = "Variable") +
  scale_fill_manual(values = c("number_of_cases" = "blue", 
                               "number_of_hospitalizations" = "orange", 
                               "number_of_deaths" = "red")) +  # Custom fill colors
  theme_minimal()

# Convert ggplot to plotly object for interactivity
ggplotly(p, tooltip = "text")

# Arlington by age group_1.16.24 ------------------------------------------
# Reshape the data for plotting
arlington_Jan2024_long <- arlington_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count")

# Reorder the levels of the variable factor to move "number_of_deaths" to the last position
arlington_Jan2024_long$variable <- factor(arlington_Jan2024_long$variable,
                                           levels = c("number_of_cases", "number_of_hospitalizations", "number_of_deaths"))

# Create a ggplot object
p <- ggplot(arlington_Jan2024_long, aes(x = age_group, y = count, fill = variable, text = paste("Count:", count))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +  # Log scale for y-axis
  labs(title = "COVID-19 Counts by Age Group in Arlington (January 2024)",
       x = "Age Group",
       y = "Count (log scale)",
       fill = "Variable") +
  scale_fill_manual(values = c("number_of_cases" = "blue", 
                               "number_of_hospitalizations" = "orange", 
                               "number_of_deaths" = "red")) +  # Custom fill colors
  theme_minimal()

# Convert ggplot to plotly object for interactivity
ggplotly(p, tooltip = "text")

# Fairfax by age group_1.16.24 --------------------------------------------
# Reshape the data for plotting
fairfax_Jan2024_long <- fairfax_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count")

# Reorder the levels of the variable factor to move "number_of_deaths" to the last position
fairfax_Jan2024_long$variable <- factor(fairfax_Jan2024_long$variable,
                                          levels = c("number_of_cases", "number_of_hospitalizations", "number_of_deaths"))

# Create a ggplot object
p <- ggplot(fairfax_Jan2024_long, aes(x = age_group, y = count, fill = variable, text = paste("Count:", count))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +  # Log scale for y-axis
  labs(title = "COVID-19 Counts by Age Group in Fairfax (January 2024)",
       x = "Age Group",
       y = "Count (log scale)",
       fill = "Variable") +
  scale_fill_manual(values = c("number_of_cases" = "blue", 
                               "number_of_hospitalizations" = "orange", 
                               "number_of_deaths" = "red")) +  # Custom fill colors
  theme_minimal()

# Convert ggplot to plotly object for interactivity
ggplotly(p, tooltip = "text")

# Loudoun by age group_1.16.24 --------------------------------------------
# Reshape the data for plotting
loudoun_Jan2024_long <- loudoun_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count")

# Reorder the levels of the variable factor to move "number_of_deaths" to the last position
loudoun_Jan2024_long$variable <- factor(loudoun_Jan2024_long$variable,
                                        levels = c("number_of_cases", "number_of_hospitalizations", "number_of_deaths"))

# Create a ggplot object
p <- ggplot(loudoun_Jan2024_long, aes(x = age_group, y = count, fill = variable, text = paste("Count:", count))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +  # Log scale for y-axis
  labs(title = "COVID-19 Counts by Age Group in Loudoun (January 2024)",
       x = "Age Group",
       y = "Count (log scale)",
       fill = "Variable") +
  scale_fill_manual(values = c("number_of_cases" = "blue", 
                               "number_of_hospitalizations" = "orange", 
                               "number_of_deaths" = "red")) +  # Custom fill colors
  theme_minimal()

# Convert ggplot to plotly object for interactivity
ggplotly(p, tooltip = "text")

# PW by age group_1.16.24 -------------------------------------------------
# Reshape the data for plotting
pw_Jan2024_long <- prince_william_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count")

# Reorder the levels of the variable factor to move "number_of_deaths" to the last position
pw_Jan2024_long$variable <- factor(pw_Jan2024_long$variable,
                                        levels = c("number_of_cases", "number_of_hospitalizations", "number_of_deaths"))

# Create a ggplot object
p <- ggplot(pw_Jan2024_long, aes(x = age_group, y = count, fill = variable, text = paste("Count:", count))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +  # Log scale for y-axis
  labs(title = "COVID-19 Counts by Age Group in Prince William (January 2024)",
       x = "Age Group",
       y = "Count (log scale)",
       fill = "Variable") +
  scale_fill_manual(values = c("number_of_cases" = "blue", 
                               "number_of_hospitalizations" = "orange", 
                               "number_of_deaths" = "red")) +  # Custom fill colors
  theme_minimal()

# Convert ggplot to plotly object for interactivity
ggplotly(p, tooltip = "text")

# Combine all district plots into one viz ---------------------------------
# Create a function to generate plotly plots
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

# Reshape and plot data for each district
alexandria_plot <- generate_plot(alexandria_Jan2024_long, "COVID-19 Counts by Age Group in Alexandria (January 2024)")
arlington_plot <- generate_plot(arlington_Jan2024_long, "COVID-19 Counts by Age Group in Arlington (January 2024)")
fairfax_plot <- generate_plot(fairfax_Jan2024_long, "COVID-19 Counts by Age Group in Fairfax (January 2024)")
loudoun_plot <- generate_plot(loudoun_Jan2024_long, "COVID-19 Counts by Age Group in Loudoun (January 2024)")
pw_plot <- generate_plot(pw_Jan2024_long, "COVID-19 Counts by Age Group in Prince William (January 2024)")

# Combine plots into one page with dropdown menu
subplot(alexandria_plot, arlington_plot, fairfax_plot, loudoun_plot, pw_plot,
        nrows = 5, titleX = TRUE, titleY = TRUE, 
        titleXposition = "middle", titleYposition = "middle",
        shareX = TRUE, shareY = TRUE)