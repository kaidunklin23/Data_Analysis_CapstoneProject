library(dplyr)
library(lubridate)
library(ggplot2)

# Ensure combined_data_clean is your final, consolidated data frame
# (id, Activity Date, Total Steps, Calories, Total Active Minutes)

# a. Extract Day of the Week
combined_data_clean_with_day <- combined_data_clean %>%
  mutate(
    day_of_week = wday(ActivityDate, label = TRUE, abbr = FALSE) # Full day name (e.g., "Monday")
  )

# b. Calculate Average by Day of Week
weekly_avg_trends <- combined_data_clean_with_day %>%
  group_by(day_of_week) %>%
  summarize(
    AvgSteps = mean(TotalSteps, na.rm = TRUE),
    AvgCalories = mean(Calories, na.rm = TRUE),
    AvgActiveMinutes = mean(TotalActiveMinutes, na.rm = TRUE)
  ) %>%
  ungroup()

print(weekly_avg_trends)

# c. Visualize Weekly Trends (Example for Steps)
ggplot(data = weekly_avg_trends, aes(x = day_of_week, y = AvgSteps, group = 1)) +
  geom_line(color = "darkblue", size = 1) + # Use geom_line for a cyclical pattern
  geom_point(color = "red", size = 3) +
  labs(
    title = "Average Daily Steps by Day of the Week",
    x = "Day of the Week",
    y = "Average Total Steps"
  ) +
  theme_minimal() +
  # Order the days of the week correctly
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# You can create similar plots for AvgCalories and AvgActiveMinutes
# Example for Active Minutes:
ggplot(data = weekly_avg_trends, aes(x = day_of_week, y = AvgActiveMinutes, group = 1)) +
  geom_col(fill = "forestgreen") + # Use geom_col (bar chart) for discrete categories
  labs(
    title = "Average Active Minutes by Day of the Week",
    x = "Day of the Week",
    y = "Average Active Minutes"
  ) +
  theme_minimal() +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
