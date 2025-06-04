library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)

##Loading in data
table1 <- read_csv("dailyActivity_merged.csv")
table2 <- read_csv("dailyCalories_merged.csv")
table3 <- read_csv("dailyIntensities_merged.csv")
table4 <- read_csv("dailySteps_merged.csv")

##rename date column
table1 <- table1 %>%
  rename(ActivityDay = ActivityDate)

# Combine the active minutes columns into a new 'total_active_minutes' column
table3 <- table3 %>%
  mutate(TotalActiveMinutes = LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes)

##Delete unneeded columns
table3 <- table3 %>%
   select(-LightlyActiveMinutes,
          -FairlyActiveMinutes,
          -VeryActiveMinutes,
          -SedentaryMinutes,
          -SedentaryActiveDistance,
          -LightActiveDistance,
          -ModeratelyActiveDistance,
          -VeryActiveDistance)
table1 <- table1 %>%
  select(-TotalDistance,
         -TrackerDistance,
         -LoggedActivitiesDistance,
         -VeryActiveDistance,
         -VeryActiveMinutes,
         -LightActiveDistance,
         -LightlyActiveMinutes,
         -VeryActiveDistance,
         -VeryActiveMinutes,
         -ModeratelyActiveDistance,
         -SedentaryActiveDistance,
         -SedentaryMinutes,
         -FairlyActiveMinutes)

##Combine tables
Daily_metrics <- combined_daily_metrics %>%
  full_join(table1, by = c("Id", "ActivityDay")) %>%
  full_join(table3, by = c("Id", "ActivityDay")) 
  

glimpse(combined_daily_metrics)
glimpse(table1)
glimpse(table2)
glimpse(table3)
glimpse(table4)

##Fix combined Data
# --- 1. Prepare table1 (our primary source for steps and calories) ---
# Rename Id and ActivityDay for consistency, and convert ActivityDay to Date type
daily_activity_data <- table1 %>%
  rename(
    id = Id,
    ActivityDate = ActivityDay
  ) %>%
  mutate(ActivityDate = mdy(ActivityDate)) # '4/12/2016' is Month/Day/Year format

glimpse(daily_activity_data) # Check the cleaned table1

# --- 2. Prepare table3 (our source for TotalActiveMinutes) ---
# Rename Id and ActivityDay for consistency, and convert ActivityDay to Date type
daily_active_minutes_data <- table3 %>%
  rename(
    id = Id,
    ActivityDate = ActivityDay
  ) %>%
  mutate(ActivityDate = mdy(ActivityDate)) # '4/12/2016' is Month/Day/Year format

glimpse(daily_active_minutes_data) # Check the cleaned table3

# --- 3. Perform the full_join ---
# Join the prepared activity data with the prepared active minutes data.
# We're specifically choosing these two because they contain all the unique columns
# you've identified from your four tables.
combined_data_clean <- daily_activity_data %>%
  full_join(daily_active_minutes_data, by = c("id", "ActivityDate"))

# --- 4. Inspect the final combined data ---
glimpse(combined_data_clean)

# Check for any remaining NA values (expected if some Id/ActivityDate pairs
# exist in one table but not the other, though unlikely given your glimpses)
colSums(is.na(combined_data_clean))

# View the first few rows
head(combined_data_clean)

##Create visual to see trends
install.packages("ggplot2")
library(ggplot2)

## Trend of TotalSteps over time for all users

ggplot(data = combined_data_clean, aes(x = ActivityDate, y = TotalSteps, group = id, color = as.factor(id))) +
  geom_line(alpha = 0.6) + # alpha makes lines slightly transparent if they overlap
  labs(
    title = "Daily Total Steps for All Users",
    x = "Date",
    y = "Total Steps",
    color = "User ID"
  ) +
  theme_minimal() + # A clean theme
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") # Format x-axis for dates

## Trend of All metics for All Users

# This is where daily_avg_steps comes from:
daily_avg_steps <- combined_data_clean %>%
  group_by(ActivityDate) %>% # Group the data by each unique date
  summarize(
    AvgTotalSteps = mean(TotalSteps, na.rm = TRUE), # Calculate the average TotalSteps for that date
    AvgCalories = mean(Calories, na.rm = TRUE),     # Calculate the average Calories for that date
    AvgTotalActiveMinutes = mean(TotalActiveMinutes, na.rm = TRUE) # Calculate the average TotalActiveMinutes for that date
  )

# First, reshape your 'daily_avg_steps' data to a 'long' format for easier plotting of multiple metrics
# This uses the 'pivot_longer' function from 'tidyr' (part of tidyverse)

daily_avg_long <- daily_avg_steps %>%
  pivot_longer(
    cols = c(AvgTotalSteps, AvgCalories, AvgTotalActiveMinutes), # Columns to pivot
    names_to = "Metric",                                    # New column for metric names
    values_to = "Value"                                     # New column for metric values
  )

ggplot(data = daily_avg_long, aes(x = ActivityDate, y = Value, color = Metric)) +
  geom_line(size = 1) +
  labs(
    title = "Average Daily Trends: Steps, Calories, and Active Minutes",
    x = "Date",
    y = "Average Value"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  # Optional: Use facet_wrap to put each metric on its own plot if scales are very different
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) # 'free_y' allows each plot to have its own y-axis scale
