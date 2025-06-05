user_summary <- combined_data_clean %>%
  group_by(id) %>%
  summarize(
    AvgStepsPerDay = mean(TotalSteps, na.rm = TRUE),
    AvgCaloriesPerDay = mean(Calories, na.rm = TRUE),
    AvgActiveMinutesPerDay = mean(TotalActiveMinutes, na.rm = TRUE),
    DaysTracked = n_distinct(ActivityDate) # How many days each user tracked
  ) %>%
  ungroup() %>%
  arrange(desc(AvgStepsPerDay)) # See who are the most active

print(user_summary)

# You could then visualize distributions of these averages (e.g., histograms of AvgStepsPerDay)
ggplot(user_summary, aes(x = AvgStepsPerDay)) +
  geom_histogram(binwidth = 1000, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Average Daily Steps per User", x = "Average Daily Steps", y = "Number of Users") +
  theme_minimal()

write_csv(user_summary, "user_summary.csv")
