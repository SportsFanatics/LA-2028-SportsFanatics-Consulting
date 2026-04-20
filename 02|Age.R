library(tidyverse)

age_summary <- data %>%
  filter(!is.na(Age), Season == "Summer", Year > 1950) %>%
  group_by(Year) %>%
  summarise(
    avg_age = mean(Age, na.rm = TRUE),
    lower_age = quantile(Age, 0.10, na.rm = TRUE),
    upper_age = quantile(Age, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

# Fit prediction model
age_model <- lm(avg_age ~ poly(Year, 2), data = age_summary)

# Years to predict through 2028
pred_years <- tibble(
  Year = seq(min(age_summary$Year), 2028, by = 4)
) %>%
  mutate(
    pred_age = predict(age_model, newdata = .)
  )

# Last observed year
last_data_year <- max(age_summary$Year)

ggplot() +

  geom_point(
    data = age_summary,
    aes(x = Year, y = avg_age),
    color = "#1FA08A",
    size = 3
  ) +
  geom_line(
    data = pred_years,
    aes(x = Year, y = pred_age),
    color = "#1FA08A",
    linewidth = 1.2,
    linetype = "solid"
  ) +
  geom_point(
    data = pred_years %>% filter(Year > 2016),
    aes(x = Year, y = pred_age),
    color = "#1FA08A",
    size = 3
  ) +
  geom_vline(
    xintercept = last_data_year,
    linetype = "dotted",
    color = "gray40",
    linewidth = 1
  ) +
  labs(
    title = "Average Age of Competing Olympians",
    x = "Year",
    y = "Age (Years)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#16324F"),
    axis.title = element_text(face = "bold", color = "#16324F"),
    panel.grid.minor = element_blank()
  )
