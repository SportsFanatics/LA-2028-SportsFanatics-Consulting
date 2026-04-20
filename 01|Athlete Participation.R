library(tidyverse)

summer_participation <- data %>%
  filter(Season == "Summer") %>%
  group_by(Year) %>%
  summarise(total_athletes = n_distinct(ID), .groups = "drop")

ggplot(summer_participation, aes(x = Year, y = total_athletes)) +
  geom_line(color = "#16324F", linewidth = 1.2) +
  geom_point(color = "#16324F", size = 2.5) +
  labs(
    title = "Total Athlete Participation (Summer Games)",
    x = "Year",
    y = "Total Athletes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#16324F"),
    axis.title = element_text(face = "bold", color = "#16324F"),
    panel.grid.minor = element_blank()
  )