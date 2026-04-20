library(tidyverse)

delegation_2016 <- data %>%
  filter(Year == 2016, Season == "Summer") %>%
  group_by(NOC) %>%
  summarise(athletes = n_distinct(ID), .groups = "drop") %>%
  filter(athletes > 0)

ggplot(delegation_2016, aes(x = athletes)) +
  geom_histogram(
    binwidth = 20,
    fill = "#C73E2B",
    color = "white",
    boundary = 0
  ) +
  labs(
    title = "Distribution of Delegation Sizes (Rio 2016)",
    x = "Number of Athletes per Nation",
    y = "Number of Nations (NOCs)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#16324F"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Top 10% of NOCs by athlete count
top10_threshold <- quantile(delegation_2016$athletes, 0.90)

top10_nocs <- delegation_2016 %>%
  filter(athletes >= top10_threshold)

# Share of total athletes they represent
top10_share <- sum(top10_nocs$athletes) / sum(delegation_2016$athletes) * 100

top10_share
