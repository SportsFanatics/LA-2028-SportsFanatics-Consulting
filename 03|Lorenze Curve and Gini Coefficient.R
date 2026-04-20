library(tidyverse)

noc_medals <- data %>%
  filter(Year == 2016, Season == "Summer") %>%
  group_by(NOC) %>%
  summarise(
    athletes = n_distinct(ID),
    medals = sum(!is.na(Medal)),
    .groups = "drop"
  ) %>%
  filter(!is.na(NOC), athletes > 0) %>%
  mutate(
    medals_per_athlete = medals / athletes
  ) %>%
  arrange(medals_per_athlete) %>%
  mutate(
    cum_athletes = cumsum(athletes) / sum(athletes),
    cum_medals = cumsum(medals) / sum(medals)
  )


plot_df <- bind_rows(
  tibble(cum_athletes = 0, cum_medals = 0),
  noc_medals %>% select(cum_athletes, cum_medals)
)


ggplot(plot_df, aes(x = cum_athletes, y = cum_medals)) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = "navy", linewidth = 1.2
  ) +
  geom_ribbon(
    aes(ymin = cum_medals, ymax = cum_athletes),
    fill = "lightblue", alpha = 0.4
  ) +
  geom_line(color = "steelblue", linewidth = 1.5) +
  labs(
    title = "Concentration of Medals by Athlete Share Across NOCs",
    x = "Cumulative share of athletes",
    y = "Cumulative share of medals"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#16324F"),
    axis.title = element_text(face = "bold", color = "#16324F"),
    panel.grid.minor = element_blank())

#Gini Coefficient:
install.packages("ineq")
library(ineq)

ineq(delegation_2016$athletes, type = "Gini")
