library(tidyverse)
install.packages("quantreg")
library(quantreg)
data <- read.csv("~/Downloads/Athlete Events.csv")
GDPs <- read.csv("~/Downloads/Country GDPs.csv")

# Total medals per country
country_totals <- data %>%
  filter(!is.na(Medal)) %>%
  count(NOC, name = "total_medals")

# Sport distribution per country
sport_dist <- data %>%
  filter(!is.na(Medal)) %>%
  count(NOC, Sport) %>%
  group_by(NOC) %>%
  mutate(p = n / sum(n))

# Compute entropy (specialization)
specialization <- sport_dist %>%
  group_by(NOC) %>%
  summarise(
    entropy = -sum(p * log(p)),
    .groups = "drop"
  )


# Convert GDP to numeric
GDPs$IMF.2026 <- as.numeric(GDPs$IMF.2026)

country_summary <- country_totals %>%
  left_join(specialization, by = "NOC") %>%
  left_join(GDPs %>% select(NOC, IMF.2026), by = "NOC")

#Frontier Line
frontier_model <- rq(total_medals ~ poly(entropy, 2), tau = 0.95, data = country_summary)

x_grid <- tibble(
  entropy = seq(
    min(country_summary$entropy, na.rm = TRUE),
    max(country_summary$entropy, na.rm = TRUE),
    length.out = 200
  )
)

frontier_line <- x_grid %>%
  mutate(predicted_medals = predict(frontier_model, newdata = x_grid))

ggplot(country_summary, aes(x = entropy, y = total_medals, color = IMF.2026)) +
  geom_point(alpha = 0.7, size = 5) +
  geom_line(
    data = frontier_line,
    aes(x = entropy, y = predicted_medals),
    inherit.aes = FALSE,
    color = "red",
    linewidth = 1.3
  ) +
  scale_color_viridis_c(
    labels = scales::comma, 
    trans = "log10", 
    na.value = "grey50"
  ) + 
  labs(title = "Total Medals by Diversification",
       x = "Diversification (Entropy)",
       y = "Total Medals",
       color = "GDP ($)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#16324F"),
    axis.title = element_text(face = "bold", color = "#16324F"),
    legend.title = element_text(face = "bold", color = "#16324F"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
    ) +
  guides(
      color = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 20,
        barheight = 1
      )
    )


#Expected Performance Metric
country_summary2 <- country_summary %>%
  filter(!is.na(IMF.2026), IMF.2026 > 0, !is.na(total_medals))

fit <- glm(
  total_medals ~ log(IMF.2026),
  family = quasipoisson(link = "log"),
  data = country_summary2
)

country_perf <- country_summary2 %>%
  mutate(
    expected_medals = predict(fit, type = "response"),
    medal_gap = total_medals - expected_medals,
    performance_ratio = total_medals / expected_medals
  )

#Percentage Underperforming
country_perf %>%
  filter(entropy < 2) %>%
  summarise(percent_under = mean(performance_ratio < 0.5, na.rm = TRUE) * 100)

country_perf %>%
  filter(entropy > 2) %>%
  summarise(percent_under = mean(performance_ratio < 0.5, na.rm = TRUE) * 100)
