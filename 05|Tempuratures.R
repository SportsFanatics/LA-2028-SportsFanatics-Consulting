library(tidyverse)

data <- read.csv("~/Downloads/Athlete Events.csv")
temps <- read.csv("~/Downloads/la_july_avg_temps.csv")

ggplot(temps, aes(x = year, y = july_avg_temp_f)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2.5) +
  labs(title = "LA July Average Temperatures 1990-2025",
       x = "Year",
       y = "Average Temperature (°F)") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

temps <- read_csv("~/Downloads/la_july_high_low_avg.csv")
ggplot(temps, aes(x = year)) +
  geom_ribbon(aes(ymin = low, ymax = high), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = average), color = "steelblue", linewidth = 1.2) +
  labs(title = "LA July Temperatures 1990-2025",
       x = "Year",
       y = "Temperature (°F)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#16324F"),
    axis.title = element_text(face = "bold", color = "#16324F"),
    panel.grid.minor = element_blank()
  )
