library(readxl)
library(dplyr)
library(ggplot2)


# ------- 0. LOAD DATA --------------------------
raw <- read_excel("GhostCatchTime.xlsx", sheet = "Example1")

df <- raw %>%
  select(
    time = `Time`,
    angle_tryhard = `Angle tryhard`,
    angle_casual = `Angle casual`,
  )


# ------- 1. CREATE PLOT --------------------------
line_colours <- c(
  "Competitive Player" = "red",
  "Casual Player" = "blue",
  "Ghost lit threshold" = "black"
)

p <- ggplot(df, aes(x = time)) +
  geom_line(aes(y = angle_tryhard, colour = "Competitive Player")) +
  geom_line(aes(y = angle_casual, colour = "Casual Player")) +
  geom_line(aes(y = 18, colour = "Ghost lit threshold"), linetype = "dashed") +
  scale_colour_manual(
    name = NULL,
    values = line_colours
  ) +
  labs(
    y = "Angle to ghost (degrees)",
    x = "Time since ghost spawn (seconds)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(linetype = c("solid", "solid", "dashed")),
      label.theme = element_text(size = 13)
    )
  )

plot(p)
