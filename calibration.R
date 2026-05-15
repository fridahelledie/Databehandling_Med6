library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

raw <- read_excel("our_data.xlsx", sheet = "Calibrate")

#raw <- read_excel("6. Semester/Projekt/Databehandling/our_data.xlsx", sheet = "Calibrate")


long <- raw %>%
  pivot_longer(
    cols = -`Participant ID`,
    names_to = "mAh",
    values_to = "VAS"
  )


long <- long %>%
  mutate(
    mAh = as.numeric(mAh),
    VAS = as.numeric(VAS)
  ) %>%
  filter(!is.na(VAS))


plot_data <- long %>%
  filter(`Participant ID` %in% c(11,12,13,14))


x_min <- min(long$mAh, na.rm = TRUE)
x_max <- max(long$mAh, na.rm = TRUE)

y_min <- min(long$VAS, na.rm = TRUE)
y_max <- max(long$VAS, na.rm = TRUE)


p <- ggplot(plot_data, aes(x = mAh, y = VAS, color = factor(`Participant ID`))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Calibration curves",
    x = "Stimuli Intensity in mAh",
    y = "VAS Score",
    color = "Participant"
  ) +
  theme_minimal(base_size = 13)+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),  # x-axis values
    axis.text.y = element_text(size = 16)   # y-axis values
    
  )+ coord_cartesian(xlim = c(x_min, x_max),
                  ylim = c(y_min, y_max))


plot(p)


last_VAS <- long %>%
  arrange(`Participant ID`, mAh) %>%   # order correctly
  group_by(`Participant ID`) %>%
  slice_tail(n = 1) %>%                # take LAST row per participant
  ungroup()


mean_last_VAS <- last_VAS %>%
  summarise(mean_VAS = mean(VAS, na.rm = TRUE))

mean_last_VAS

cat("Average final VAS score:", mean_last_VAS$mean_VAS)


sd_last_VAS <- last_VAS %>%
  summarise(sd_VAS = sd(VAS, na.rm = TRUE))

sd_last_VAS
cat("Standard deviation of final VAS:", sd_last_VAS$sd_VAS)
