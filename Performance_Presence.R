library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rmcorr)

# ------- 0. LOAD & CLEAN DATA --------------------------

raw <- read_excel("our_data.xlsx", sheet = "Performance+Presence")
#raw <- read_excel("6. Semester/Projekt/Databehandling/our_data.xlsx", sheet = "Performance+Presence")

df <- raw %>% select(
  id = 'Participant',
  condition = 'Condition',
  total_ghost = 'Total Ghosts',
  captured = 'Ghosts Captured',
  total_haunted_toys = 'Total Haunted Toys',
  toys_held = 'Toys Held',
  trigger_presses = 'Trigger Presses',
  presence = "Presence Score")

scatter_captured <- ggplot(df, aes(presence, captured))
scatter_captured + geom_point() + geom_smooth(method = "lm") + ggtitle("Captured ghost vs Presence")

scatter_held <- ggplot(df, aes(presence, toys_held))
scatter_held + geom_point() + geom_smooth(method = "lm") + ggtitle("Toys held vs Presence")

scatter_trigger <- ggplot(df, aes(presence, trigger_presses))
scatter_trigger + geom_point() + geom_smooth(method = "lm") + ggtitle("Trigger presses vs Presence")

cor(df$presence, df$captured, use="complete.obs", method="spearman")
cor(df$presence, df$toys_held, use="complete.obs", method="spearman")
cor(df$presence, df$trigger_presses, use="complete.obs", method="spearman")