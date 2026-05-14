library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rmcorr)

# ------- 0. LOAD & CLEAN DATA --------------------------

raw <- read_excel("our_data.xlsx", sheet = "Performance+Presence")

df <- raw %>% select(
  id = 'Participant',
  condition = 'Condition',
  total_ghost = 'Total Ghosts',
  captured = 'Ghosts Captured',
  total_haunted_toys = 'Total Haunted Toys',
  toys_held = 'Toys Held',
  trigger_presses = 'Trigger Presses',
  presence = "Presence Score")


# ------- 1. LINEAR MIXED MODEL --------------------------
cat("====== trigger_presses ~ Presence (LMM) ======\n")
model <- lmer(presence ~ trigger_presses + (1 | id), data = df)
s <- summary(model)
print(s)

# Extract values for plot annotation
beta <- fixef(model)["trigger_presses"]
pval <- coef(s)["trigger_presses", "Pr(>|t|)"]
r2 <- as.numeric(MuMIn::r.squaredGLMM(model)[,"R2m"])

# Format label
sig_stars <- ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "Not significant"))
annot_label <- sprintf("β = %.3f, p = %.3f %s\nMarginal R² = %.3f", beta, pval, sig_stars, r2)

# ------- 2. PLOT --------------------------
p <- ggplot(df, aes(x = trigger_presses, y = presence)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +
  annotate(
    "text",
    x = Inf, y = Inf,              # top-right corner
    hjust = 1.05, vjust = 1.5,    # nudge inward from edge
    label = annot_label,
    size = 4.5,
    fontface = "italic"
  ) +
  labs(
    y = "Presence Score",
    x = "Trigger Presses"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16)
  )

plot(p)