library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(MuMIn)

# ------- 0. LOAD DATA --------------------------
raw <- read_excel("our_data.xlsx", sheet = "Pain+Vibration")
df <- raw %>%
  select(
    id = 'Participant',
    condition  = 'Condition',
    left_vib = 'Left vibration',
    right_vib  = 'Right vibration',
    vas_change = 'VAS-Change',
    z_vas = 'Z-VAS'
  ) %>%
  filter(condition %in% c("ArmVibration", "AllVibration")) %>%
  mutate(
    id = factor(id),
    mean_vib  = (left_vib + right_vib) / 2   # combined predictor
  )

# ------- 1. MODELS --------------------------
cat("=== LEFT ARM VIBRATION ~ VAS CHANGE ===\n")
m_left <- lmer(vas_change ~ left_vib + (1 | id), data = df)
s_left <- summary(m_left)
print(s_left)

cat("\n=== RIGHT ARM VIBRATION ~ VAS CHANGE ===\n")
m_right <- lmer(vas_change ~ right_vib + (1 | id), data = df)
s_right <- summary(m_right)
print(s_right)

cat("\n=== MEAN ARM VIBRATION ~ VAS CHANGE ===\n")
m_mean <- lmer(vas_change ~ mean_vib + (1 | id), data = df)
s_mean <- summary(m_mean)
print(s_mean)

# ------- 2. PLOT HELPER FUNCTION --------------------------
make_vib_plot <- function(model, model_summary, data, x_var, x_label) {
  beta <- fixef(model)[x_var]
  pval <- coef(model_summary)[x_var, "Pr(>|t|)"]
  r2m <- as.numeric(r.squaredGLMM(model)[, "R2m"])
  sig  <- ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "Not significant"))
  annot <- sprintf("β = %.3f, p = %.3f %s\nMarginal R² = %.3f", beta, pval, sig, r2m)
  
  ggplot(data, aes(x = .data[[x_var]], y = vas_change)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +
    annotate("text",
             x = Inf, y = Inf,
             hjust = 1.05, vjust = 1.5,
             label = annot, size = 4.5, fontface = "italic") +
    labs(x = x_label, y = "VAS Change") +
    theme_minimal(base_size = 13) +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x  = element_text(size = 16),
      axis.text.y  = element_text(size = 16)
    )
}

# ------- 3. PLOTS --------------------------
p_left  <- make_vib_plot(m_left,  s_left,  df, "left_vib",  "Left arm vibration (Hz·s)")
p_right <- make_vib_plot(m_right, s_right, df, "right_vib", "Right arm vibration (Hz·s)")
p_mean  <- make_vib_plot(m_mean,  s_mean,  df, "mean_vib",  "Mean arm vibration (Hz·s)")

plot(p_left)
plot(p_right)
plot(p_mean)