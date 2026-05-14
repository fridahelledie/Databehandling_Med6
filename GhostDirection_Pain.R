library(ez)
library(lme4)
library(lmerTest)

# ------- 0. LOAD & CLEAN DATA --------------------------

raw <- read_excel("our_data.xlsx", sheet = "GhostDirection+Pain")

df <- raw %>%
  select(
    id = `Participant`,
    ghost_dir = `GhostDir`,
    ghost_dir_last = `GhostDirLast`,
    vas_change = `VAS-Change`,
    vas_z = `Z-VAS`
  )


# ------- 1. SHAPIRO-WILK NORMALITY --------------------------

sw_ghostdir <- df %>%
  group_by(ghost_dir) %>%
  summarise(
    n = n(),
    W = shapiro.test(vas_z)$statistic,
    p = shapiro.test(vas_z)$p.value,
    normal = ifelse(p > 0.05, "Yes", "No"),
    .groups = "drop"
  )
print(sw_ghostdir)


# ------- 2. GHOST DIRECTION EFFECT LLM --------------------------

cat("\n=== GHOST DIRECTION EFFECT LINEAR MIXED MODEL ===\n")

model <- lmer(vas_change ~ ghost_dir + (1 | id), data = df %>%
                mutate(ghost_dir = factor(ghost_dir)))
print(summary(model))

cat("\n=== ANOVA ===\n")
print(anova(model))
