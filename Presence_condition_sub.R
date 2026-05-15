library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(ez)

# ------- 0. LOAD & CLEAN DATA --------------------------

#raw <- read_excel("our_data.xlsx", sheet = "VAS")
raw <- read_excel("6. Semester/Projekt/Databehandling/our_data.xlsx", sheet = "Pain+Presence")

# Keep only the columns we need and give them tidy names
df <- raw %>%
  select(
    id = `Participant ID.`,
    condition = Condition,
    presence_score = `Presence Score`,
    responsive = s_2,
    natural = `s_3`,
    consistent = `s_7`,
    involved = `s_13`,
    subitems = `Subitems`
  ) %>%
  
  filter(!(id%in% c(4,5)))


#------ 1. find complete participants
# Participants with full data (all 4 conditions present and no NA in 'change')
complete_ids <- df %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  filter(n == 4) %>%
  pull(id)

#filter final data
df_complete <- df %>% filter(id %in% complete_ids)

cat("Participants with complete Data sets:", paste(complete_ids, collapse = ", "), "\n")
cat("N =", length(complete_ids), "\n\n")


#----------- make factors------------
#create two factors from the dataset
df_complete <- df_complete %>%
  mutate(
    arm = case_when(
      condition %in% c("Arm-vibrations", "Arm+hand-vibrations") ~ "ON",
      TRUE ~ "OFF"
    ),
    hand = case_when(
      condition %in% c("Hand-vibration", "Arm+hand-vibrations") ~ "ON",
      TRUE ~ "OFF"
    )
  ) %>%
  mutate(
    arm = factor(arm),
    hand = factor(hand)
  )

#----------- 2. check for normality

#extract residuals
lm_model <- lm(subitems ~ arm * hand + factor(id), data = df_complete)

#get residuals
res <- residuals(lm_model)

#test for normality
norm_results <- shapiro.test(res)

print(norm_results)

#visual check
qqnorm(res)
qqline(res)


#-----------2x2 RM ANOVA--------------
cat("\n=== 2x2 REPEATED-MEASURES ANOVA (parametric path) ===\n")

aov_model <- ezANOVA(
  data = df_complete %>% mutate(id = factor(id)),
  dv = subitems,
  wid = id,
  within = .(arm, hand),   # the TWO factors
  type = 3,
  detailed = TRUE
)

print(aov_model)


#--------t-test-----
#bot residuals is normal and ANOVA is valid, so we schould use t-test
#“Since you are using a parametric ANOVA, your post-hoc tests should also be parametric (paired t-tests), not non-parametric (Wilcoxon).”
cat("\n=== T TEST ===\n")

pairwise_results <- lapply(pairs, function(pair) {
  
  data_wide <- df_complete %>%
    filter(condition %in% pair) %>%
    select(id, condition, subitems) %>%
    pivot_wider(names_from = condition, values_from = subitems)
  
  t_test <- t.test(
    data_wide[[pair[1]]],
    data_wide[[pair[2]]],
    paired = TRUE
  )
  
  data.frame(
    cond_1 = pair[1],
    cond_2 = pair[2],
    t = t_test$statistic,
    df = as.numeric(t_test$parameter),
    p_raw = t_test$p.value,
    p_bonferroni = min(t_test$p.value * n_pairs, 1),
    significant = ifelse(min(t_test$p.value * n_pairs, 1) < .05, "YES", "NO")
  )
})


pairwise_df <- bind_rows(pairwise_results)
print(pairwise_df)

#----- desriptive statistics


desc <- df_complete %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    mean_presence = mean(subitems),
    sd_presence = sd(subitems),
    .groups = "drop"
  ) %>%
  mutate(
    se_presence = sd_presence / sqrt(n),
    t_crit = qt(0.975, df = n - 1),
    ci_lower = mean_presence - t_crit * se_presence,
    ci_upper = mean_presence + t_crit * se_presence
  )



# ------- Plots --------------------------

#REORDER CONDITIONS

df_complete$condition <- factor(
  df_complete$condition,
  levels = c(
    "No-vibrations",
    "Hand-vibration",
    "Arm-vibrations",
    "Arm+hand-vibrations"
    
  )
)



desc$condition <- factor(
  desc$condition,
  levels = levels(df_complete$condition)
)



y_max <- max(df_complete$subitems, na.rm = TRUE)


p <- ggplot(df_complete, aes(x = condition, y = subitems)) +
  
  
  #individual data points
  geom_jitter(
    width = 0.1,
    alpha = 0.5,
    color = "grey40",
    size = 2,
  ) +
  
  # CONNECTED LINES PER PARTICIPANT
  #geom_line(
  # aes(group = id),
  #color = "grey70",
  #linewidth = 0.6,
  #alpha = 0.6
  #) +
  
  #mean point
  geom_point(
    data = desc,
    aes(x = condition, y = mean_presence),
    color = "black",
    size = 4,
    inherit.aes = FALSE
    
  ) +
  
  
  #confidence intervals
  geom_errorbar(
    data = desc,
    aes(
      x = condition,
      ymin = ci_lower,
      ymax = ci_upper
    ),
    inherit.aes = FALSE,
    linewidth = 0.7,
    width = 0.15
  ) +
  
  
  # Mean value labels
  geom_text(
    data = desc,
    aes(
      x = condition,
      y = mean_presence,
      label = sprintf("%.2f", mean_presence)
    ),
    inherit.aes = FALSE,
    hjust = +1.8,
    fontface = "bold",
    size = 4
    
  ) +
  labs(
    title = "Presence Sub Scores Across Conditions",
    x = "Condition",
    y = "Presence Sub Score"
  ) +
  
    theme_minimal(base_size = 13)+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),  # x-axis values
    axis.text.y = element_text(size = 16)   # y-axis values
    
  )+
    annotate(
    "segment",
    x = 1, xend = 4,
    y = y_max + 0.3, yend = y_max + 0.3
  ) +
  
  # second line
  annotate(
    "segment",
    x = 2, xend = 4,
    y = y_max + 0.5, yend = y_max + 0.5
  )+
  geom_text(
    aes(x = 3, y = y_max + 0.55, label = "*"),
    size = 6,
    inherit.aes = FALSE
  )+

geom_text(
  aes(x = 2.5, y = y_max + 0.35, label = "*"),
  size = 6,
  inherit.aes = FALSE
)

print(p)

