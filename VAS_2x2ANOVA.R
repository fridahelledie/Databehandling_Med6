library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)


# ------- 0. LOAD & CLEAN DATA --------------------------

#raw <- read_excel("our_data.xlsx", sheet = "VAS")
raw <- read_excel("6. Semester/Projekt/Databehandling/our_data.xlsx", sheet = "VAS")

# Keep only the columns we need and give them tidy names
df <- raw %>%
  select(
    id = `Participant ID.`,
    condition = Condition,
    order = Order,
    sequence = Sequence,
    baseline = `VAS-Baseline`,
    vas = VAS,
    change = `VAS-Change`,
    pct_change = `Percent change`
  ) %>%
  # Coerce to numeric  (any non-numeric cell becomes NA)
  mutate(across(c(baseline, vas, change, pct_change), ~ suppressWarnings(as.numeric(.)))) %>%
  # Remove logging error rows
  filter(!(id %in% c(4, 5)))

# Participants with full data (all 4 conditions present and no NA in 'change')
complete_ids <- df %>%
  group_by(id) %>%
  summarise(n_valid = sum(!is.na(change))) %>%
  filter(n_valid == 4) %>%
  pull(id)

cat("Participants with complete VAS-Change data:", paste(complete_ids, collapse = ", "), "\n")
cat("N =", length(complete_ids), "\n\n")

df_complete <- df %>% filter(id %in% complete_ids)


# ------- 1. NORMALITY: SHAPIRO-WILK PER CONDITION --------------------------
# Assesses whether VAS-Change within each condition is normally distributed.
# Percent-change normality is also checked as a backup.
# Our N is quite low so power will be too. 


cat("=== SHAPIRO-WILK NORMALITY TESTS (VAS-Change per condition) ===\n")

sw_results <- df_complete %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    W = shapiro.test(change)$statistic,
    p = shapiro.test(change)$p.value,
    normal = ifelse(p > 0.05, "Yes", "No"),
    .groups = "drop"
  )
print(sw_results)

cat("\n=== SHAPIRO-WILK (Percent Change per condition) ===\n")
sw_pct <- df_complete %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    W = shapiro.test(pct_change)$statistic,
    p = shapiro.test(pct_change)$p.value,
    normal = ifelse(p > 0.05, "Yes", "No"),
    .groups = "drop"
  )
print(sw_pct)

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

cat("=== Residuals ===\n")

#extract residuals
lm_model <- lm(change ~ arm * hand + factor(id), data = df_complete)

#get residuals
res <- residuals(lm_model)

#test for normality
norm_results <- shapiro.test(res)

print(norm_results)

#visual check
qqnorm(res)
qqline(res)


# ------- DECISION GUIDE --------------------------
# If ALL conditions pass normality (p > .05):
#   -> Use repeated-measures ANOVA (parametric, Section 2a)
# If ANY condition fails normality (p =< .05):
#   -> Use Friedman test (non-parametric, Section 2b)
# ------- END GUIDE --------------------------


# ------- 2a. PARAMETRIC PATH: REPEATED-MEASURES ANOVA --------------------------
# Use when all conditions are normally distributed

cat("\n=== REPEATED-MEASURES ANOVA (parametric path) ===\n")

# Requires the 'ez' package: install.packages("ez")
if (requireNamespace("ez", quietly = TRUE)) {
  library(ez)
  aov_model <- ezANOVA(
    data = df_complete %>% mutate(id = factor(id), condition = factor(condition)),
    dv = change,
    wid = id,
    within = condition,
    type = 3,
    detailed = TRUE
  )
  print(aov_model)
  # If p < .05, proceed to pairwise comparisons (Section 3)
} else {
  cat("Package 'ez' not installed. Run: install.packages('ez')\n")
}

#-----------2x2 RM ANOVA--------------
cat("\n=== 2x2 REPEATED-MEASURES ANOVA (parametric path) ===\n")

aov_model <- ezANOVA(
  data = df_complete %>% mutate(id = factor(id)),
  dv = change,
  wid = id,
  within = .(arm, hand),   # the TWO factors
  type = 3,
  detailed = TRUE
)

print(aov_model)



# ------- 2b. NON-PARAMETRIC PATH: FRIEDMAN TEST --------------------------
# Use when normality is violated in one or more conditions

cat("\n=== FRIEDMAN TEST (non-parametric path) ===\n")

# Friedman requires a matrix: rows = participants, cols = conditions
friedman_matrix <- df_complete %>%
  select(id, condition, change) %>%
  pivot_wider(names_from = condition, values_from = change) %>%
  select(-id) %>%
  as.matrix()

friedman_result <- friedman.test(friedman_matrix)
print(friedman_result)
# If p < .05, proceed to pairwise comparisons (Section 3)

# Effect size: Kendall's W
# W = chi^2 / (n * (k-1))  where k = number of conditions
n_participants <- nrow(friedman_matrix)
k_conditions <- ncol(friedman_matrix)
kendall_W <- friedman_result$statistic / (n_participants * (k_conditions - 1))
cat(sprintf("Kendall's W (effect size) = %.3f\n", kendall_W))
# Interpretation: .1 = small, .3 = medium, .5 = large


# ------- 3. POST-HOC PAIRWISE COMPARISONS --------------------------
# Run only if the omnibus test (ANOVA or Friedman) is significant (p < .05)
# Wilcoxon signed-rank tests with Bonferroni correction are used here

cat("\n=== POST-HOC: WILCOXON SIGNED-RANK (pairwise, Bonferroni-corrected) ===\n")

conditions <- unique(df_complete$condition)
pairs <- combn(conditions, 2, simplify = FALSE)
n_pairs <- length(pairs)  # = 6 for 4 conditions

pairwise_results <- lapply(pairs, function(pair) {
  x <- df_complete$change[df_complete$condition == pair[1]]
  y <- df_complete$change[df_complete$condition == pair[2]]
  
  # Match by participant to ensure proper pairing
  data_wide <- df_complete %>%
    filter(condition %in% pair) %>%
    select(id, condition, change) %>%
    pivot_wider(names_from = condition, values_from = change)
  
  w_test <- wilcox.test(
    data_wide[[pair[1]]],
    data_wide[[pair[2]]],
    paired = TRUE,
    exact = FALSE  # avoids errors when ties exist
  )
  
  data.frame(
    cond_1 = pair[1],
    cond_2 = pair[2],
    W = w_test$statistic,
    p_raw = w_test$p.value,
    p_bonferroni = min(w_test$p.value * n_pairs, 1),  # Bonferroni correction
    significant = ifelse(min(w_test$p.value * n_pairs, 1) < .05, "YES", "NO")
  )
})

pairwise_df <- bind_rows(pairwise_results)
print(pairwise_df)

#--------t-test-----
#bot residuals is normal and ANOVA is valid, so we schould use t-test
#“Since you are using a parametric ANOVA, your post-hoc tests should also be parametric (paired t-tests), not non-parametric (Wilcoxon).”
cat("\n=== T TEST ===\n")

pairwise_results <- lapply(pairs, function(pair) {
  
  data_wide <- df_complete %>%
    filter(condition %in% pair) %>%
    select(id, condition, change) %>%
    pivot_wider(names_from = condition, values_from = change)
  
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


# ------- 4. DESCRIPTIVE STATS --------------------------

cat("\n=== DESCRIPTIVE STATISTICS ===\n")
desc <- df_complete %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    mean_change = mean(change),
    sd_change = sd(change),
    IQR_change = IQR(change),
    mean_pct = mean(pct_change, na.rm = TRUE),
    sd_pct = sd(pct_change, na.rm = TRUE),
    .groups = "drop"
  )
print(desc)

# ------- 5. Plots --------------------------

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


# Add SEM to existing descriptive stats
# <- desc %>%
# mutate(se_change = sd_change / sqrt(n))

#Add 95% CI
desc <- desc %>%
  mutate(
    se_change = sd_change / sqrt(n),
    t_crit = qt(0.975, df = n-1),
    ci_lower = mean_change - t_crit * se_change,
    ci_upper = mean_change + t_crit * se_change
    
  )

desc$condition <- factor(
  desc$condition,
  levels = levels(df_complete$condition)
)



y_max <- max(df_complete$change, na.rm = TRUE)

#add a shared jitter
pos <- position_jitter(width = 0.01, height = 0)


p <- ggplot(df_complete, aes(x = condition, y = change)) +
  

  #individual data points
  geom_point(
    position = pos,
    alpha = 0.5,
    color = "grey40",
    size = 2,
  ) +
  
  # CONNECTED LINES PER PARTICIPANT
  geom_line(
    aes(group = id),
    color = "grey70",
    linewidth = 0.6,
    alpha = 0.6
  ) +
  
  #mean point
  geom_point(
    data = desc,
    aes(x = condition, y = mean_change),
    color = "black",
    size = 4,
    inherit.aes = FALSE
    
  ) +
  

  #mean + SEM
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
      y = mean_change,
      label = sprintf("%.2f", mean_change)
    ),
    inherit.aes = FALSE,
    hjust = +1.8,
    fontface = "bold",
    size = 4
    
  ) +
  labs(
    title = "Mean VAS Baseline Change Across Conditions",
    x = "Condition",
    y = "VAS Baseline Change (negative = pain reduction)"
  ) +
  
  
  theme_minimal(base_size = 13)

#significance annotation
p_final <- p + geom_segment(
  aes(x = 1, xend = 4, y = y_max + 0.5, yend = y_max + 0.5),
  inherit.aes = FALSE
) +
  geom_text(
    aes(x = 2.5, y = y_max + 0.8, label = "*"),
    size = 6,
    inherit.aes = FALSE
  )

print(p_final)



cat("\n=== TABLE: SUBJECT x CONDITION ===\n")

table_wide <- df_complete %>%
  select(id, condition, change) %>%
  pivot_wider(
    names_from = condition,
    values_from = change
  ) %>%
  arrange(id)


print(table_wide)
write_xlsx(table_wide, "VAS_table_subjects_conditions.xlsx")


