# ------- 0. LOAD & CLEAN DATA --------------------------

#raw <- read_excel("our_data.xlsx", sheet = "VAS")
raw <- read_excel("6. Semester/Projekt/Databehandling/our_data.xlsx", sheet = "Presence")

# Keep only the columns we need and give them tidy names
df <- raw %>%
  select(
    id = `Participant ID.`,
    condition = Condition,
    order = Order,
    sequence = Sequence,
    presence_score = `Presence Score`
  ) %>%
  filter(!(id%in% c(4,5)))


#------ 1. find complete participants
# Participants with full data (all 4 conditions present and no NA in 'change')
complete_ids <- df %>%
  group_by(id) %>%
  pull(id)


#filter final data
df_complete <- df %>% filter(id %in% complete_ids)

cat("Participants with complete Data sets:", paste(complete_ids, collapse = ", "), "\n")
cat("N =", length(complete_ids), "\n\n")


#----------- make factors------------

df_complete <- df_complete %>%
  mutate(sequence = factor(sequence))

#----------- 2. check for normality

#extract residuals
lm_model <- lm(presence_score ~ sequence + factor(id), data = df_complete)

#get residuals
res <- residuals(lm_model)

#test for normality
norm_results <- shapiro.test(res)

print(norm_results)

#visual check
qqnorm(res)
qqline(res)



cat("\n=== SEQUENCE EFFECT ANOVA ===\n")

aov_sequence <- ezANOVA(
  data = df_complete %>% mutate(id = factor(id)),
  dv = presence_score,
  wid = id,
  within = sequence,
  type = 3,
  detailed = TRUE
)

print(aov_sequence)
