library(rmcorr)

# ------- 0. LOAD & CLEAN DATA --------------------------

raw <- read_excel("6. Semester/Projekt/Databehandling/our_data.xlsx", sheet = "Pain+Presence")

# Keep only the columns we need and give them tidy names
df <- raw %>%
  select(
    id = `Participant ID.`,
    condition = Condition,
    presence_score = `Presence Score`,
    VAS_change = `VAS-Change`
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


#-------2. Run correlation -----------

rmcorr_result <- rmcorr(
  participant = id,
  measure1 = presence_score,
  measure2 = VAS_change,
  dataset = df_complete
)


p <- ggplot(df_complete, aes(x = VAS_change, y = presence_score)) +
  
  # individual points
  geom_point(
    alpha = 0.6,
    size = 2,
    #color = "black",
    #aes(color = factor(id))   # olor by participant
    
  ) +
  
  # overall regression line
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    linewidth = 1
  ) +
  
  labs(
    y = "Presence Score",
    x = "VAS Change (negative = pain reduction)",
    title = "Relationship Between Presence and Pain Reduction"
  ) +
  
  theme_minimal(base_size = 13)

print(rmcorr_result)

plot(p)