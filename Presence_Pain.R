library(rmcorr)

# ------- 0. LOAD & CLEAN DATA --------------------------

raw <- read_excel("our_data.xlsx", sheet = "Pain+Presence")

# Keep only the columns we need and give them tidy names
df <- raw %>%
  select(
    id = `Participant ID.`,
    condition = Condition,
    presence_score = `Presence Score`,
    VAS_change = `VAS-Change`
  ) %>%
  filter(!(id%in% c(4,5,6)))


# ------- 1. LINEAR MIXED MODEL --------------------------
cat("====== VAS-Change ~ Presence (LMM) ======\n")
model <- lmer(VAS_change ~ presence_score + (1 | id), data = df)
s <- summary(model)
print(s)

# Extract values for plot annotation
beta <- fixef(model)["presence_score"]
pval <- coef(s)["presence_score", "Pr(>|t|)"]
r2 <- as.numeric(MuMIn::r.squaredGLMM(model)[,"R2m"])

# Format label
sig_stars <- ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "Not significant"))
annot_label <- sprintf("β = %.3f, p = %.3f %s\nMarginal R² = %.3f", beta, pval, sig_stars, r2)

# ------- 2. PLOT --------------------------
p <- ggplot(df, aes(x = presence_score, y = VAS_change)) +
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
    y = "VAS Change (negative = pain reduction)",
    x = "Presence Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 16),
    axis.text.y  = element_text(size = 16)
  )

plot(p)


# OLD SCRIPT BELOW IN CASE WE WANT TO FALLBACK OR NEED TO INSPECT IT
# #------ 1. find complete participants
# # Participants with full data (all 4 conditions present and no NA in 'change')
# complete_ids <- df %>%
#   group_by(id) %>%
#   summarise(n = n()) %>%
#   filter(n == 4) %>%
#   pull(id)
# 
# 
# #filter final data
# df_complete <- df %>% filter(id %in% complete_ids)
# 
# cat("Participants with complete Data sets:", paste(complete_ids, collapse = ", "), "\n")
# cat("N =", length(complete_ids), "\n\n")
# 
# 
# #-------2. Run correlation -----------
# 
# rmcorr_result <- rmcorr(
#   participant = id,
#   measure1 = presence_score,
#   measure2 = VAS_change,
#   dataset = df_complete
# )
# 
# 
# p <- ggplot(df_complete, aes(x = VAS_change, y = presence_score)) +
#   
#   # individual points
#   geom_point(
#     alpha = 0.6,
#     size = 2,
#     #color = "black",
#     #aes(color = factor(id))   # olor by participant
#     
#   ) +
#   
#   # overall regression line
#   geom_smooth(
#     method = "lm",
#     se = TRUE,
#     color = "black",
#     linewidth = 1
#   ) +
#   
#   labs(
#     y = "Presence Score",
#     x = "VAS Change (negative = pain reduction)",
#     title = "Relationship Between Presence and Pain Reduction"
#   ) +
#   
#   theme_minimal(base_size = 13)
# 
# print(rmcorr_result)
# 
# plot(p)