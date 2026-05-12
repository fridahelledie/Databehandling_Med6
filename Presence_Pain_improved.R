library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)

        
# ------- 0. LOAD & CLEAN DATA --------------------------

raw <- read_excel("6. Semester/Projekt/Databehandling/our_data.xlsx", sheet = "Pain+Presence")

# Keep only the columns we need and give them tidy names
data <- raw %>%
  select(
    id = `Participant ID.`,
    condition = Condition,
    presence = `Presence Score`,
    VAS_change = `VAS-Change`
  ) %>%
  filter(!(id%in% c(4,5)))


#------ 1. find complete participants
# Participants with full data (all 4 conditions present and no NA in 'change')
complete_ids <- data %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  filter(n == 4) %>%
  pull(id)


#filter final data
data <- data %>% filter(id %in% complete_ids)

cat("Participants with complete Data sets:", paste(complete_ids, collapse = ", "), "\n")
cat("N =", length(complete_ids), "\n\n")



#------------convert condition into two columns--------

data <- data %>%
  mutate(
    vib_arm = ifelse(condition %in% c("Arm-vibrations", "Arm+hand-vibrations"), 1, 0),
    vib_hand = ifelse(condition %in% c("Hand-vibration", "Arm+hand-vibrations"), 1, 0)
  )

#-----------create a within-subject centeret presence variable------

data <- data %>%
  group_by(id) %>%
  mutate(presence_ws = presence - mean(presence, na.rm = TRUE)) %>%
  ungroup()


#------Use linear Mixed Model----
model <- lmer(VAS_change ~ vib_arm * vib_hand + presence_ws + (1 | id), data = data)

cat("i should print this\n")

print(summary(model))
