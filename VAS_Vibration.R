library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ------- 0. LOAD & CLEAN DATA --------------------------

raw <- read_excel("our_data.xlsx", sheet = "Pain+Vibration")
#raw <- read_excel("6. Semester/Projekt/Databehandling/our_data.xlsx", sheet = "Pain+Vibration")

df <- raw %>% select(
  id = 'Participant',
  condition = 'Condition',
  procedure_vib = 'Left vibration',
  non_procedure_vib = 'Right vibration',
  hand_vib = 'Hand vibration',
  vas_change = 'VAS-Change',
  z_vas = 'Z-VAS')

#Plotting vas-change againts procedure arm vibration - ALL CONDITIONS
scatter_all <- ggplot(df, aes(procedure_vib, vas_change))
scatter_all + geom_point() + geom_smooth(method = "lm") + ggtitle("Procedure Arm vibration at stimulation vs VAS change (ALL)")

#Plotting vas-change againts procedure arm vibration - ARM AND ALL
df_arm <- df %>% filter((condition %in% c("AllVibration", "ArmVibration")))
scatter_arm <- ggplot(df_arm, aes(procedure_vib, vas_change))
scatter_arm + geom_point() + geom_smooth(method = "lm") + ggtitle("Procedure Arm vibration at stimulation vs VAS change (ArmVibration + AllVibration conditions only)")

#Plotting vas-change againts procedure arm vibration - HAND AND ALL
df_hand <- df %>% filter((condition %in% c("AllVibration", "HandVibration")))
scatter_hand <- ggplot(df_hand, aes(hand_vib, vas_change))
scatter_hand + geom_point() + geom_smooth(method = "lm")  + ggtitle("Hand Arm vibration at stimulation vs VAS change (HandVibration + AllVibration conditions only )")

#Calculate correlation coefficients
cor(df$procedure_vib, df$vas_change, use="complete.obs", method="kendall")
cor(df_arm$procedure_vib, df_arm$vas_change, use="complete.obs", method="kendall")
cor(df_hand$hand_vib, df_hand$vas_change, use="complete.obs", method="kendall")

###############################  Z-VAS  #######################################
#Plotting vas-change againts procedure arm vibration - ALL CONDITIONS
scatter_all_Z <- ggplot(df, aes(procedure_vib, z_vas))
scatter_all_Z + geom_point() + geom_smooth(method = "lm") + ggtitle("Procedure Arm vibration at stimulation vs Z-VAS (ALL conditions)")

#Plotting vas-change againts procedure arm vibration - ARM AND ALL
scatter_arm_Z <- ggplot(df_arm, aes(procedure_vib, z_vas))
scatter_arm_Z + geom_point() + geom_smooth(method = "lm") + ggtitle("Procedure Arm vibration at stimulation vs Z-VAS (ArmVibration + AllVibration conditions only)")

#Plotting vas-change againts procedure arm vibration - HAND AND ALL
scatter_hand_Z <- ggplot(df_hand, aes(hand_vib, z_vas))
scatter_hand_Z + geom_point() + geom_smooth(method = "lm") + ggtitle("Hand Arm vibration at stimulation vs Z-VAS (HandVibration + AllVibration conditions only )")

#Calculate correlation coefficients
cor(df$procedure_vib, df$z_vas, use="complete.obs", method="kendall")
cor(df_arm$procedure_vib, df_arm$z_vas, use="complete.obs", method="kendall")
cor(df_hand$hand_vib, df_hand$z_vas, use="complete.obs", method="kendall")
