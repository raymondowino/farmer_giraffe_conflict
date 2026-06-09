# Libraries required
library(tidyverse)      # Data wrangling and visualization
library(sjPlot)         # Visualize random effects in models
library(ggdist)         # Visualize distributions (e.g., halfeye plots)
library(gghalves)       # Half-violin plots with jitter

# Data 
farm <- read.csv("data/farm_experiment.csv")

#  Data preparation
farm <- farm %>%
  # Fix typo in Experiment labels (if needed)
  mutate(Experiment = gsub("S-Treatement", "S-Treatment", Experiment)) %>%
  
  # Create Usage Rate: proportion of days giraffes were deterred
  mutate(Usage_rate = Days_worked / Days.operation) %>%
  
  # Classify Device type
  mutate(Device = case_when(
    grepl("^L-", Experiment) ~ "Light (Floodlight)",
    grepl("^S-", Experiment) ~ "Sound (Predator Call)",
    TRUE ~ "Unknown"
  )) %>%
  
  # Classify Treatment vs Control
  mutate(Treatment_group = case_when(
    grepl("Treatment", Experiment) ~ "Treatment",
    grepl("Control", Experiment) ~ "Control",
    TRUE ~ "Unknown"
  ))

# Wilcoxon test - Treatment vs Control
wilcox_result <- wilcox.test(Usage_rate ~ Treatment_group, data = farm)

# Print the test result
print(wilcox_result)

# Compare Light vs Sound within treatment group only
treatments_only <- farm %>% filter(Treatment_group == "Treatment")
light_vs_sound <- wilcox.test(Usage_rate ~ Device, data = treatments_only)

# Print the test result
print(light_vs_sound)

# Visualize usage rate by group (Treatment vs Control)
ggplot(farm, aes(x = Experiment, y = Usage_rate, fill = Treatment_group)) +
  geom_boxplot(width = 0.6, outlier.shape = 21, alpha = 0.8) +
  labs(
    title = "",
    x = "",
    y = "Deterrence success rate",
    fill = "Group"
  ) + theme_bw(base_size = 12) + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5), 
    axis.text = element_text(face = "bold", size = 14, color = "black"), 
    axis.title = element_text(size = 14, face = "bold")
  ) + theme(legend.position = "none")
  
# Plot - Light vs Sound (treatments only)
ggplot(treatments_only, aes(x = Device, y = Usage_rate, fill = Device)) +
  geom_boxplot(width = 0.6, outlier.shape = 21, alpha = 0.8) +
  labs(
    title = "",
    x = "",
    y = "Success rate"
  ) + 
  theme_bw(base_size = 12) + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5), 
    axis.text = element_text(face = "bold", size = 14, color = "black"), 
    axis.title = element_text(size = 14, face = "bold")
  )+ theme(legend.position = "none")
  