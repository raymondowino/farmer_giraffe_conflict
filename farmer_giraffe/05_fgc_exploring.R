library(tidyverse)

############# Methods used and their effectiveness ###################
#  Clean and categorize 'measures_other' with separate checks
dc <- dc %>%
  mutate(
    measures_other_clean = case_when(
      str_detect(tolower(measures_other), "flashlight") ~ "flashlight",
      str_detect(tolower(measures_other), "floodlight") ~ "flashlight",
      str_detect(tolower(measures_other), "torch") ~ "flashlight",
      str_detect(tolower(measures_other), "flashing") ~ "flashlight",
      
      str_detect(tolower(measures_other), "scare crow") ~ "scarecrow",
      str_detect(tolower(measures_other), "scarecrow") ~ "scarecrow",
      
      str_detect(tolower(measures_other), "vessel") ~ "propellas",
      str_detect(tolower(measures_other), "propellas") ~ "propellas",
      
      is.na(measures_other) ~ NA_character_,
      TRUE ~ "other"
    )
  )

# Check cleaned unique values
unique(dc$measures_other_clean)

# Create combined columns dataframe
dc_combined <- dc %>%
  mutate(row_id = row_number()) %>%
  mutate(
    measures_combined = measures,
    effectiveness_combined = pmap_chr(select(., starts_with("effectiveness")),
                                      ~ paste(na.omit(c(...)), collapse = ", ")),
    reasons_combined = pmap_chr(select(., starts_with("eff_resn")),
                                ~ paste(na.omit(c(...)), collapse = ", "))
  ) %>%
  select(row_id, measures_combined, effectiveness_combined, reasons_combined) %>%
  as.data.frame()

# Find max number of reasons per row
max_reasons <- max(str_count(dc_combined$reasons_combined, ",") + 1, na.rm = TRUE)

# Create column names for reasons
reason_cols <- paste0("reason_", seq_len(max_reasons))

# Split reasons_combined into multiple columns
dc_split_reasons <- dc_combined %>%
  separate(reasons_combined, into = reason_cols, sep = ",", fill = "right", extra = "drop") %>%
  mutate(across(all_of(reason_cols), str_trim))

# Find max number of effectiveness entries per row
max_effectiveness <- max(str_count(dc_split_reasons$effectiveness_combined, ",") + 1, na.rm = TRUE)

# Create column names for effectiveness
effectiveness_cols <- paste0("effectiveness_", seq_len(max_effectiveness))

# Split effectiveness_combined into multiple columns
dc_split_effectiveness <- dc_split_reasons %>%
  separate(effectiveness_combined, into = effectiveness_cols, sep = ",", fill = "right", extra = "drop") %>%
  mutate(across(all_of(effectiveness_cols), str_trim))

#  Find max number of measures per row
max_measures <- max(str_count(dc_split_effectiveness$measures_combined, ",") + 1, na.rm = TRUE)

#  Create column names for measures
measures_cols <- paste0("measure_", seq_len(max_measures))

# Split measures_combined into multiple columns
dc_split_measures <- dc_split_effectiveness %>%
  separate(measures_combined, into = measures_cols, sep = ",", fill = "right", extra = "drop") %>%
  mutate(across(all_of(measures_cols), str_trim))

# Prepare long format data frames for joining
measures_long <- dc_split_measures %>%
  pivot_longer(
    cols = starts_with("measure_"),
    names_to = "measure_num",
    values_to = "measure",
    values_drop_na = TRUE
  ) %>%
  filter(measure != "") %>%
  mutate(num = as.integer(sub("measure_", "", measure_num)))

effectiveness_long <- dc_split_measures %>%
  pivot_longer(
    cols = starts_with("effectiveness_"),
    names_to = "effectiveness_num",
    values_to = "effectiveness",
    values_drop_na = TRUE
  ) %>%
  filter(effectiveness != "") %>%
  mutate(num = as.integer(sub("effectiveness_", "", effectiveness_num)))

reasons_long <- dc_split_measures %>%
  pivot_longer(
    cols = starts_with("reason_"),
    names_to = "reason_num",
    values_to = "reason",
    values_drop_na = TRUE
  ) %>%
  filter(reason != "") %>%
  mutate(num = as.integer(sub("reason_", "", reason_num)))

# Join all by row_id and num to keep aligned
dc_long <- measures_long %>%
  inner_join(effectiveness_long, by = c("row_id", "num")) %>%
  inner_join(reasons_long, by = c("row_id", "num")) %>%
  arrange(row_id, num) %>%
  select(row_id, measure, effectiveness, reason)

#  Replace 'others' in measure with cleaned measures_other from original dc by row_id
dc_long <- dc_long %>%
  mutate(
    measure = if_else(
      measure == "others",
      dc$measures_other_clean[row_id],  # replace with cleaned 'measures_other'
      measure
    )
  )

# Plot the data
ggplot(dc_long, aes(x = measure, fill = effectiveness)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2", na.value = "grey80") +
  labs(title = "Effectiveness of Measures",
       x = "Measure",
       y = "Count",
       fill = "Effectiveness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Export 
#write.csv(dc_long, "raw_data/dc_long.csv", row.names = FALSE)


### edit reasons
dc_long <- dc_long %>%
  mutate(
    reason_clean = case_when(
      
      # 1. Prevent Entry / Barriers
      str_detect(tolower(reason), "prevent") ~ "prevent entry",
      str_detect(tolower(reason), "bar") ~ "prevent entry",
      str_detect(tolower(reason), "block") ~ "prevent entry",
      str_detect(tolower(reason), "reduce.*invasion") ~ "prevent entry",
      str_detect(tolower(reason), "not enter") ~ "prevent entry",
      str_detect(tolower(reason), "avoid entry") ~ "prevent entry",
      
      # 2. Chase or Scare Away
      str_detect(tolower(reason), "chase") ~ "chase away",
      str_detect(tolower(reason), "scare") ~ "chase away",
      str_detect(tolower(reason), "run") ~ "chase away",
      str_detect(tolower(reason), "spray") ~ "chase away",
      
      # 3. Indicate Human Presence (noise, light, etc.)
      str_detect(tolower(reason), "indicate") ~ "human presence",
      str_detect(tolower(reason), "presence") ~ "human presence",
      str_detect(tolower(reason), "simulate") ~ "human presence",
      str_detect(tolower(reason), "think someone") ~ "human presence",
      str_detect(tolower(reason), "alert") ~ "human presence",
      str_detect(tolower(reason), "noise") ~ "human presence",
      str_detect(tolower(reason), "show.*human") ~ "human presence",
      str_detect(tolower(reason), "think.*human") ~ "human presence",
      
      # 4. Fear of Fire/Light
      str_detect(tolower(reason), "fire") ~ "fear of fire/light",
      str_detect(tolower(reason), "light") ~ "fear of fire/light",
      str_detect(tolower(reason), "torch") ~ "fear of fire/light",
      str_detect(tolower(reason), "flashlight") ~ "fear of fire/light",
      
      # 5. Giraffes Acclimate / Get Used To It
      str_detect(tolower(reason), "get used to") ~ "get used to it",
      str_detect(tolower(reason), "used to it") ~ "get used to it",
      str_detect(tolower(reason), "used to this") ~ "get used to it",
      str_detect(tolower(reason), "they are now used to") ~ "get used to it",
      str_detect(tolower(reason), "no longer afraid") ~ "get used to it",
      str_detect(tolower(reason), "not afraid of fire") ~ "get used to it",
      str_detect(tolower(reason), "not scared") ~ "get used to it",
      str_detect(tolower(reason), "first few days") ~ "get used to it",
      str_detect(tolower(reason), "effective but") ~ "get used to it",
      str_detect(tolower(reason), "only helps initially") ~ "get used to it",
      str_detect(tolower(reason), "not.*afraid") ~ "human presence",
      
      # 6. Giraffes Still Come / Never Effective
      str_detect(tolower(reason), "still come") ~ "still ineffective",
      str_detect(tolower(reason), "still invade") ~ "still ineffective",
      str_detect(tolower(reason), "return") ~ "still ineffective",
      str_detect(tolower(reason), "keep coming") ~ "still ineffective",
      str_detect(tolower(reason), "don't care") ~ "still ineffective",
      str_detect(tolower(reason), "doesn't help") ~ "still ineffective",
      str_detect(tolower(reason), "minimal") ~ "still ineffective",
      str_detect(tolower(reason), "not really") ~ "still ineffective",
      str_detect(tolower(reason), "very little") ~ "still ineffective",
      str_detect(tolower(reason), "no longer work") ~ "still ineffective",
      str_detect(tolower(reason), "not.*effective") ~ "still ineffective",
      
      # 7. Break or Jump Fence
      str_detect(tolower(reason), "break") ~ "break/jump fence",
      str_detect(tolower(reason), "jump") ~ "break/jump fence",
      str_detect(tolower(reason), "over fence") ~ "break/jump fence",
      str_detect(tolower(reason), "through fence") ~ "break/jump fence",
      str_detect(tolower(reason), "breakthrough") ~ "break/jump fence",
      
      # 8. Fence Quality (weak, porous, expensive, etc.)
      str_detect(tolower(reason), "porous") ~ "fence issues",
      str_detect(tolower(reason), "weak") ~ "fence issues",
      str_detect(tolower(reason), "temporary") ~ "fence issues",
      str_detect(tolower(reason), "expensive") ~ "fence issues",
      str_detect(tolower(reason), "not strong") ~ "fence issues",
      str_detect(tolower(reason), "low fence") ~ "fence issues",
      str_detect(tolower(reason), "short fence") ~ "fence issues",
      
      # 9. Human Limitation (sleep, tired, danger)
      str_detect(tolower(reason), "sleep") ~ "human limitation",
      str_detect(tolower(reason), "tired") ~ "human limitation",
      str_detect(tolower(reason), "can't stay") ~ "human limitation",
      str_detect(tolower(reason), "danger") ~ "human limitation",
      str_detect(tolower(reason), "not always around") ~ "human limitation",
      str_detect(tolower(reason), "get lazy") ~ "human limitation",
      
      # 10. Wildlife Interference (e.g. hippos)
      str_detect(tolower(reason), "hippo") ~ "Safety/hazards",
      str_detect(tolower(reason), "dangarous.*farm") ~ "Safety/hazards",
      
      # 11. Unclear or Unsure
      str_detect(tolower(reason), "not sure") ~ "unsure",
      str_detect(tolower(reason), "no idea") ~ "unsure",
      str_detect(tolower(reason), "i am not sure") ~ "unsure",
      str_detect(tolower(reason), "n/a") ~ "unsure",
      str_detect(tolower(reason), "^\\d+$") ~ "unsure",
      
      # Default
      TRUE ~ "other"
    )
  )


# Create a lookup table for all 72 reasons (paste from your list)
reason_map <- tribble(
  ~reason,                                                             ~new_category,
 "In addition to human this can help",                               "Effective",
  "It just help the guard see them but nothing more",                "Human presence",
  "Giraffe destroys the fence",                                      "fence issues",
  "Still invade",                                                    "Effectiveness",
  "Giraffe don't fear live guard",                                  "Giraffe not afraid",
  "They thing it is trap",                                           "Effectiveness",
  "Still invade the farm",                                           "Effectiveness",
  "Giraffe don't often come",                                        "Effectiveness",
  "It's reduce by small chance",                                     "Effectiveness",
  "Act as a trap",                                                  "Effectiveness",
  "They can not always be around",                                  "Effectiveness",
  "They come regardless",                                           "Effectiveness",
  "It's local fence that giraffe can easily pass",                 "fence issues",
  "Giraffe can easily pass over the fence",                        "fence issues",
  "Giraffe pass over the fence",                                   "fence issues",
  "Pass over the fence",                                           "fence issues",
  "Assist in chasing away the Giraffes",                           "Effective",
  "Destroyed easily by the girraffe",                              "fence issues",
  "Not that much effective",                                       "Effectiveness",
  "Some times not effective due to fear",                          "Effectiveness",
  "Not that much effective for now",                              "Effectiveness",
  "Only when installed properly",                                 "Effectiveness",
  "Show that there is somebody around",                           "Human presence",
  "They are afraid",                                              "Effectiveness",
  "Tall better fence",                                            "fence issues",
  "Install better fence",                                         "fence issues",
  "Tall wire fence",                                             "fence issues",
  "Fence is too short  than giraffe",                            "fence issues",
  "Only effective when around throughout",                       "Effectiveness",
  "They think there is someone at the farm",                     "Human presence",
  "Dogs are afraid",                                              "Effective",
  "Dogs are afraid of giraffe",                                  "Giraffe not afraid",
  "They are not afraid",                                          "Giraffe not afraid",
  "They go but come back",                                       "Effectiveness",
  "They try but the farm is big with multiple entry points",     "Effectiveness",
  "Only female and young are afraid",                            "Demographic response",
  "Female and young",                                            "Demographic response",
  "They fences",                                                "fence issues",
  "They produce sound when wind blows making the think there are human", "Human presence",
  "They don't like like at night",                              "Effectiveness",
  "This helps with new individuals and maybe females males do not care", "Demographic response",
  "Works for females and young but not adult males",            "Demographic response",
  "Some are afraid but not all",                                "Effectiveness",
  "They think there are human around",                          "Human presence",
  "They think there is someone on the farm",                    "Human presence",
  "They even pass close to it",                                 "Effectiveness",
  "giraffe are afraid of human",                                "Effectiveness",
  "They are used it",                                           "Acclimation",
  "They help in visibility",                                    "Effective",
  "They are used",                                              "Acclimation",
  "it is a live fence",                                         "fence issues",
  "Giraffe are not afraid",                                     "Giraffe not afraid",
  "They avoid places with sheets tied for wind to blow",       "Effectiveness",
  "Helps only when awake present",                              "Effectiveness",
  "It will cost me a lot to fance",                            "Cost issues",
  "Because I can get effective by the coldness",               "Effectiveness",
  "Because  will west more time",                               "Cost issues",
  "It will cost my money's alot",                               "Cost issues",
  "By spending more money buying it",                           "Cost issues",
  "It reduce the girrafe to enter the farm",                    "Effectiveness",
  "Giraffe destroyed",                                          "Fence issues",
  "Buy applying new model",                                     "Effectiveness",
  "Yes",                                                       "Effectiveness",
  "It reduces the girrafe to enter the farm",                    "prevent entry",
  "It reduces the girrafe to enter the farm not deally",         "prevent entry",
  "Because of gizaa",                                           "Effectiveness" # gizaa = darkness; presumably means low visibility helps
)

# Join with your data frame (replace dc_long with your actual data frame name)
dc_long <- dc_long %>%
  left_join(reason_map, by = "reason") %>%
  mutate(
    reason_clean = coalesce(new_category, reason_clean)
  ) %>%
  select(-new_category)

## plot
ggplot(dc_long, aes(x = fct_infreq(reason_clean), fill = reason_clean)) +
geom_bar()  +
  labs(
    title = "Categorized Reasons for Protection Measures",
    x = "Reason Category",
    y = "Count",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~measure)

############## Methods people are willing to try  (Q2i) ###############

# Clean and categorize 'new_mtd'
dc <- dc %>%
  mutate(
    new_mtd_clean = case_when(
      # Fence and wire
      str_detect(tolower(new_mtd), "fence") ~ "fence/wire",
      str_detect(tolower(new_mtd), "wire") ~ "fence/wire",
      str_detect(tolower(new_mtd), "mesh") ~ "fence/wire",
      
      # Lighting
      str_detect(tolower(new_mtd), "light") ~ "light",
      str_detect(tolower(new_mtd), "flashlight") ~ "light",
      str_detect(tolower(new_mtd), "floodlight") ~ "light",
      str_detect(tolower(new_mtd), "flashing") ~ "light",
      
      # Live guards and security
      str_detect(tolower(new_mtd), "guard") ~ "live guard",
      str_detect(tolower(new_mtd), "watchmen") ~ "live guard",
      str_detect(tolower(new_mtd), "security") ~ "live guard",
      
      # Pitfall and trench
      str_detect(tolower(new_mtd), "pitfall") ~ "pitfall/trench",
      str_detect(tolower(new_mtd), "trench") ~ "pitfall/trench",
      
      # Propellas and DIY
      str_detect(tolower(new_mtd), "propella") ~ "propella",
      str_detect(tolower(new_mtd), "diy") ~ "propella",
      
      # Gun scare and gov support
      str_detect(tolower(new_mtd), "gun") ~ "gun scare",
      str_detect(tolower(new_mtd), "kws") ~ "gun scare",
      str_detect(tolower(new_mtd), "government") ~ "gun scare",
      
      # Unsure responses
      str_detect(tolower(new_mtd), "i don't know") ~ "unsure",
      str_detect(tolower(new_mtd), "not sure") ~ "unsure",
      str_detect(tolower(new_mtd), "no idea") ~ "unsure",
      str_detect(tolower(new_mtd), "none") ~ "unsure",
      str_detect(tolower(new_mtd), "any") ~ "unsure",
      new_mtd %in% c("5", "2") ~ "unsure",
      
      # NA remains NA
      is.na(new_mtd) ~ NA_character_,
      
      # Catch-all
      TRUE ~ "other"
    )
  )

# View unique cleaned values
unique(dc$new_mtd_clean)


# Basic count plot of new_mtd_clean categories
ggplot(dc, aes(x = fct_infreq(new_mtd_clean), fill = new_mtd_clean)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Count of Cleaned Protection Measures",
    x = "Measure Category",
    y = "Count",
    fill = "Measure Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Response to how they can improve method  (Q2g) ####

# Clean and categorize 'impro_mtd1'
dc <- dc %>%
  filter(!is.na(impro_mtd_clean))%>%
  mutate(
    impro_mtd_clean = case_when(
      # Fence and wire
      str_detect(tolower(impro_mtd1), "fence") ~ "fence/wire",
      str_detect(tolower(impro_mtd1), "wire") ~ "fence/wire",
      str_detect(tolower(impro_mtd1), "mesh") ~ "fence/wire",
      str_detect(tolower(impro_mtd1), "brick wall") ~ "fence/wire",
      str_detect(tolower(impro_mtd1), "perimeter wall") ~ "fence/wire",
      
      # Lighting
      str_detect(tolower(impro_mtd1), "light") ~ "light",
      str_detect(tolower(impro_mtd1), "flashlight") ~ "light",
      str_detect(tolower(impro_mtd1), "floodlight") ~ "light",
      str_detect(tolower(impro_mtd1), "torch") ~ "light",
      
      # Live guards and security
      str_detect(tolower(impro_mtd1), "guard") ~ "live guard",
      str_detect(tolower(impro_mtd1), "watchmen") ~ "live guard",
      str_detect(tolower(impro_mtd1), "manpower") ~ "live guard",
      str_detect(tolower(impro_mtd1), "security") ~ "live guard",
      str_detect(tolower(impro_mtd1), "lifeguard") ~ "live guard",
      
      # Government or financial support
      str_detect(tolower(impro_mtd1), "government") ~ "gov/financial support",
      str_detect(tolower(impro_mtd1), "financial") ~ "gov/financial support",
      str_detect(tolower(impro_mtd1), "support") ~ "gov/financial support",
      
      # Unclear or numeric responses
      str_detect(tolower(impro_mtd1), "don't know") ~ "unsure",
      str_detect(tolower(impro_mtd1), "not sure") ~ "unsure",
      str_detect(tolower(impro_mtd1), "no idea") ~ "unsure",
      impro_mtd1 %in% c("5", "6", "10", "N/A", "NA") ~ "unsure",
      str_detect(tolower(impro_mtd1), "^\\d+$") ~ "unsure",
      
      
    )
  )


okabe_ito <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

ggplot(dc_clean, aes(x = fct_infreq(impro_mtd_clean), fill = impro_mtd_clean)) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.2,
    size = 3,
    fontface = "bold"   # bold counts on bars
  ) +
  scale_fill_manual(values = okabe_ito, guide = "none") +
  labs(x = "", y = " Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),      # bold title if used
    axis.title.y = element_text(face = "bold"),                  # bold y-axis title
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # bold x-axis text
    axis.text.y = element_text(face = "bold"),                   # bold y-axis text
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)  # bold box
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))




dc_long <- read.csv("raw_data/dc_long.csv")


# Step 0: Clean measure labels
dc_long <- dc_long %>%
  mutate(measure = recode(measure,
                          "fence" = "Fence",
                          "fire" = "Fire",
                          "flashlight" = "Flashlight",
                          "guard" = "Human guard",
                          "guard_dog" = "Guard dog",
                          "pitfall" = "Trench",
                          "propellas" = "Sound devices",
                          "scarecrow" = "Scarecrow"))

# Step 1: Recode effectiveness into 3 categories
dc_long <- dc_long %>%
  mutate(effectiveness_clean = tolower(effectiveness),
         effectiveness_grouped = case_when(
           effectiveness_clean %in% c("extremely", "highly") ~ "Effective",
           effectiveness_clean %in% c("moderately") ~ "Neutral",
           TRUE ~ "Ineffective"
         ))

# Step 2: Calculate counts and percentages per measure
plot_data <- dc_long %>%
  group_by(measure, effectiveness_grouped) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(measure) %>%
  mutate(total = sum(count),
         percent = count / total * 100) %>%
  ungroup() %>%
  mutate(
    percent_plot = case_when(
      effectiveness_grouped == "Ineffective" ~ -percent,
      TRUE ~ percent
    ),
    label = paste0("n=", count)
  )

# Step 3: Order measure factor by total count descending and reverse for top-to-bottom
measure_order <- plot_data %>%
  distinct(measure, total) %>%
  arrange(desc(total)) %>%  # largest first
  pull(measure)

# Reverse order so largest is at the top when coord_flip is applied
plot_data$measure <- factor(plot_data$measure, levels = rev(measure_order))

# Step 4: Set factor order for effectiveness (Negative at bottom)
plot_data$effectiveness_grouped <- factor(plot_data$effectiveness_grouped,
                                          levels = c("Ineffective", "Neutral", "Effective"))

# Step 5: Plot
ggplot(plot_data, aes(x = measure, y = percent_plot, fill = effectiveness_grouped)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.99) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            size = 3, color = "black") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Ineffective" = "#E69F00",  # Orange
      "Neutral" = "#999999",   # Gray
      "Effective" = "#56B4E9"   # Blue-Green
    ),
    guide = guide_legend(title = NULL)
  ) +
  scale_y_continuous(labels = abs, expand = expansion(mult = c(0.01, 0.01))) +
  labs(
    title = "",
    x = NULL,
    y = "Frequency (percentage)"
  ) +
  theme_bw(base_size = 12, base_family = "") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 3),
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(r = 20) 
  )
###################
#### Run from here
library(tidyverse)

dc_long <- read.csv("raw_data/dc_long.csv")

# Clean data
dc_long_clean <- dc_long %>%
  filter(
    !sum_reason %in% c("no reason", "Unsure"),
    !measure %in% c("pitfall", "scarecrow")
  )

# Recode effectiveness
dc_long_clean <- dc_long_clean %>%
  mutate(
    effectiveness_clean = tolower(effectiveness),
    effectiveness_grouped = case_when(
      effectiveness_clean %in% c("extremely", "highly") ~ "Effective",
      effectiveness_clean == "moderately" ~ "Neutral",
      TRUE ~ "Ineffective"
    )
  )

# Create complete grid to ensure all categories appear in all facets
all_measures <- unique(dc_long_clean$measure)
all_reasons <- unique(dc_long_clean$sum_reason)
all_effectiveness <- c("Ineffective", "Neutral", "Effective")

plot_data <- dc_long_clean %>%
  count(measure, sum_reason, effectiveness_grouped) %>%
  complete(measure = all_measures,
           sum_reason = all_reasons,
           effectiveness_grouped = all_effectiveness,
           fill = list(n = 0)) %>%
  group_by(measure, sum_reason) %>%
  mutate(
    total = sum(n),
    percent = ifelse(total == 0, 0, n / total * 100),
    percent_plot = ifelse(effectiveness_grouped == "Ineffective", -percent, percent),
    label = ifelse(n == 0, "", paste0("n=", n))
  ) %>%
  ungroup()

# Order measures by total responses (largest first)
measure_order <- plot_data %>%
  group_by(measure) %>%
  summarise(total_responses = sum(n), .groups = "drop") %>%
  arrange(desc(total_responses)) %>%
  pull(measure)

plot_data$measure <- factor(plot_data$measure, levels = measure_order)
plot_data$sum_reason <- factor(plot_data$sum_reason, levels = rev(sort(unique(plot_data$sum_reason))))
plot_data$effectiveness_grouped <- factor(plot_data$effectiveness_grouped,
                                          levels = c("Ineffective", "Neutral", "Effective"))

# Named vector for descriptive facet labels
measure_labels <- c(
  "fence" = "Fence",
  "fire" = "Fire",
  "flashlight" = "Flashlight",
  "guard" = "Human Guard",
  "guard_dog" = "Guard dog",
  "propellas" = "Sound device"
)

# Plot
ggplot(plot_data, aes(x = sum_reason, y = percent_plot, fill = effectiveness_grouped)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.99) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            size = 3, color = "black") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Ineffective" = "#E69F00",
      "Neutral" = "#999999",
      "Effective" = "#56B4E9"
    ),
    guide = guide_legend(title = NULL)
  ) +
  scale_y_continuous(labels = abs, expand = expansion(mult = c(0.01, 0.01))) +
  labs(
    title = "",
    x = "",
    y = " "
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  ) +
  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~measure, scales = "free_y", labeller = labeller(measure = measure_labels)) +clean_up
#####
# plot_data <- plot_data %>%
#   mutate(sum_reason = recode(as.character(sum_reason), 
#                              "Psychological" = "Learning")) %>%
#   mutate(sum_reason = factor(sum_reason, levels = rev(sort(unique(sum_reason)))))
# 
# ggplot(plot_data, aes(x = sum_reason, y = percent_plot, fill = effectiveness_grouped)) +
#   geom_col(position = position_stack(reverse = TRUE), width = 0.99) +
#   geom_text(aes(label = label),
#             position = position_stack(vjust = 0.5, reverse = TRUE),
#             size = 3, color = "black") +
#   coord_flip() +
#   scale_fill_manual(
#     values = c(
#       "Ineffective" = "#E69F00",
#       "Neutral" = "#999999",
#       "Effective" = "#56B4E9"
#     ),
#     guide = guide_legend(title = NULL)
#   ) +
#   scale_y_continuous(labels = abs, expand = expansion(mult = c(0.01, 0.01))) +
#   labs(
#     title = "",
#     x = "",
#     y = " "
#   ) +
#   theme_bw(base_size = 12) +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "bottom",
#     panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
#   ) +
#   geom_hline(yintercept = 0, color = "black") +
#   facet_wrap(~measure, scales = "free_y", labeller = labeller(measure = measure_labels)) + clean_up
# Define filename
output_file <- "summary_plots/dc_effectiveness_plot.png"

# Save the plot
ggsave(
  filename = output_file,
  plot = last_plot(),  # the last ggplot object you created
  width = 16,          # increase width for multiple facets
  height = 10,         # increase height for readability
  units = "in",
  dpi = 300            # high resolution
)

#### Methods they are willing to try

ggplot(
  dc %>%
    mutate(
      new_mtd_group = case_when(
        str_detect(new_mtd_clean, "wire|mesh|barbed|steel") ~ "Fence/wire",
        str_detect(new_mtd_clean, "electric|electronic") ~ "Fence/wire",
        str_detect(new_mtd_clean, "wall|block|brick|concrete") ~ "Fence/wire",
        str_detect(new_mtd_clean, "guard|watchmen|security") &
          !str_detect(new_mtd_clean, "light") ~ "Human guards",
        str_detect(new_mtd_clean, "light|floodlight|flash") ~ "Floodlights",
        str_detect(new_mtd_clean, "government|kws|conservation") ~ "Government support",
        str_detect(new_mtd_clean, "trench|pitfall|tree") ~ "Trench",
        str_detect(new_mtd_clean, "gun") ~ "Government support",
        is.na(new_mtd_clean) ~ "Don't know",
        TRUE ~ "Unsure"
      )
    ) %>%
    filter(new_mtd_group != "Don't know") %>%
    count(new_mtd_group) %>%
    mutate(
      percent = n / sum(n) * 100,
      label = paste0("n = ", n)
    ) %>%
    arrange(percent),
  aes(x = reorder(new_mtd_group, percent), y = percent)
) +
  geom_bar(stat = "identity", fill = "#1f4260", width = 0.99) +
  geom_text(aes(label = label),
            hjust = 1.1,  # push label inside the bar, right-aligned
            size = 3,
            color = "white") +  # white text for better contrast inside bars
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  labs(
    x = NULL,
    y = "Frequency (percentage)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.text = element_text(face = "bold", size = 12, color = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(r = 15, l = 10, t = 10, b = 10)
  )
  
#### in table

dc %>%
  mutate(
    new_mtd_group = case_when(
      str_detect(new_mtd_clean, "wire|mesh|barbed|steel") ~ "Fence/wire",
      str_detect(new_mtd_clean, "electric|electronic") ~ "Fence/wire",
      str_detect(new_mtd_clean, "wall|block|brick|concrete") ~ "Fence/wire",
      str_detect(new_mtd_clean, "guard|watchmen|security") &
        !str_detect(new_mtd_clean, "light") ~ "Human guards",
      str_detect(new_mtd_clean, "light|floodlight|flash") ~ "Floodlights",
      str_detect(new_mtd_clean, "government|kws|conservation") ~ "Government support",
      str_detect(new_mtd_clean, "trench|pitfall|tree") ~ "Trench",
      str_detect(new_mtd_clean, "gun") ~ "Government support",
      is.na(new_mtd_clean) ~ "Don't know",
      TRUE ~ "Unsure"
    )
  ) %>%
  filter(new_mtd_group != "Don't know") %>%  # Exclude NAs
  count(new_mtd_group) %>%
  mutate(
    Percent = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(desc(n)) %>%
  print(n = Inf)
  