# Farmer/Giraffe conflict (FGC)
# Author: Jessie Golding, Raymond Owino
# Date: 09/17/2025

# Primary File

################################################################################
## 01. Setup

# Load and run package loading function
source("./01_fgc_load_packages.R")

################################################################################
## 02. Format and clean data

# Load data
d <-read_csv("./raw_data/survey_responses.csv")

# Load function to clean data
source("./02_fgc_data_load_clean.R")

# Pull in coding from dc_long csv
#dc_long <-read_csv("./raw_data/dc_long.csv")

# Run function to clean data
fgc_load_clean_data(d)

################################################################################
## 04. Filter data to address survey goals 

# Load function to filter data
source("./03_fgc_filter_data.R")

# Run filter function for each question

# Question 1a - Is invasion by giraffes occurring?
# Select columns of row id, village, invasion, and defense measures
fgc_filter_data(dc, "1a", c("row_id", "village", "invasion", "measures_fence", "measures_guard",
                            "measures_fire", "measures_propellas", "measures_guard_dog",
                            "measures_pitfall"))

# Question 1b - When is invasion by giraffes occurring?
# Select columns of row id, village, invasion frequency, crop types, and defense measures
fgc_filter_data(dc, "1b", c("row_id", "village", "invasion_frq","crop_mangoes", 
                            "crop_lemons", "crop_bananas", "crop_vegetable", 
                            "crop_melon", "measures_fence", "measures_guard",
                            "measures_fire", "measures_propellas", "measures_guard_dog",
                            "measures_pitfall"))

# Question 1c - What is the relationship of invasion and crop type?
fgc_filter_data(dc, "1c", c("row_id", "village", "invasion","crop_mangoes", 
                            "crop_lemons", "crop_bananas", "crop_vegetable", 
                            "crop_melon"))
q1c <- dc_q_1c %>%
  filter(invasion == "no")%>%
  rowwise()%>%
  mutate(sum_crops= sum(c_across(starts_with("crop_")), na.rm = TRUE))#%>%
  #filter(sum_crops == 1)%>%
  #group_by(sum_crops)%>%
  #count(sum_crops)
  #count(crop_mangoes)
  #count(crop_lemons)
  #count(crop_bananas)
  #count(crop_vegetable)
  #count(crop_melon)

# Question 2b - How interested are farmers in trying something new and why/what
# motivates them?
fgc_filter_data(dc, "2b", c("row_id", "try_new", "trial_resn_neighbour_uses",
                            "trial_resn_kws_use","trial_resn_available",
                            "trial_resn_easy_use","trial_resn_cheap"))

# Question 2c - What measures are farmers willing to try and why?
fgc_filter_data(dc, "2c", c("row_id", "try_new", "new_mtd_clean",
                            "trial_resn_neighbour_uses",
                            "trial_resn_kws_use","trial_resn_available",
                            "trial_resn_easy_use","trial_resn_cheap"))

# Question 2d - Why do famers think their current methods are effective (or ineffective)?
dc_long <-read_csv("./raw_data/dc_long.csv")


# Question 3 - Does what farmers are currently using impact what they are willing to try?
dc_q_3 <-dc %>%
  mutate(row_id = survey_id)%>%
  left_join(.,dc_long, by = "row_id")%>%
  select(row_id, try_new, measure, effectiveness)%>%
  mutate(measure = ifelse(is.na(measure), "none", measure))%>%
  mutate(effectiveness = ifelse(is.na(effectiveness), "not", effectiveness))%>%
  distinct()

# Additional question - what countermeasures and crops are being grown where there is no
# invasion?

test <-fgc_filter_data(dc, "4", c("row_id", "village", "invasion", "crop_mangoes", 
                           "crop_lemons", "crop_bananas", "crop_vegetable", 
                           "crop_melon", "measures_fence", "measures_guard",
                           "measures_fire", "measures_propellas", "measures_guard_dog",
                           "measures_pitfall"))

test2 <-test %>%
  filter(invasion == "no")
################################################################################
## 05. Create plots based on plot prototype files

# Question 1a - Is invasion by giraffes occurring?

# Formatting code from Raymond recieved on 3/2/25
clean_up <- 
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

# Added formatting code from Raymond on 3/2/25 to the code below. Tried to pick a consistent 
# color scheme. 

p1 <-dc_q_1a %>%
  mutate(count_yes = ifelse(invasion =="yes",1,0))%>%
  mutate(proportion_all = sum(count_yes)/nrow(.))%>%
  group_by(village)%>%
  mutate(count_village=n())%>%
  group_by(village)%>%
  mutate(proportion = sum(count_yes)/count_village)%>%
  ggplot(., aes(x=village, y=proportion, fill = invasion, label = count_village)) +
  geom_bar(position="fill", stat="identity")+
  #geom_text(hjust = -0.75)+
  labs(x="Village", y = "Proportion of respondents")+
  scale_fill_manual("Giraffe Invasion", values = c("no" = "gray92", "yes" = "#B0A5F3"))+
  scale_x_discrete(labels=c("shabah" = "Shabah", "sankuri" = "Sankuri",
                            "raya" = "Raya", "korakora" = "Korakora",
                            "jarirot" = "Jarirot", "bula_salama"= "Bula Salama",
                            "bula_gem" = "Bula Gem", "bula_bakari"="Bula Bakari",
                            "bula_adc" = "Bula Adc", "bour_algy"="Bour Algy",
                            "awarot"="Awarot"))+
  coord_flip()+
  #theme_bw()
  theme_bw(base_size = 12, base_family = "") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    plot.margin = margin(r = 20) 
  )


p2 <-dc_q_1a %>%
  mutate(count_yes = ifelse(invasion =="yes",1,0))%>%
  mutate(proportion_all = sum(count_yes)/nrow(.))%>%
  group_by(village)%>%
  mutate(count_village=n())%>%
  group_by(village)%>%
  mutate(proportion = sum(count_yes)/count_village)%>%
  mutate(all = "All")%>%
  ggplot(., aes(x=all, y=proportion_all, fill = invasion)) +
  geom_bar(position = "fill", stat="identity")+
  labs(x="", y="")+
  scale_fill_manual("", values = c("no" = "gray92", "yes" = "#B0A5F3"))+
  coord_flip()+
  # theme_bw()+
  # theme(legend.position = "none", 
  #       axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank())
  theme_bw(base_size = 12, base_family = "") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    plot.margin = margin(r = 20) 
  )

p3 <-dc_q_1a %>%
  mutate(count_yes = as.factor(ifelse(invasion =="yes",1,0)))%>%
  group_by(count_yes)%>%
  summarise(
    fence = sum(measures_fence, na.rm = TRUE),
    guard = sum(measures_guard, na.rm = TRUE),
    prop = sum(measures_propellas, na.rm = TRUE),
    fire = sum(measures_fire, na.rm = TRUE),
    guard_dog = sum(measures_guard_dog, na.rm = TRUE),
    pitfall = sum(measures_pitfall, na.rm = TRUE)
  )%>%
  pivot_longer(!count_yes, names_to = "measure", values_to = "count")%>%
  ggplot(., aes(x=measure, y=count, fill = count_yes)) +
  geom_col()+
  labs(x="Protection Measure", y="Count of Respondents")+
  scale_fill_manual(name = "Giraffe Invasion", values = c("0" = "gray92", "1" = "#B0A5F3"), labels = c("No","Yes"))+
  scale_x_discrete(labels=c(
  "prop" = "Sound devices",
  "pitfall" = "Trench",
  "guard_dog" = "Guard dog", 
  "guard" = "Human guard",
  "fire" = "Fire", 
  "fence"= "Fence"
))+
  coord_flip()+
  # theme_bw()+
  # theme(legend.position = "none")
  theme_bw(base_size = 12, base_family = "") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(r = 20) 
  )
################################################################################
# Figure 4

p2 + p1 + p3 + plot_layout(ncol = 1, heights = c(1, 10, 6)) 

################################################################################

# Question 1b - When is invasion by giraffes occurring?

p4 <-dc_q_1b %>%
  drop_na(invasion_frq)%>%
  group_by(invasion_frq)%>%
  summarize(tot = n())%>%
  mutate(All = "All")%>%
  #mutate(invasion_frq = replace_na(invasion_frq, "na"))%>%
  ggplot(., aes(x=All, y=tot, fill = invasion_frq)) +
  geom_bar(position = "fill", stat="identity")+
  labs(x="", y="")+
  scale_fill_manual("", values = c("all_year_round" = "#00006D", "wet" = "#0A93CD",
                                   "dry"="darkgoldenrod2"),
                       labels=c("all_year_round" = "All year", "wet" = "Wet season",
                            "dry" = "Dry season", "na" = "Not answered"))+
  coord_flip()+
  # theme_bw()+
  # theme(legend.position = "none", 
  #       axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank())
  theme_bw(base_size = 12, base_family = "") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    plot.margin = margin(r = 20) 
  )
  

p5 <-dc_q_1b %>%
  drop_na(invasion_frq)%>%
  group_by(village, invasion_frq)%>%
  summarize(tot_by_village = n())%>%
  #mutate(invasion_frq = replace_na(invasion_frq, "na"))%>%
  ggplot(., aes(x=village, y=tot_by_village, fill = invasion_frq)) +
  geom_bar(position = "fill", stat="identity")+
  scale_fill_manual("", values = c("all_year_round" = "#00006D", "wet" = "#0A93CD",
                                   "dry"="darkgoldenrod2"),
                    labels=c("all_year_round" = "All year", "wet" = "Wet season",
                             "dry" = "Dry season", "na" = "Not answered"))+
  scale_x_discrete(labels=c("shabah" = "Shabah", "sankuri" = "Sankuri",
                            "raya" = "Raya", "korakora" = "Korakora",
                            "jarirot" = "Jarirot", "bula_salama"= "Bula Salama",
                            "bula_gem" = "Bula Gem", "bula_bakari"="Bula Bakari",
                            "bula_adc" = "Bula Adc", "bour_algy"="Bour Algy",
                            "awarot"="Awarot"))+
  labs(x="Village", y="")+
  coord_flip()+
  # theme_bw()+
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank())
  theme_bw(base_size = 12, base_family = "") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    plot.margin = margin(r = 20) 
  )


p6 <-dc_q_1b %>%
  drop_na(invasion_frq)%>%
  group_by(invasion_frq)%>%
  summarize(Mango = sum(crop_mangoes), Lemons = sum(crop_lemons),
            Bananas = sum(crop_bananas), Vegetables = sum(crop_vegetable),
            Melon = sum(crop_melon))%>%
  mutate(invasion_frq = replace_na(invasion_frq, "na"))%>%
  pivot_longer(.,cols = c(Mango, Lemons, Bananas, Vegetables, Melon),
               names_to = "crop",
               values_to = "count")%>%
  ggplot(., aes(x=crop, y=count, fill = invasion_frq)) +
  geom_bar(position = "fill", stat="identity")+
  scale_fill_manual("", values = c("all_year_round" = "#00006D", "wet" = "#0A93CD",
                                   "dry"="darkgoldenrod2"),
                    labels=c("all_year_round" = "All year", "wet" = "Wet season",
                             "dry" = "Dry season", "na" = "Not answered"))+
  labs(x="Crop", y="Proportion of respondents")+
  coord_flip()+
  # theme_bw()+
  # theme(legend.position = "none")
  theme_bw(base_size = 12, base_family = "") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(r = 20) 
  )

################################################################################
# Figure 5

p4 + p5 + p6 + plot_layout(ncol = 1, heights = c(1, 10, 5)) 

################################################################################

# Question 2b - What measures are farmers willing to try?

p7 <-dc_q_2b %>%
  group_by(try_new)%>%
  filter(try_new != "no_interest")%>%
  summarize(Neighbor_uses = sum(trial_resn_neighbour_uses), Kws_use = sum(trial_resn_kws_use),
            Available = sum(trial_resn_available), Easy = sum(trial_resn_easy_use),
            Cheap = sum(trial_resn_cheap))%>%
  pivot_longer(.,cols = c(Neighbor_uses, Kws_use, Available, Easy, Cheap),
               names_to = "reason",
               values_to = "count")%>%
  ggplot(., aes(x=reason, y=count, fill = try_new)) +
  #ggplot(., aes(x=reason, y=count)) +
  #geom_bar(position = "stack", stat="identity")+
  geom_bar(position = "fill", stat="identity")+
  scale_fill_manual("", values = c("extreme_interest" = "#1f6f6f", "moderate_interest" = "#54a1a1",
                                   "slight_interest"="#9fc8c8"),
                    labels=c("extreme_interest" = "Extremely interested", "moderate_interest" = "Moderately interested",
                             "slight_interest" = "Slightly interested"))+
  scale_x_discrete(labels = c("Neighbor_uses" = "Neighbor Uses", "Kws_use" = "Kenya Wildlife\nService Uses", 
                              "Easy" = "Easy", "Cheap" = "Cheap", "Available" = "Available"))+
  labs(x="Reason", y="Proportion of respondents")+
  coord_flip()+
  #facet_grid(.~try_new)+
  #theme_bw()
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

################################################################################
# Figure 6

p7 + plot_layout(ncol = 1, heights = c(5)) 

################################################################################

# Question 2c - What measures are farmers willing to try?
p8 <- dc_q_2c %>%
  mutate(new_mtd_clean = ifelse(is.na(new_mtd_clean), "none", new_mtd_clean)) %>%
  group_by(try_new, new_mtd_clean) %>%
  summarize(
    Neighbor_uses = sum(trial_resn_neighbour_uses),
    Kws_use = sum(trial_resn_kws_use),
    Available = sum(trial_resn_available),
    Easy = sum(trial_resn_easy_use),
    Cheap = sum(trial_resn_cheap)
  ) %>%
  pivot_longer(
    cols = c(Neighbor_uses, Kws_use, Available, Easy, Cheap),
    names_to = "reason",
    values_to = "count"
  ) %>%
  # Recode the reason names for cleaner y-axis
  mutate(reason = recode(reason,
                         Neighbor_uses = "Used by neighbor",
                         Kws_use = "Used by Kenya \nWildlife Service uses",
                         Available = "Available",
                         Easy = "Easy",
                         Cheap = "Cheap")) %>%
  ggplot(aes(x = reason, y = count, fill = try_new)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(
    "",
    values = c(
      "extreme_interest" = "#1f6f6f",
      "moderate_interest" = "#a00000",
      "slight_interest" = "#b8b8b8"
    ),
    labels = c(
      "extreme_interest" = "Extremely interested",
      "moderate_interest" = "Moderately interested",
      "slight_interest" = "Slightly interested"
    )
  ) +
  labs(x = "Reason", y = "Proportion of respondents") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 3),
    axis.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    plot.margin = margin(r = 20)
  )

p8 + plot_layout(ncol = 1, heights = c(5)) 


# Question 2d - Why do farmers think their current methods are effective (or ineffective)?
p9 <-dc_long %>%
  mutate(reason_clean = ifelse(is.na(reason_clean), "none", reason_clean))%>%
  group_by(reason_clean, measure, effectiveness)%>%
  count()%>%
  ggplot(., aes(x=reason_clean, y=n, fill = effectiveness)) +
  geom_bar(position = "fill", stat="identity")+
  scale_fill_manual("", values = c("extremely" = "#1f6f6f", "Highly" = "#54a1a1",
                                   "moderately"="#9fc8c8","not"="azure4","don_know"="azure3"),
                    labels=c("extremely" = "Extremely effective", "Highly" = "Highly effective",
                             "moderately" = "Moderately effective", "not"= "Not effective", "don_know"= "Don't know"))+
  labs(x="Reason", y="Proportion of respondents")+
  facet_grid(measure~.)+
  coord_flip()+
  theme_bw()

p9 + plot_layout(ncol = 1, heights = c(5))

# Question 3 - Why do farmers think their current methods are effective (or ineffective)?
p10 <-dc_q_3 %>%
  group_by(row_id, try_new, effectiveness, measure)%>%
  count()%>%
  mutate(willing = case_when(
    try_new == "no_interest" ~ "no",
    try_new == "extreme_interest" ~"yes",
    try_new == "slight_interest" ~ "yes",
    try_new == "moderate_interest" ~ "yes"))%>%
  ggplot(., aes(x=willing, y=n, fill = effectiveness)) +
  geom_bar(stat="identity")+
  scale_fill_manual("Current measure\neffectiveness", values = c("extremely" = "#1f6f6f", "Highly" = "#54a1a1",
                                   "moderately"="#9fc8c8","not"="azure4","don_know"="azure3"),
                    labels=c("extremely" = "Extremely effective", "Highly" = "Highly effective",
                             "moderately" = "Moderately effective", "not"= "Not effective", "don_know"= "Don't know"))+
  labs(x="Willing to try new measures", y="Number of responses")+
  facet_wrap(.~measure, scales = "free_y")+
  #coord_flip()+
  theme_bw()



################################################################################
## 04. Create summary plots of proportions of responses

# Load function to create summary response plots
#source("./03_fgc_plot_prop.R")
source("./03_fgc_plot_prop_jg.R")

# Load function to create summary response plots

source("./03_fgc_plot_prop_for_multivar.R", echo = TRUE)

# Load function to create summary response relationship plots

source("./03_fgc_plot_prop_for_multivar_relationships.R", echo = TRUE)
# Run function to plot data summary data of categories
### Note 09/10/2025 - this one is the most generalizable and can replace all of the
### other 03_plotting functions

# https://select-statistics.co.uk/blog/analysing-categorical-survey-data/#:~:text=Categorical%20data%20can%20also%20be,our%20choice%20of%20summary%20statistics.

### Demographic questions

## By gender (question #1)
fgc_plot_prop(dc = dc, variable = gender, xlab = "Gender", 
              title = "Respondents by Gender", "resp_by_gender")

## By age (question #2)
fgc_plot_prop(dc = dc, variable = age, xlab = "Age", 
              title = "Respondents by Age", "resp_by_age")

## By education (question #3)
fgc_plot_prop(dc = dc, variable = education, xlab = "Highest Education", 
              title = "Respondents by Education", filename = "resp_by_education")

## By profession (question #4) #### UPDATE FUNCTION TO ACCOMODATE??
fgc_plot_prop(dc = dc, variable = career, xlab = "Career", 
              title = "Respondents by career type", filename = "resp_by_career")

## By residency (question #5) #### NOTE TYPO IN COLUMN NAME
fgc_plot_prop(dc = dc, variable = resisdence_time, xlab = "Residence Time", 
              title = "Respondents by Residence Time", filename = "resp_by_resd")

## By birth place (question #6) 
fgc_plot_prop(dc = dc, variable = birth_place, xlab = "Born in Garissa", 
              title = "Respondents born in Garissa", filename = "resp_by_birth_pl")

## By farm role (question #7) 
fgc_plot_prop(dc = dc, variable = farm_role, xlab = "Role on Farm", 
              title = "Respondents by farm role", filename = "resp_by_farm_role")

## By distance from river Tana (question #8) 
fgc_plot_prop(dc = dc, variable = river_dist, xlab = "Distance of Farm from River Tana", 
              title = "Respondents by Distance From River Tana", filename = "resp_by_river_dist")

## By farm size (question #9) 
fgc_plot_prop(dc = dc, variable = farm_size, xlab = "Farm Size", 
              title = "Respondents by Farm Size", filename = "resp_by_farm_size")

## By land use activities (question #10) 
fgc_plot_prop(dc = dc, variable = landuse, xlab = "Land Use Allowed", 
              title = "Respondents by Land Use Allowed", filename = "resp_by_land_use")

## By crops (question #12) #### NEED TO FIX CODE TO ADJUST CATEGORIES
fgc_plot_prop(dc = dc, variable = crop, xlab = "Crops", 
              title = "Respondents by Crop Type", filename = "resp_by_crop")

### Plotting vairable with multiple columns (crops)
fgc_plot_prop_multi(
  dc = dc,
  columns = names(dc)[grepl("^crop_", names(dc))],  # all crop_* columns
  xlab = "Crops",
  title = "Respondents by Crop Type",
  filename = "resp_by_crop"
)

## By land acquisition (question #13) 
fgc_plot_prop(dc = dc, variable = ownership_by, xlab = "Land Aquisition", 
              title = "How did you aquire the land?", filename = "resp_by_land_aq")

## By land tenure system (question #14) 
fgc_plot_prop(dc = dc, variable = tenure, xlab = "Land Tenure System", 
              title = "How can you describe your land tenure system", filename = "resp_by_land_ten")

### Perception of overlap/conflict

## By invasion (question #1A) 
fgc_plot_prop(dc = dc, variable = invasion, xlab = "Do giraffes invade?", 
              title = "Giraffe invasion by respondents", filename = "resp_by_invasion")

## By invasion (question #1B) 
fgc_plot_prop(dc = dc, variable = giraffe_action, xlab = "What are giraffes doing?", 
              title = "Giraffe Action Perception", filename = "resp_by_girf_action")

### Alternative
fgc_plot_prop_multi(
  dc = dc,
  columns = names(dc)[grepl("^giraffe_action_", names(dc))],  
  xlab = "Giraffe action on the farm",
  title = "Respondents by giraffe actions on farm",
  filename = "resp_by_giraffe_action"
)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## By frequency (question #??) #### NOTE - FIND QUESTION #
fgc_plot_prop(dc = dc, variable = invasion_frq, xlab = "Giraffe Invasion Timing", 
              title = "Perception of Giraffe Invasion Frequency", 
              filename = "resp_by_invs_freq")

## By reason (question #1C) 
fgc_plot_prop(dc = dc, variable = giraffe_action, xlab = "Giraffe Invasion Reason", 
              title = "Perception of Giraffe Invasion Reason", 
              filename = "resp_by_invs_reason")

## Crop output with no damage (question #1H) 
fgc_plot_prop(dc = dc, variable = harvest1, xlab = "Crop output with no Invasion", 
              title = "Crop output with no Invasion", 
              filename = "resp_by_crop_output_no_invasion")

## Crop output with damage (question #1I) 
fgc_plot_prop(dc = dc, variable = harvest2, xlab = "Crop output with Invasion", 
              title = "Crop output with Invasion", 
              filename = "resp_by_crop_output_invasion")

## Don't have a good measure of crop loss (so people perceive loss but don't measure it 
## in quantitative ways)


## By timing (question #1D) 
fgc_plot_prop(dc = dc, variable = invasion_time, xlab = "Giraffe Invasion Time of Day", 
              title = "Perception of Giraffe Invasion Time of Day", 
              filename = "resp_by_invs_freq_day")
# Who are the people perceiving that they aren't invading in the dry season (might have to do with location)

## By invasion trend (question #1F) 
fgc_plot_prop(dc = dc, variable = invasion_trend, xlab = "Giraffe Invasion Trend", 
              title = "Perception of Giraffe Invasion Trend", 
              filename = "resp_by_invs_trend")
# Who are the people perceiving that they aren't increasing? (don't have data to speak to giraffes on this)


## By reduction methods (question #2C) 
fgc_plot_prop(dc = dc, variable = measures, xlab = "Giraffe Protection Measures", 
              title = "Type of Giraffe Protection Measures", 
              filename = "resp_by_invs_measures")

##### Alternative 
fgc_plot_prop_multi(
  dc = dc,
  columns = names(dc)[grepl("^measures_", names(dc))],  
  xlab = "Measure",
  title = "Type of Giraffe Protection Measures",
  filename = "resp_by_invs_measures"
)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## By reduction methods in place - y/n (question #2B) 
fgc_plot_prop(dc = dc, variable = using_measure, xlab = "Giraffe Protection Measures", 
              title = "Implementation of Giraffe Protection Measures", 
              filename = "resp_by_invs_measures_impl")




################################################################################
## 04. Create summary plots to combine two different types of responses

# Load function to create plots that include multiple variables

source("./04_fgc_plot_prop_facet.R")

## Example run: by invasion and farm size
fgc_plot_prop(dc = dc, variable1 = invasion, facet_variable = farm_size, xlab = "Giraffe Invasion", title = "Invasion by Farm Size")

# Draft code for plots that will be included in function
# d2 <-dc %>%
#   group_by(farm_size,invasion, education) %>%
#   mutate(count = n())%>%
#   select(farm_size, invasion, education, count)%>%
#   distinct()
# 
# d2 <-dc %>%
#   group_by(farm_size, invasion, crop_lemons) %>%
#   mutate(count = n())%>%
#   select(farm_size, invasion, crop_lemons, count)%>%
#   distinct()
# 
# ggplot(d2, aes(x = invasion, y = count)) +
#   geom_bar(stat = "identity") +
#   xlab("Giraffe Invasion") +
#   ylab("Count") +
#   labs("Giraffe Invasion by Measure and Size")+
#   facet_grid(crop_lemons~.)+
#   theme_light()

# HOW do people understand this conflict? 
# Mangos flower only in dry season, so people think its a mango problem
# But it's a dry season problem

# 1) Is there a mismatch between what is happening with giraffes and  
# 2) Mismatch fueled by multiple things, ways they should benefit from these animals
# is not happening (ecotourism not happening)


################ How to apply new relationship function ################
################################## Examples ####################################

## Example 1 — Basic count plot (no grouping/faceting)
fgc_plot_prop_multi(
  dc = dc,
  columns = names(dc)[grepl("^trial_resn", names(dc))],
  xlab = "Crop Type",
  fill = ""
  title = "Respondents by Crop Type",
  filename = "resp_by_crop"
)

## Example 2 — Relationship plot with grouping (e.g., career)
rel_var_with_multcol(
  dc = dc,
  columns = names(dc)[grepl("^trial_resn_", names(dc))],
  fillvar = "^try_new",
  xlab = "Crop Type",
  title = "Crop Type by career",
  filename = "rel_crop_career"
)

## Example 3 — With one facet (e.g., gender)
rel_var_with_multcol(
  dc = dc,
  columns = names(dc)[grepl("^trial_resn_", names(dc))],
  fillvar = "^try_new",
  facetvar = "invasion",
  xlab = "Crop Type",
  title = "Crop Type by Education and Gender",
  filename = "rel_crop_education_gender"
)

## Example 4 — With two facets (e.g., gender x resisdence_time)
rel_var_with_multcol(
  dc = dc,
  columns = names(dc)[grepl("^trial_res", names(dc))],
  fillvar = "try_new",
  facetvar = c("gender", "resisdence_time"),
  xlab = "Crop Type",
  title = "Crop Type by Education, Gender, and Residence Time",
  filename = "rel_crop_edu_gender_res"
)
