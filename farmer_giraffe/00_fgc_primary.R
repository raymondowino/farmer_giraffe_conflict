# Farmer/Giraffe conflict (FGC)
# Author: Jessie Golding, Raymond Owino
# Date: 11/02/2026

# Primary File
# Run all data loading, cleaning, analysis, and output creation for survey component
# here

# Accompanies publication Owino et al. (Under review). Crop raiding as an emerging 
# threat to giraffes: drivers and perceived effectiveness of countermeasures.

################################################################################
## 00. Setup

# Set working directory where you have files downloaded from GitHub stored
# setwd("filepath") ###### UPDATE WITH YOUR FILEPATH

# Load functions
source("./01_fgc_load_packages.R") # load packages for analysis
source("./02_fgc_data_load_clean.R") # clean and format data
source("./03_fgc_demg_summary.R") # generate table of demographic summary info
source("./04_fgc_filter_data.R") # filter function to select specific data for questions
source("./05_fgc_plot_invasion_summaries.R") # filter function to select specific data for questions
source("./06_fgc_run_invasion_analysis.R") # run multivariable Firth regression
source("./07_fgc_countermeasure_summary.R") # summarizes countermeasure data
source("./08_fgc_plot_countermeasure_percep.R") # plots perception of countermeasure effectiveness
source("./09_fgc_plot_countermeasure_int.R") # plots interest in countermeasures

################################################################################
## 01. Load, format, and clean data

# Load data
d <-read_csv("./raw_data/survey_responses.csv")

# Run function to clean data
fgc_load_clean_data(d)

# Load the long version of the data (dc_long) which contains more detailed 
# information on the survey responses about motivations
dc_long <-read_csv("./raw_data/dc_long.csv")

# Creates tibble "dc" and "dc_long" which is all cleaned survey data

################################################################################
## 02. Create demographic summary table for survey responses

# Run generate summary table
fgc_demg_summary_table(dc)
print(dc_demg_summary)

# Creates tibble "dc_demg_summary" which is a non-formatted version of Table 1 
# in Owino et al.  
# Table 1. Demographic and farm characteristics of those that participated in the 
# survey. Note that for crop type the percentages add up to more than 100 
# because most farms grew multiple crops.

################################################################################
## 03. Examine invasion relative to farm characteristics and measures

# Is invasion occurring? If so where and when and what countermeasures are implemented? 
# Select columns of row id, village, invasion frequency, crop types, and defense measures

fgc_filter_data(dc, "invasion", c("row_id", "village", "invasion", "invasion_frq",
                                  "crop_mangoes", "crop_lemons", "crop_bananas", 
                                  "crop_vegetable", "crop_melon", "measures_fence", 
                                  "measures_guard", "measures_fire", 
                                  "measures_propellas", "measures_guard_dog",
                                  "measures_pitfall"))

# Creates tibble "dc_invasion" of all characteristics of giraffe invasion timing, 
# location, and countermeasure used in Figures 4 and 5 below

# Create plot for Figure 5 
fig4_crops <- c(
  "crop_mangoes"   = "Mango",
  "crop_lemons"    = "Lemons",
  "crop_bananas"   = "Bananas",
  "crop_vegetable" = "Vegetables",
  "crop_melon"     = "Melon"
)

fgc_create_composite_figure(
  df = dc_invasion,
  group_var = "invasion_frq",
  measure_cols = fig4_crops,
  fill_colors = c("all_year_round" = "#00006D", "wet" = "#0A93CD", "dry" = "darkgoldenrod2"),
  fill_labels = c("all_year_round" = "All year", "wet" = "Wet season", "dry" = "Dry season"),
  legend_title = "",
  p3_x_label = "Crop",
  p3_y_label = "Proportion of respondents",
  panel_heights = c(1, 10, 5)
)

# Create Figure 4 composite 
fig4 <- composite_plot
print(fig4)

# Summarize total that have experienced invasion
dc_invasion_summary <-dc_invasion %>%
  group_by(invasion)%>%
  summarise(n = n())%>%
  mutate(percent = round((n/207)*100))
print(dc_invasion_summary)

# Creates tibble "dc_invasion_summary" with just the totals of how many farms
# have experiences giraffe invasion 

# Create plot for Figure 5  
fig5_measures <- c(
  "measures_fence"    = "Fence",
  "measures_fire"     = "Fire",
  "measures_guard"    = "Human guard",
  "measures_guard_dog"= "Guard dog",
  "measures_pitfall"  = "Trench",
  "measures_propellas"= "Sound devices"
)

fgc_create_composite_figure(
  df = dc_invasion,
  group_var = "invasion",
  measure_cols = fig5_measures,
  fill_colors = c("no" = "gray92", "yes" = "#B0A5F3"),
  fill_labels = c("no" = "No", "yes" = "Yes"),
  legend_title = "Giraffe Invasion",
  p3_x_label = "Protection Measure",
  p3_y_label = "Proportion of Respondents",
  panel_heights = c(1, 10, 6)
)

# Create Figure 5 composite 
fig5 <- composite_plot
print(fig5)

# Creates Figures 4 and 5 from Owino et al. 
# Figure 4: Stacked bar chart illustrating (A) the proportion of farmers who 
# thought giraffe invasion occurred all year (dark blue), only in the dry season 
# (orange), or only in the wet season (light blue); (B) the proportion of farmers 
# by village who thought giraffe invasion occurred all year, only in the dry season, 
# or only in the wet season; and (C) the proportion of farmers by the type of crop 
# grown who thought giraffe invasion occurred all year, only in the dry season, 
# or only in the wet season.

# Figure 5: Figure 5: Stacked bar charts illustrating: (A) the proportion of farmers 
# who did (purple) or did not (gray) experience giraffe invasion on their farm; 
# (B) the proportion of farmers by village who did (purple) or did not (gray) 
# experience giraffe invasion on their farm; and (C) the count of measures used 
# by individuals who did (purple) or did not (gray) experience giraffe invasion. 
# Some individuals used multiple measures, so counts add up to more than the 207 
# farmers.

################################################################################
## 04. Examine invasion relative to crop growing

# Create new data frame for examining farm crop characteristics with invasion
dc_invasion_crop <- dc_invasion %>%
  select(row_id, invasion, crop_mangoes, crop_lemons, crop_bananas, crop_vegetable, crop_melon)%>%
  mutate(crop_total = rowSums(across(crop_mangoes:crop_melon), na.rm = TRUE))

# Run multivariable Firth regression and create Table 2 output
results_table <- fgc_run_giraffe_invasion_analysis(dc_invasion_crop)
print(results_table)

# Creates "results_table" which is a non-formatted version of Table 2 in Owino et al. 
# Table 2. Summary of farm characteristics, observed giraffe invasion rates, 
# and multivariable Firth penalized logistic regression results. 

################################################################################
## 05. Summarize use and perceptions about countermeasures

#  Format dc_long and clean measure labels
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

# Format dc_long and recode effectiveness into 3 categories
dc_long <- dc_long %>%
  mutate(effectiveness_clean = tolower(effectiveness),
         effectiveness_grouped = case_when(
           effectiveness_clean %in% c("extremely", "highly") ~ "Effective",
           effectiveness_clean %in% c("moderately") ~ "Neutral",
           TRUE ~ "Ineffective"
         ))

# Run function to summarize the data and create summary tables
fgc_generate_tables_3_and_4(dc = dc, dc_long = dc_long)

# Output objects generated in your global workspace:
print(table_3)
print(table_4)

################################################################################
## 06. Create plot to show perceptions of countermeasures

# Create Figure 6  
fig6 <- fgc_plot_countermeasure_effectiveness(dc_long)
print(fig6)

# Creates Figure 6 from Owino et al. 
# Figure 6: The percentage (bar length) and total number of farmers (text on bars) 
# who mentioned the efficacy of countermeasures and whether or not they thought 
# countermeasures were ineffective (negative; orange), neutral (gray), 
# or effective (positive; blue). 

################################################################################
## 07. Create plot to show why respondents are interested in adopting new countermeasure

# Create Figure 7  
fig7 <- fgc_plot_trial_reasons_by_interest(dc)
print(fig7)

# Creates Figure 7 from Owino et al. 
# Figure 7: A graph showing factors influencing farmers’ decisions to try a new 
# method of reducing farm invasion by giraffes. The colors teal, red, and gray 
# represent farmers that responded that they were extremely, moderately, and 
# slightly interested in trying a new method. .
