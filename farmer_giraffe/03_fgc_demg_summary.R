# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino, Gemini AI
# Date: 10/26/2026

# Function purpose: Generate a single unified summary table from a survey dataframe.

#################################### Intro #####################################

# Function name: fgc_demg_summary_table
# Description: Summarizes survey data into a single table, combining demographic 
#              characteristics (gender, age, education) followed by farm 
#              characteristics (village, farm size, crop number, crop type).
#              Handles comma-separated lists in the `crop` variable.

################################# Arguments ####################################

# df:
#        Data frame (e.g., `dc`) containing survey variables: gender, age, 
#        education, village, farm_size, and crop.
# total_n:
#        Optional numeric total sample size (defaults to `nrow(df)`).


################################## Output ######################################

# combined_summary - A single summary table containing all demographic and farm
#                    characteristic statistics formatted as "XX% (n = YY)".

################################# Function #####################################
fgc_demg_summary_table <- function(df, total_n = NULL) {
  
  # Load required packages within scope
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  
  if (is.null(total_n)) {
    total_n <- nrow(df)
  }
  
  # Helper function to summarize single-choice categorical variables
  summarize_cat <- function(data, var_name, category_header) {
    data %>%
      filter(!is.na(.data[[var_name]])) %>%
      group_by(Category = as.character(.data[[var_name]])) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(
        Pct = round((Count / total_n) * 100),
        `Proportion of Survey Sample` = sprintf("%d%% (n = %d)", Pct, Count),
        Header = category_header
      ) %>%
      select(Header, Category, `Proportion of Survey Sample`)
  }
  
  # --- 1. Demographic Characteristics ---
  gender_sum    <- summarize_cat(df, "gender", "Gender")
  age_sum       <- summarize_cat(df, "age", "Age")
  education_sum <- summarize_cat(df, "education", "Education")
  
  # --- 2. Farm Characteristics ---
  village_sum   <- summarize_cat(df, "village", "Village")
  farm_size_sum <- summarize_cat(df, "farm_size", "Farm Size")
  
  crop_number_sum <- df %>%
    filter(!is.na(crop) & crop != "") %>%
    mutate(
      crop_count = str_count(crop, ",") + 1,
      Category = if_else(crop_count == 1, "Single crop", "Two or more crops")
    ) %>%
    group_by(Category) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(
      Pct = round((Count / total_n) * 100),
      `Proportion of Survey Sample` = sprintf("%d%% (n = %d)", Pct, Count),
      Header = "Crop Number"
    ) %>%
    select(Header, Category, `Proportion of Survey Sample`)
  
  crop_type_sum <- df %>%
    filter(!is.na(crop) & crop != "") %>%
    separate_rows(crop, sep = "\\s*,\\s*") %>%
    mutate(crop = str_to_title(trimws(crop))) %>%
    filter(crop != "") %>%
    group_by(Category = crop) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(
      Pct = round((Count / total_n) * 100),
      `Proportion of Survey Sample` = sprintf("%d%% (n = %d)", Pct, Count),
      Header = "Crop Type*"
    ) %>%
    arrange(desc(Count)) %>%
    select(Header, Category, `Proportion of Survey Sample`)
  
  # --- Combine into a single table (Demographics first, then Farm info) ---
  combined_summary <- bind_rows(
    gender_sum,
    age_sum,
    education_sum,
    village_sum,
    farm_size_sum,
    crop_number_sum,
    crop_type_sum
  )
  
####################### Write data to global environment #######################
  data <- list(
    "dc_demg_summary" = combined_summary
  )
  list2env(data, .GlobalEnv)
  
}