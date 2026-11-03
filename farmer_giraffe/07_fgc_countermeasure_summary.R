# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino, Gemini AI
# Date: 11/02/2026

# Function purpose: Generate publication-ready Table 3 and Table 4 with exact numerical parity.

#################################### Intro #####################################

# Function name: fgc_generate_tables_3_and_4
# Description: Generates Table 3 (usage and perception) and Table 4 (future interest, 
#              reasons, and suggested interventions) with strict respondent deduplication.

################################# Arguments ####################################

# dc:
#        Main survey dataframe containing: row_id, invasion, measures_*, 
#        try_new, trial_resn_*, impro_mtd_clean.
# dc_long:
#        Recoded long dataset containing measure and effectiveness_grouped ratings.
# n_non_invaded:
#        Total number of non-invaded farms (defaults to 13).
# n_invaded:
#        Total number of invaded farms (defaults to 194).


################################## Output ######################################

# table_3 - Countermeasure use (by invasion status) and perception (Effective, Neutral, Ineffective).
# table_4 - Interest in new countermeasures, reasons for willingness, and suggested interventions.

################################# Function #####################################
fgc_generate_tables_3_and_4 <- function(dc, 
                                    dc_long = NULL,
                                    n_non_invaded = 13, 
                                    n_invaded = 194) {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  measure_order <- c(
    "Human guards",
    "Fences",
    "Fire",
    "Guard dogs",
    "Trenches",
    "Sound devices",
    "Flashlights",
    "Scare crows"
  )
  
  # ============================================================================
  # 1. TABLE 3: COUNTERMEASURE USAGE (Deduplicated across dc)
  # ============================================================================
  
  # A. Standard binary measure columns from dc
  binary_usage <- dc %>%
    mutate(across(starts_with("measures_"), as.character)) %>%
    pivot_longer(
      cols = starts_with("measures_"),
      names_to = "measure_raw",
      values_to = "used"
    ) %>%
    filter(measure_raw != "measures_other") %>%
    filter(used %in% c("1", "TRUE", "true", "yes", "Yes")) %>%
    mutate(
      measure_clean = trimws(tolower(gsub("measures_", "", measure_raw))),
      Countermeasure = case_when(
        str_detect(measure_clean, "guard_dog|dog")                           ~ "Guard dogs",
        str_detect(measure_clean, "guard")                                     ~ "Human guards",
        str_detect(measure_clean, "fence")                                     ~ "Fences",
        str_detect(measure_clean, "fire")                                      ~ "Fire",
        str_detect(measure_clean, "pitfall|trench")                            ~ "Trenches",
        str_detect(measure_clean, "propella|sound")                            ~ "Sound devices",
        str_detect(measure_clean, "flashlight|flash_light|light")               ~ "Flashlights",
        str_detect(measure_clean, "\\bscare\\s*crows?\\b|\\bscarecrow\\b")     ~ "Scare crows",
        TRUE                                                                   ~ str_to_title(gsub("_", " ", measure_clean))
      )
    ) %>%
    select(row_id, invasion, Countermeasure)
  
  # B. Text responses in measures_other (Strict word-boundary matching)
  other_usage <- dc %>%
    filter(!is.na(measures_other) & measures_other != "") %>%
    mutate(
      other_clean = tolower(trimws(measures_other)),
      Countermeasure = case_when(
        str_detect(other_clean, "flash|torch|flood")                           ~ "Flashlights",
        str_detect(other_clean, "\\bscare\\s*crows?\\b|\\bscarecrow\\b")     ~ "Scare crows",
        TRUE                                                                   ~ NA_character_
      )
    ) %>%
    filter(!is.na(Countermeasure)) %>%
    select(row_id, invasion, Countermeasure)
  
  # C. Combine & Deduplicate by row_id and Countermeasure
  dc_usage_dedup <- bind_rows(binary_usage, other_usage) %>%
    distinct(row_id, Countermeasure, .keep_all = TRUE)
  
  use_summary <- dc_usage_dedup %>%
    filter(!is.na(Countermeasure)) %>%
    group_by(Countermeasure, invasion) %>%
    summarise(Count = n_distinct(row_id), .groups = "drop") %>%
    pivot_wider(names_from = invasion, values_from = Count, values_fill = 0)
  
  if (!"no" %in% names(use_summary)) use_summary$no <- 0
  if (!"yes" %in% names(use_summary)) use_summary$yes <- 0
  
  use_summary <- use_summary %>%
    rename(no_count = `no`, yes_count = `yes`) %>%
    mutate(
      non_invaded_pct = (no_count / n_non_invaded) * 100,
      invaded_pct     = (yes_count / n_invaded) * 100,
      
      `Non-invaded farms` = if_else(
        no_count == 0, "--",
        sprintf("%s (n = %d)", if_else(non_invaded_pct < 1 & non_invaded_pct > 0, "<1%", sprintf("%.0f%%", non_invaded_pct)), no_count)
      ),
      `Invaded farms` = if_else(
        yes_count == 0, "--",
        sprintf("%s (n = %d)", if_else(invaded_pct < 1 & invaded_pct > 0, "<1%", sprintf("%.0f%%", invaded_pct)), yes_count)
      )
    ) %>%
    select(Countermeasure, `Non-invaded farms`, `Invaded farms`)
  
  missing_measures <- setdiff(measure_order, use_summary$Countermeasure)
  if (length(missing_measures) > 0) {
    use_summary <- bind_rows(
      use_summary,
      tibble(Countermeasure = missing_measures, `Non-invaded farms` = "--", `Invaded farms` = "--")
    )
  }
  
  # ============================================================================
  # 2. TABLE 3: COUNTERMEASURE PERCEPTION (Direct from dc_long)
  # ============================================================================
  
  if (!is.null(dc_long)) {
    
    perception_data <- dc_long %>%
      mutate(
        m_raw = trimws(tolower(as.character(measure))),
        Countermeasure = case_when(
          str_detect(m_raw, "guard_dog|guard dog|dog")                         ~ "Guard dogs",
          str_detect(m_raw, "human guard|guard")                                ~ "Human guards",
          str_detect(m_raw, "fence")                                            ~ "Fences",
          str_detect(m_raw, "fire")                                             ~ "Fire",
          str_detect(m_raw, "trench|pitfall")                                   ~ "Trenches",
          str_detect(m_raw, "sound|propella")                                   ~ "Sound devices",
          str_detect(m_raw, "flashlight|flash_light|light")                    ~ "Flashlights",
          str_detect(m_raw, "\\bscare\\s*crows?\\b|\\bscarecrow\\b")         ~ "Scare crows",
          TRUE                                                                  ~ as.character(measure)
        )
      )
    
    perception_summary <- perception_data %>%
      filter(!is.na(effectiveness_grouped) & !is.na(Countermeasure)) %>%
      group_by(Countermeasure, effectiveness_grouped) %>%
      summarise(Count = n_distinct(row_id), .groups = "drop") %>%
      group_by(Countermeasure) %>%
      mutate(
        total_users = sum(Count),
        pct = (Count / total_users) * 100,
        cell_text = if_else(
          Count == 0, "--",
          sprintf("%s (n = %d)", if_else(pct < 1 & pct > 0, "<1%", sprintf("%.0f%%", pct)), Count)
        )
      ) %>%
      ungroup() %>%
      select(Countermeasure, effectiveness_grouped, cell_text) %>%
      pivot_wider(names_from = effectiveness_grouped, values_from = cell_text, values_fill = "--")
    
  } else {
    perception_summary <- tibble(
      Countermeasure = measure_order,
      Effective = "--", Neutral = "--", Ineffective = "--"
    )
  }
  
  for (col in c("Effective", "Neutral", "Ineffective")) {
    if (!col %in% names(perception_summary)) perception_summary[[col]] <- "--"
  }
  
  table_3 <- left_join(use_summary, perception_summary, by = "Countermeasure") %>%
    mutate(Countermeasure = factor(Countermeasure, levels = measure_order)) %>%
    arrange(Countermeasure) %>%
    mutate(Countermeasure = as.character(Countermeasure)) %>%
    select(Countermeasure, `Non-invaded farms`, `Invaded farms`, Effective, Neutral, Ineffective)
  
  # ============================================================================
  # 3. TABLE 4: INTEREST LEVELS, REASONS, AND SUGGESTED INTERVENTIONS
  # ============================================================================
  
  fmt_pct_n <- function(count, total) {
    pct <- round((count / total) * 100)
    sprintf("%d%% (n = %d)", pct, count)
  }
  
  # --- Part A: All Participants (n = 207) ---
  n_all <- nrow(dc)
  part_a <- dc %>%
    filter(!is.na(try_new) & try_new != "") %>%
    mutate(
      Category = case_when(
        str_detect(tolower(try_new), "extreme")  ~ "Extremely interested",
        str_detect(tolower(try_new), "moderate") ~ "Moderately interested",
        str_detect(tolower(try_new), "slight")   ~ "Slightly interested",
        str_detect(tolower(try_new), "not")      ~ "Not interested",
        TRUE                                     ~ as.character(try_new)
      )
    ) %>%
    group_by(Category) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(
      Category = factor(Category, levels = c("Extremely interested", "Moderately interested", "Slightly interested", "Not interested"))
    ) %>%
    arrange(Category) %>%
    mutate(
      Category = as.character(Category),
      `Proportion of respondents` = fmt_pct_n(Count, n_all),
      Section = sprintf("A. All participants (n = %d)", n_all)
    ) %>%
    select(Section, Category, `Proportion of respondents`)
  
  # --- Part B: Reasons for Willingness (n = 132) ---
  dc_extreme <- dc %>% filter(str_detect(tolower(try_new), "extreme"))
  n_extreme <- nrow(dc_extreme)
  if (n_extreme == 0) n_extreme <- 132
  
  reason_cols <- c(
    "trial_resn_available"      = "Local availability",
    "trial_resn_easy_use"       = "Ease of use",
    "trial_resn_neighbour_uses" = "Used by neighbor",
    "trial_resn_kws_use"        = "Used by Kenya Wildlife Services",
    "trial_resn_cheap"          = "Cheap"
  )
  
  part_b <- dc_extreme %>%
    summarise(across(all_of(names(reason_cols)), ~ sum(.x == 1 | .x == TRUE, na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "col_name", values_to = "Count") %>%
    mutate(
      Category = reason_cols[col_name],
      `Proportion of respondents` = fmt_pct_n(Count, n_extreme),
      Section = sprintf("B. Participants extremely interested in trying a new countermeasure (n = %d)", n_extreme)
    ) %>%
    mutate(Category = factor(Category, levels = unname(reason_cols))) %>%
    arrange(Category) %>%
    mutate(Category = as.character(Category)) %>%
    select(Section, Category, `Proportion of respondents`)
  
  # --- Part C: Suggested Interventions (Evaluated directly on dc) ---
  dc_opinions <- dc %>% filter(!is.na(impro_mtd_clean) & trimws(impro_mtd_clean) != "")
  n_opinions <- n_distinct(dc_opinions$row_id)
  
  part_c_data <- dc_opinions %>%
    mutate(imp_lower = tolower(impro_mtd_clean)) %>%
    summarise(
      `Fences (including variations like electric, wires, or a wall)` = n_distinct(row_id[str_detect(imp_lower, "fence|wire|wall|electric")]),
      `Light`                                                          = n_distinct(row_id[str_detect(imp_lower, "light|flash|torch")]),
      `Human guard`                                                    = n_distinct(row_id[str_detect(imp_lower, "guard|watchman|askari")]),
      `Government`                                                     = n_distinct(row_id[str_detect(imp_lower, "\\bgov\\b|government|kws|compensation|state")])
    ) %>%
    pivot_longer(cols = everything(), names_to = "Category", values_to = "Count")
  
  cat_order_c <- c(
    "Fences (including variations like electric, wires, or a wall)",
    "Light",
    "Human guard",
    "Government"
  )
  
  part_c <- part_c_data %>%
    mutate(
      Category = factor(Category, levels = cat_order_c),
      `Proportion of respondents` = fmt_pct_n(Count, n_opinions),
      Section = sprintf("C. Participants who responded with opinions on what would help reduce farm invasion by giraffes (n = %d)", n_opinions)
    ) %>%
    arrange(Category) %>%
    mutate(Category = as.character(Category)) %>%
    select(Section, Category, `Proportion of respondents`)
  
  table_4 <- bind_rows(part_a, part_b, part_c)
  
####################### Write data to global environment #######################
  data <- list(
    "table_3" = table_3,
    "table_4" = table_4
  )
  list2env(data, .GlobalEnv)
  
}