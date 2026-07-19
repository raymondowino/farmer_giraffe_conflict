# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino
# Date: 04/16/2025

# Function purpose: load and clean data

#################################### Intro #####################################

# Function name: fgc_load_clean_data
# Description:  function to load and clean data for the farmer giraffe conflict 
# (FGC) project

################################# Arguments ####################################

# d:
#       Data file (csv).

################################## Output ######################################

# dc - a tibble saved to global environment. Contains cleaned data responses for
# X survey respondents.

################################# Function #####################################
fgc_load_clean_data <- function(d) {
  
  d_clean <- d %>%
    filter(!if_all(everything(), is.na)) %>%
    select(-c(1:10), -c((ncol(.) - 2):ncol(.))) %>%
    rename_with(~ gsub("^data(-group\\d+)?-", "", .x)) %>%
    rename_with(~ gsub("^[^-]+-", "", .x)) %>%
    mutate(row_id = row_number())
  
  split_pivot_join <- function(df, col, prefix = NULL) {
    col_str <- deparse(substitute(col))
    if (is.null(prefix)) prefix <- paste0(col_str, "_")
    df_split <- df %>%
      select(row_id, {{col}}) %>%
      mutate(tmp_col = {{col}}) %>%
      replace_na(list(tmp_col = "")) %>%
      separate_rows(tmp_col, sep = ",") %>%
      mutate(tmp_col = trimws(tmp_col)) %>%
      filter(tmp_col != "", !tolower(tmp_col) %in% c("other", "others"))
    if (nrow(df_split) == 0) {
      return(df)
    }
    df_pivot <- df_split %>%
      mutate(tmp_val = 1) %>%
      pivot_wider(
        id_cols = row_id,
        names_from = tmp_col,
        values_from = tmp_val,
        values_fill = 0,
        names_prefix = prefix,
        names_repair = "unique"
      )
    out <- left_join(df, df_pivot, by = "row_id")
    new_cols <- setdiff(names(out), names(df))
    if (length(new_cols) > 0) {
      out[new_cols] <- lapply(out[new_cols], function(x) ifelse(is.na(x), 0, x))
    }
    out
  }
  
  d_clean <- d_clean %>%
    split_pivot_join(career) %>%
    split_pivot_join(landuse) %>%
    split_pivot_join(crop) %>%
    split_pivot_join(giraffe_action) %>%
    split_pivot_join(giraffe_attractants) %>%
    split_pivot_join(invasion_time, prefix = "invasion_time_") %>%
    split_pivot_join(measures, prefix = "measures_") %>%
    split_pivot_join(trial_resn, prefix = "trial_resn_") %>%
    split_pivot_join(bar_new, prefix = "bar_new_") %>%
    split_pivot_join(sol_appr, prefix = "sol_appr_") %>%
    split_pivot_join(req_try_new, prefix = "req_try_new_") %>%
    split_pivot_join(lead_training, prefix = "lead_training_") %>%
    split_pivot_join(repo_where, prefix = "repo_where_") %>%
    split_pivot_join(repo_how, prefix = "repo_how_") %>%
    split_pivot_join(sat_fdbck, prefix = "sat_fdbck_") %>%
    split_pivot_join(receiving_CH, prefix = "receiving_CH_") %>%
    split_pivot_join(receiving_CH1, prefix = "receiving_CH1_") %>%
    split_pivot_join(advice, prefix = "advice_") %>%
    split_pivot_join(prot_area, prefix = "prot_area_") %>%
    split_pivot_join(cons_org, prefix = "cons_org_")%>%
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
    ))%>%
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
      ))%>%
    mutate(survey_id = row_number())

####################### Write data to global environment #######################
  biodata <- list("dc" = d_clean)
  list2env(biodata, .GlobalEnv)
}
