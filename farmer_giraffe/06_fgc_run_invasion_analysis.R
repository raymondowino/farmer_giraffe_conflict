# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino, Gemini AI
# Date: 10/26/2026

# Function purpose: Fits Firth penalized logistic regression to crop invasion data,
#                   extracts odds ratios with profile likelihood CIs, and maps model 
#                   estimates onto observed 2x2 profile counts.


#################################### Intro ######################################

# Function name: run_giraffe_invasion_analysis
# Description: Accepts a crop invasion survey tibble (e.g., dc_invasion_crop),
#              formats binary predictors (mango presence and farm structural complexity),
#              fits a multivariable Firth penalized logistic regression model (invasion ~ mango + farm_type),
#              and constructs a publication-ready 2x2 contingency summary table with
#              observed invasion rates, adjusted Odds Ratios (aOR), 95% CIs, and p-values.

################################# Arguments #####################################

# data:
#        Data frame or tibble containing:
#        - invasion: Numeric or factor response (0/1, "Yes"/"No", etc.)
#        - crop_mangoes: Numeric indicator (0 = No mangoes, 1 = Growing mangoes)
#        - crop_total: Total number of crops grown (used to derive farm_type: 
#                      1 = Single Crop, >1 = Multiple Crop)

################################## Output #######################################

# final_table - A formatted tibble containing 4 farm profiles alongside raw sample 
#               ratios (n/N), observed invasion percentages (%), adjusted Odds 
#               Ratios (aOR), 95% confidence intervals (including linear-combination CI), 
#               and p-values.

################################# Function #####################################
fgc_run_giraffe_invasion_analysis <- function(data) {
  
  # 1. Format and robustly parse variables from data
  df_prepared <- data %>%
    mutate(
      # Robust binary conversion for invasion (handles 1/0, "Yes"/"No", TRUE/FALSE)
      invasion_clean = case_when(
        as.character(invasion) %in% c("1", "Yes", "yes", "TRUE", "true", "Invaded") ~ 1,
        as.character(invasion) %in% c("0", "No", "no", "FALSE", "false", "Not Invaded") ~ 0,
        TRUE ~ suppressWarnings(as.numeric(as.character(invasion)))
      ),
      
      # Binary factor for mango cultivation (0 = No, 1 = Yes)
      mango = factor(ifelse(crop_mangoes == 1, 1, 0), levels = c(0, 1)),
      
      # Farm structure: Multiple Crop (total > 1) vs. Single Crop (total == 1)
      farm_type = factor(ifelse(crop_total > 1, "Multiple", "Single"), 
                         levels = c("Multiple", "Single"))
    ) %>%
    # Filter out missing values in critical variables
    filter(!is.na(invasion_clean), !is.na(mango), !is.na(farm_type))
  
  # 2. Fit Firth Penalized Likelihood Logistic Regression
  fit_model <- logistf(
    invasion_clean ~ mango + farm_type, 
    data = df_prepared, 
    firth = TRUE
  )
  
  # Extract ORs, Profile Likelihood 95% CIs, and p-values for main effects
  model_coefs <- coef(fit_model)
  model_cis   <- confint(fit_model)
  model_p     <- fit_model$prob
  
  # Extract variance-covariance matrix for linear combinations
  V <- vcov(fit_model)
  
  # 3. Calculate observed 2x2 contingency breakdown across profiles
  profile_summary <- df_prepared %>%
    group_by(farm_type, mango) %>%
    summarise(
      Invaded = sum(invasion_clean == 1, na.rm = TRUE),
      Total   = n(),
      .groups = "drop"
    ) %>%
    mutate(
      Observed_Rate = paste0(round((Invaded / Total) * 100, 1), "%"),
      Sample_Ratio  = paste0(Invaded, " / ", Total)
    )
  
  # 4. Extract Main Effects
  # Mango Effect (Row 2)
  or_mango  <- exp(model_coefs["mango1"])
  ci_mango  <- exp(model_cis["mango1", ])
  p_mango   <- model_p["mango1"]
  
  # Farm Structure Effect (Row 3)
  or_single <- exp(model_coefs["farm_typeSingle"])
  ci_single <- exp(model_cis["farm_typeSingle", ])
  p_single  <- model_p["farm_typeSingle"]
  
  # 5. Compute Linear Combination & 95% CI for Row 4 (Single Crop | Growing Mangoes)
  log_or_comb  <- model_coefs["mango1"] + model_coefs["farm_typeSingle"]
  or_comb      <- exp(log_or_comb)
  
  # Standard Error of linear combination: sqrt(Var(A) + Var(B) + 2*Cov(A,B))
  se_comb      <- sqrt(V["mango1", "mango1"] + 
                         V["farm_typeSingle", "farm_typeSingle"] + 
                         2 * V["mango1", "farm_typeSingle"])
  
  ci_lower_comb <- exp(log_or_comb - 1.96 * se_comb)
  ci_upper_comb <- exp(log_or_comb + 1.96 * se_comb)
  ci_comb_str   <- paste0("[", round(ci_lower_comb, 2), ", ", round(ci_upper_comb, 2), "]")
  
  # 6. Build mapping for table output
  effect_map <- tibble(
    farm_type = factor(c("Multiple", "Multiple", "Single", "Single"), levels = c("Multiple", "Single")),
    mango     = factor(c(0, 1, 0, 1), levels = c(0, 1)),
    Profile   = c(
      "Multiple Crop | No Mangoes (Baseline)",
      "Multiple Crop | Growing Mangoes",
      "Single Crop | No Mangoes",
      "Single Crop | Growing Mangoes"
    ),
    aOR_val = c(1.00, or_mango, or_single, or_comb),
    CI_str  = c(
      "—", 
      paste0("[", round(ci_mango[1], 2), ", ", round(ci_mango[2], 2), "]"),
      paste0("[", round(ci_single[1], 3), ", ", round(ci_single[2], 2), "]"),
      ci_comb_str
    ),
    p_str   = c(
      "—",
      ifelse(p_mango < 0.001, "< 0.001", sprintf("%.3f", p_mango)),
      sprintf("%.3f", p_single),
      "—"
    )
  )
  
  # 7. Join sample counts with model parameters
  final_table <- effect_map %>%
    left_join(profile_summary, by = c("farm_type", "mango")) %>%
    mutate(
      `Adjusted Odds Ratio (aOR)` = sprintf("%.3f", aOR_val),
      `Adjusted Odds Ratio (aOR)` = ifelse(aOR_val == 1.00, "1.00 (Reference)", `Adjusted Odds Ratio (aOR)`)
    ) %>%
    select(
      `Farm Profile`                              = Profile,
      `No. Invaded / Total (n/N)`                 = Sample_Ratio,
      `Observed Invasion Rate`                    = Observed_Rate,
      `Adjusted Odds Ratio (aOR)`                 ,
      `95% Profile Confidence Interval`           = CI_str,
      `p-value`                                   = p_str
    )
  
  return(final_table)
}
  
 