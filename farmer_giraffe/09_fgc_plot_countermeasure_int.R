# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino, Gemini AI
# Date: 11/02/2026

# Function purpose: Generate plot to show interest in trying new countermeasure
# reasoning by level of interest.

#################################### Intro #####################################

# Function name: fgc_plot_trial_reasons_by_interest
# Description: Summarizes survey willingness reasons across interest categories (`try_new`),
#              reorders factor levels so '#1f6f6f' (Extremely interested) plots first on the left,
#              and adds a bottom legend ordered from left-to-right as (Slightly -> Moderately -> Extremely)
#              with plain text (non-bold) legend labels.

################################# Arguments ####################################

# dc:
#        Main survey dataframe containing 'try_new', 'new_mtd_clean', and trial_resn_* columns.


################################## Output ######################################

# p: A ggplot2 object ready for display or ggsave export.
# Writes 'p_reasons_interest' to the global environment.

################################# Function #####################################
fgc_plot_trial_reasons_by_interest <- function(dc) {

  # 1. Data Processing
  plot_data <- dc %>%
    filter(!is.na(try_new) & try_new != "") %>%
    mutate(new_mtd_clean = if_else(is.na(new_mtd_clean), "none", new_mtd_clean)) %>%
    group_by(try_new, new_mtd_clean) %>%
    summarise(
      Neighbor_uses = sum(trial_resn_neighbour_uses, na.rm = TRUE),
      Kws_use       = sum(trial_resn_kws_use, na.rm = TRUE),
      Available     = sum(trial_resn_available, na.rm = TRUE),
      Easy          = sum(trial_resn_easy_use, na.rm = TRUE),
      Cheap         = sum(trial_resn_cheap, na.rm = TRUE),
      .groups       = "drop"
    ) %>%
    pivot_longer(
      cols = c(Neighbor_uses, Kws_use, Available, Easy, Cheap),
      names_to = "reason",
      values_to = "count"
    ) %>%
    mutate(
      reason = recode(
        reason,
        Neighbor_uses = "Used by neighbor",
        Kws_use       = "Used by Kenya \nWildlife Service uses",
        Available     = "Available",
        Easy          = "Easy",
        Cheap         = "Cheap"
      )
    )
  
  # 2. Factor Level Ordering
  # Stack order: extreme_interest (#1f6f6f) renders first (leftmost)
  interest_levels <- c("extreme_interest", "moderate_interest", "slight_interest")
  
  plot_data <- plot_data %>%
    filter(try_new %in% interest_levels) %>%
    mutate(try_new = factor(try_new, levels = interest_levels))
  
  # 3. Plot Construction
  p <- ggplot(plot_data, aes(x = reason, y = count, fill = try_new)) +
    geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
    scale_fill_manual(
      name = NULL,
      values = c(
        "extreme_interest"  = "#1f6f6f",
        "moderate_interest" = "#a00000",
        "slight_interest"   = "#b8b8b8"
      ),
      labels = c(
        "extreme_interest"  = "Extremely interested",
        "moderate_interest" = "Moderately interested",
        "slight_interest"   = "Slightly interested"
      ),
      # reverse = TRUE inside guide_legend flips the legend items left-to-right
      # without altering the bar stack fill order
      guide = guide_legend(title = NULL, reverse = TRUE)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
    labs(
      x = "Reason",
      y = "Proportion of respondents"
    ) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 3),
      axis.text        = element_text(face = "bold", size = 14, color = "black"),
      axis.title       = element_text(size = 14, face = "bold"),
      legend.position  = "bottom",
      legend.text      = element_text(size = 12, face = "plain"), # Set to plain (not bold)
      plot.margin      = margin(r = 20)
    )
  
####################### Write data to global environment #######################
  assign("p_reasons_interest", p, envir = .GlobalEnv)
  
  return(p)
}