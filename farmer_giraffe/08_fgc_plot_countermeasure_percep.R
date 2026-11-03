# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino, Gemini AI
# Date: 11/02/2026

# Function purpose: Generate publication-ready Table 3 and Table 4 with exact numerical parity.

#################################### Intro #####################################

# Function name: fgc_plot_countermeasure_effectiveness
# Description: Summarizes effectiveness ratings per countermeasure from dc_long, 
#              calculates diverging percentages (Ineffective flipped to negative y-axis), 
#              and outputs a custom styled ggplot object.

################################# Arguments ####################################

# dc_long:
#        Recoded long dataset containing 'measure' and 'effectiveness_grouped' columns.

################################## Output ######################################

# p: A ggplot2 object ready for display or ggsave export.
# Writes 'p_effectiveness' to the global environment.

################################# Function #####################################
fgc_plot_countermeasure_effectiveness <- function(dc_long) {

  
  # 1. Data Processing
  plot_data <- dc_long %>%
    filter(!is.na(measure) & !is.na(effectiveness_grouped)) %>%
    group_by(measure, effectiveness_grouped) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(measure) %>%
    mutate(
      total = sum(count),
      percent = count / total * 100
    ) %>%
    ungroup() %>%
    mutate(
      percent_plot = case_when(
        effectiveness_grouped == "Ineffective" ~ -percent,
        TRUE ~ percent
      ),
      label = paste0("n=", count)
    )
  
  # 2. Factor Level Ordering
  measure_order <- plot_data %>%
    distinct(measure, total) %>%
    arrange(desc(total)) %>%
    pull(measure)
  
  plot_data$measure <- factor(plot_data$measure, levels = rev(measure_order))
  plot_data$effectiveness_grouped <- factor(
    plot_data$effectiveness_grouped,
    levels = c("Ineffective", "Neutral", "Effective")
  )
  
  # 3. Plot Construction (Exact Original Formatting Preserved)
  p <- ggplot(plot_data, aes(x = measure, y = percent_plot, fill = effectiveness_grouped)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.99) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 3, color = "black"
    ) +
    coord_flip() +
    scale_fill_manual(
      values = c(
        "Ineffective" = "#E69F00",
        "Neutral"     = "#999999",
        "Effective"   = "#56B4E9"
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
  
####################### Write data to global environment #######################
  assign("p_effectiveness", p, envir = .GlobalEnv)
  
  return(p)
}