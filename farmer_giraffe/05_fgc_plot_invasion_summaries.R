# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino, Gemini AI
# Date: 10/26/2026

# Function purpose: Generate modular three-panel composite bar charts with 
# customizable fill colors, labels, and grouping variables.

#################################### Intro #####################################

# Function name: fgc_create_composite_figure
# Description: Creates a stacked 3-panel figure using ggplot2 and patchwork.
#              Panel 1: "All" overall summary horizontal bar.
#              Panel 2: Village-level response breakdown bar chart.
#              Panel 3: Categorical grouping breakdown (e.g., protection measures or crops)

################################# Arguments ####################################

# df:
#        Data frame containing the survey response variables.
# group_var:
#        String name of the categorical variable to stack/fill by (e.g., "invasion" or "invasion_frq").
# measure_cols:
#        Named character vector or string vector mapping binary count columns to clean display labels
#        for Panel 3 (e.g., c("measures_fence" = "Fence", "measures_guard" = "Human guard")).
# fill_colors:
#        Named character vector mapping fill levels to hex/color strings.
# fill_labels:
#        Named or un-named vector specifying legend display names for fill levels.
# village_labels:
#        Named vector mapping raw village names to formatted display names.
# legend_title:
#        String for the fill legend title in Panel 3 (defaults to "").
# p3_x_label:
#        Label for the X-axis of Panel 3 (defaults to "Protection Measure").
# p3_y_label:
#        Label for the Y-axis of Panel 3 (defaults to "Count of Respondents").
# panel_heights:
#        Numeric vector of length 3 setting relative panel height proportions (defaults to c(1, 10, 6)).


################################## Output ######################################

# composite_plot - A combined patchwork object containing Panel 1 (Top), 
#                  Panel 2 (Middle), and Panel 3 (Bottom).

################################# Function #####################################
fgc_create_composite_figure <- function(df,
                                    group_var,
                                    measure_cols,
                                    fill_colors,
                                    fill_labels = NULL,
                                    village_labels = NULL,
                                    legend_title = "",
                                    p3_x_label = "Protection Measure",
                                    p3_y_label = "Count of Respondents",
                                    panel_heights = c(1, 10, 6)) {

  group_sym <- sym(group_var)
  
  # Standardized shared theme settings
  shared_theme <- theme_bw(base_size = 12, base_family = "") + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
      axis.text = element_text(face = "bold", size = 14, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      legend.position = "none",
      plot.margin = margin(r = 20)
    )
  
  # Set default fill labels if not supplied
  if (is.null(fill_labels)) {
    fill_labels <- names(fill_colors)
  }
  
  # Default village label mapping
  if (is.null(village_labels)) {
    village_labels <- c(
      "shabah" = "Shabah", "sankuri" = "Sankuri", "raya" = "Raya",
      "korakora" = "Korakora", "jarirot" = "Jarirot", "bula_salama" = "Bula Salama",
      "bula_gem" = "Bula Gem", "bula_bakari" = "Bula Bakari",
      "bula_adc" = "Bula Adc", "bour_algy" = "Bour Algy", "awarot" = "Awarot"
    )
  }
  
  # --- Panel 1: Overall Summary ---
  p1 <- df %>%
    drop_na(!!group_sym) %>%
    group_by(!!group_sym) %>%
    summarise(tot = n(), .groups = "drop") %>%
    mutate(All = "All") %>%
    ggplot(aes(x = All, y = tot, fill = !!group_sym)) +
    geom_bar(position = "fill", stat = "identity") +
    labs(x = "", y = "") +
    scale_fill_manual(legend_title, values = fill_colors, labels = fill_labels) +
    coord_flip() +
    shared_theme
  
  # --- Panel 2: Village Breakdown ---
  p2 <- df %>%
    drop_na(!!group_sym) %>%
    group_by(village, !!group_sym) %>%
    summarise(tot_by_village = n(), .groups = "drop") %>%
    ggplot(aes(x = village, y = tot_by_village, fill = !!group_sym)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(legend_title, values = fill_colors, labels = fill_labels) +
    scale_x_discrete(labels = village_labels) +
    labs(x = "Village", y = "") +
    coord_flip() +
    shared_theme
  
  # --- Panel 3: Categorical / Measures Breakdown ---
  p3_data <- df %>%
    drop_na(!!group_sym) %>%
    group_by(!!group_sym) %>%
    summarise(across(all_of(names(measure_cols)), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(cols = all_of(names(measure_cols)), names_to = "measure", values_to = "count")
  
  p3 <- ggplot(p3_data, aes(x = measure, y = count, fill = !!group_sym)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(name = legend_title, values = fill_colors, labels = fill_labels) +
    scale_x_discrete(labels = measure_cols) +
    labs(x = p3_x_label, y = p3_y_label) +
    coord_flip() +
    shared_theme +
    theme(legend.position = "bottom")
  
  # --- Combine Panels into Final Layout ---
  composite_plot <- p1 + p2 + p3 + plot_layout(ncol = 1, heights = panel_heights)
  
####################### Write data to global environment #######################
  data <- list("composite_plot" = composite_plot)
  list2env(data, .GlobalEnv)
  
}