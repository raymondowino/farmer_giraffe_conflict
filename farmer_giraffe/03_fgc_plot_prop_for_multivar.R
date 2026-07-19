# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino
# Date: 04/16/2025

# Function purpose: plot data for multiple binary columns (e.g. crop_*, livestock_*)

#################################### Intro #####################################

# Function name: fgc_plot_prop_multi
# Description: function to plot counts of respondents by multiple binary variables
# (e.g., crops) stored as separate columns with common prefixes.

################################# Arguments ####################################

# dc:
#       Data frame containing data
# columns:
#       Character vector of column names to summarize and plot
# xlab:
#       Label for the x axis
# title:
#       Title for the plot
# filename:
#       The name for the output image file (without extension)

################################## Output ######################################

# Draft graph image written to PNG file in ./summary_plots/
# Summary table exported to global environment as d2

################################# Function #####################################

fgc_plot_prop_multi <- function(dc, columns, xlab, title, filename){
  
  # Create summarized data frame
  d2 <- dc %>%
    select(all_of(columns)) %>%
    summarise(across(everything(), ~sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "count") %>%
    mutate(variable = sub("^.*?_", "", variable))  # Remove prefix before plotting
  
  # Create plot
  plot_prop <- ggplot(d2, aes(x = reorder(variable, -count), y = count)) +
    geom_bar(stat = "identity") +
    xlab(xlab) +
    ylab("Count") +
    labs(title = title) +
    theme_light()
  
  ############################# Write plot to file ###############################
  png(paste0("./summary_plots/", filename, ".png"), res = 300, width = 6500, height = 2800)
  plot(plot_prop)
  dev.off()
  
  # Export summary table to global environment
  biodata <- list("d2" = d2)
  list2env(biodata, .GlobalEnv)
  
  # Return plot object (optional)
  return(plot_prop)
}

