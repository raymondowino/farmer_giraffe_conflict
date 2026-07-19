# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino
# Date: 04/16/2025

# Purpose: Plotting for multiple binary columns with and without grouping/faceting

################################################################################
################################## Function 1 ##################################
# Function name: fgc_plot_prop_multi
# Description: Plot counts of respondents by multiple binary variables
# (e.g., crops_*), without grouping or faceting

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
  
  # Save to file
  png(paste0("./summary_plots/", filename, ".png"), res = 300, width = 6500, height = 2800)
  plot(plot_prop)
  dev.off()
  
  # Export summary table
  biodata <- list("d2" = d2)
  list2env(biodata, .GlobalEnv)
  
  return(plot_prop)
}

################################################################################
################################## Function 2 ##################################
# Function name: rel_var_with_multcol
# Description: Plot counts by multiple binary columns + optional fill + facet
# - Supports both single or multiple binary fill columns

rel_var_with_multcol <- function(dc, columns, fillvar = NULL, facetvar = NULL,
                                 xlab, title, filename) {
  
  numeric_cols <- columns[sapply(dc[, columns], is.numeric)]
  if (length(numeric_cols) == 0) {
    stop("No numeric columns found among the selected columns.")
  }
  
  if (!is.null(fillvar)) {
    # Handle fillvar as character names or regex
    if (length(fillvar) == 1 && grepl("^\\^", fillvar)) {
      fill_cols <- names(dc)[grepl(fillvar, names(dc))]
    } else {
      fill_cols <- fillvar
    }
    fill_cols <- fill_cols[sapply(dc[, fill_cols], is.numeric)]
  } else {
    fill_cols <- NULL
  }
  
  # Build base data
  base_data <- dc %>% select(all_of(c(numeric_cols, fill_cols, facetvar)))
  
  # Pivot both main and fill binary columns
  if (!is.null(fill_cols) && length(fill_cols) > 1) {
    long_df <- base_data %>%
      pivot_longer(cols = all_of(numeric_cols), names_to = "xvar", values_to = "xval") %>%
      pivot_longer(cols = all_of(fill_cols), names_to = "fillvar", values_to = "fillval") %>%
      filter(xval == 1, fillval == 1) %>%
      mutate(xvar = sub("^.*?_", "", xvar),
             fillvar = sub("^.*?_", "", fillvar))
    
    d2 <- long_df %>%
      group_by(xvar, fillvar, across(all_of(facetvar))) %>%
      summarise(count = n(), .groups = "drop")
    
    plot <- ggplot(d2, aes(x = reorder(xvar, -count), y = count, fill = fillvar)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab(xlab) +
      ylab("Count") +
      labs(title = title, fill = "Measure") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else {
    # Single fill or none
    long_df <- base_data %>%
      pivot_longer(cols = all_of(numeric_cols), names_to = "xvar", values_to = "xval") %>%
      filter(xval == 1) %>%
      mutate(xvar = sub("^.*?_", "", xvar))
    
    if (!is.null(fill_cols) && length(fill_cols) == 1) {
      fill_name <- fill_cols[1]
      long_df <- long_df %>%
        mutate(fillval = .data[[fill_name]])
      
      d2 <- long_df %>%
        filter(fillval == 1) %>%
        group_by(xvar, .data[[fill_name]], across(all_of(facetvar))) %>%
        summarise(count = n(), .groups = "drop")
      
      plot <- ggplot(d2, aes(x = reorder(xvar, -count), y = count,
                             fill = .data[[fill_name]])) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(fill = fill_name)
      
    } else {
      d2 <- long_df %>%
        group_by(xvar, across(all_of(facetvar))) %>%
        summarise(count = n(), .groups = "drop")
      
      plot <- ggplot(d2, aes(x = reorder(xvar, -count), y = count)) +
        geom_bar(stat = "identity")
    }
    
    plot <- plot +
      xlab(xlab) +
      ylab("Count") +
      labs(title = title) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Facet if needed
  if (!is.null(facetvar)) {
    if (length(facetvar) == 1) {
      plot <- plot + facet_wrap(vars(.data[[facetvar]]))
    } else if (length(facetvar) == 2) {
      plot <- plot + facet_grid(rows = vars(.data[[facetvar[1]]]), cols = vars(.data[[facetvar[2]]]))
    }
  }
  
  # Save to file
  png(paste0("./summary_plots/", filename, ".png"), res = 300, width = 6500, height = 2800)
  print(plot)
  dev.off()
  
  # Export summary table
  biodata <- list("d2" = d2)
  list2env(biodata, .GlobalEnv)
  
  return(plot)
}
