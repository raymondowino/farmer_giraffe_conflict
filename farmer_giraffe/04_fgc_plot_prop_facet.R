# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino
# Date: 04/16/2025

# Function purpose: plot data with multiple variables

#################################### Intro #####################################

# Function name: fgc_plot_prop_facect
# Description:  function to plot proportions of responses by multiple variables
# for the farmer giraffe conflict (FGC) project

################################# Arguments ####################################

# Labels:
#       A list of labels for the plot
# Data:
#       Data frame containing data
# Variable1:
#       The name of the variable on the x axis
# Facet_variable:
#       The name of the variable used to facet the data
# Xlab:
#       Title for the x axis
# Title:
#       Title for the graph
# Filename:
#       The name for the image file

################################## Output ######################################

# What is the output of the function?
# Draft graph image written to file

################################# Function #####################################
# Function
fgc_plot_prop_facet <-function(dc, variable1, facet_variable, xlab, title){ 
  
  # Create summarized data frame 
  d2 <-dc %>%
    group_by({{variable1}}) %>%
    mutate(count = n())%>%
    select({{variable1}}, count)%>%
    distinct()
  
  # Create plot
  plot_prop_f <- ggplot(d1, aes(x = {{variable1}}, y = count)) +
    geom_bar(stat = "identity") +
    xlab(xlab) +
    ylab("Count") +
    labs(title = title)+
    facet_grid(.~{{facet_variable}})+
    theme_light()
  
  ############################# Write plot to  file###############################
  # png(paste("./summary_plots/", filename,".png", sep =''), res = 300, width = 6500, height = 2800)
  # plot(plot_prop)
  # dev.off()
  
  biodata <- list("d2" = d2)
  list2env(biodata, .GlobalEnv)
  plot_prop_f
}