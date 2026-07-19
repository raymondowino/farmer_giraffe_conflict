# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino
# Date: 04/16/2025

# Function purpose: plot data

#################################### Intro #####################################

# Function name: fgc_plot_prop
# Description:  function to plot proportions of responses by different variables
# for the farmer giraffe conflict (FGC) project

################################# Arguments ####################################

# Labels:
#       A list of labels for the plot
# Data:
#       Data frame containing data
# Variable:
#       The name of the variable on the x axis
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
fgc_plot_prop <-function(dc, variable, xlab, title, filename){ 
 
# Create summarized data frame 
d1 <-dc %>%
    group_by({{variable}}) %>%
    mutate(count = n())%>%
    select({{variable}}, count)%>%
    distinct()

# Create plot
plot_prop <- ggplot(d1, aes(x = {{variable}}, y = count)) +
    geom_bar(stat = "identity") +
    xlab(xlab) +
    ylab("Count") +
    labs(title = title)+
    theme_light()
  
############################# Write plot to  file###############################
png(paste("./summary_plots/", filename,".png", sep =''), res = 300, width = 6500, height = 2800)
plot(plot_prop)
dev.off()

biodata <- list("d1" = d1)
list2env(biodata, .GlobalEnv)
plot_prop
}
