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
fgc_plot_prop <-function(dc, variable){ 
  
#dc$gender <-as.factor(dc$gender)
  
d1 <-dc %>%
    group_by({{variable}}) %>%
    mutate(count = n())%>%
    select({{variable}}, count)%>%
    distinct()

# alternative
plot_prop <- function(data, variable) {
  data %>%
    count({{ variable }}, name = "count") %>%
    ggplot(aes(x = {{ variable }}, y = count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_light() +
    labs(title = paste("Distribution of", deparse(substitute(variable))),
         x = deparse(substitute(variable)),
         y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# aes string in ggplot
 plot_prop <- ggplot(as.data.frame(d1), aes_string(x = variable, y = count)) +
   geom_bar(stat = "identity") +
   theme_light()
 
 # Alternative ploting 
 # This takes one variable at a time
 plot_prop(dc, education)
 plot_prop(dc, village)
 plot_prop(dc,tenure)
 plot_prop(dc,age)
 
 # All at once
 library(tidyverse)
 
 plot_all_categoricals <- function(data) {
   data %>%
     select(where(~ is.character(.x) || is.factor(.x))) %>%
     keep(~ n_distinct(.x, na.rm = TRUE) < 50) %>%
     imap(~ {
       df <- count(data, !!sym(.y))
       ggplot(df, aes_string(x = .y, y = "n")) +
         geom_bar(stat = "identity", fill = "steelblue") +
         labs(title = paste("Distribution of", .y), x = .y, y = "Count") +
         theme_light() +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
     })
 }
 
 # Example usage
 plots <- plot_all_categoricals(dc)
 walk(plots, print)
 
 # Export to PDF
 pdf("categorical_plots.pdf", width = 10, height = 6)
 walk(plots, print)
 dev.off()
 
 # plot_prop <- ggplot(d1, aes_string(x = gender, y = count)) +
 #   geom_bar(stat = "identity") +
 #   theme_light()

# ggplot(d1, aes(x = education, y = count)) +
#     geom_bar(stat = "identity") +
#     theme_light()

############################# Write plot to  file###############################
# png(paste("./summary_plots/", filename,".png", sep =''), res = 300, width = 6500, height = 2800)
# plot(plot_prop)
# dev.off()
# plot_prop
biodata <- list("d1" = d1)
list2env(biodata, .GlobalEnv)
plot_prop
}
