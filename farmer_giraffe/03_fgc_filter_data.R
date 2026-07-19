# Farmer/Giraffe conflict
# Author: Jessie Golding, Raymond Owino
# Date: 09/17/2025

# Function purpose: filter data for specific questions

#################################### Intro #####################################

# Function name: fgc_filter_data
# Description:  function to filter data to answer different research questions from
# survey data on the farmer giraffe conflict (FGC) project. The full data set has
# over 200 columns, so this function is to reduce that to a managable amount of 
# data/columns when answering individual questions

################################# Arguments ####################################

# dc:
#       Cleanned data tibble.
# qnum:
#       Question number (for naming data frame).
# variables:
#       The variables to select for the question

################################## Output ######################################

# df_q_# - a tibble saved to global environment

################################# Function #####################################
fgc_filter_data <- function(d, qnum, variables) {
  
   d2<-dc %>%
    select({{variables}})
   
   new_df_name <-paste0("dc_q_", qnum)

####################### Write data to global environment #######################
   assign(new_df_name, d2, envir = .GlobalEnv)
}