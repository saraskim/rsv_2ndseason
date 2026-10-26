# -----------------------------------------------------------------------------
# 
# Function for making clone dataset
#
# In the function, must define: 
# data: long dataset that incorporates weights - output from make_weights function
#
#
# Can access fully stacked cloned dataset using clone_data$clone_data_stacked
# Can access subsets of non-zero weight datasets using:
#    clone_data$mab_data
#    clone_data$mab_data_ideal
#    clone_data$pref_data
#    clone_data$control_data
#
# -----------------------------------------------------------------------------

here::i_am("code/3_clone_function.R")

make_clone_dataset <- function(data) {
  
  mab_data <- data %>%
    filter(wt_mab != 0) %>%
    mutate(clone = "mab", wt = wt_mab) %>%
    arrange(NEWBORN_ID, week)
  
  mab_data_ideal <- data %>%
    filter(wt_mab_ideal != 0) %>%
    mutate(clone = "mab_ideal", wt = wt_mab_ideal) %>%
    arrange(NEWBORN_ID, week)
  
  pref_data <- data %>%
    filter(wt_pref != 0) %>%
    mutate(clone = "pref", wt = wt_pref) %>%
    arrange(NEWBORN_ID, week)
  
  control_data <- data %>%
    filter(wt_cntrl != 0) %>%
    mutate(clone = "control", wt = wt_cntrl) %>%
    arrange(NEWBORN_ID, week)
  
  clone_data_stacked <- rbind(mab_data, mab_data_ideal, pref_data, control_data)
  
  return(list(mab_data = mab_data, mab_data_ideal=mab_data_ideal, pref_data = pref_data, 
              control_data = control_data, clone_data_stacked = clone_data_stacked))
}


clone_data <- make_clone_dataset(data = data_weekly_weights)

