##########################################################################################
##### Project: Efficacy of tDCS in modulating neurocognitive markers of OCD          #####
##### Author: Luisa Balzus                                                           #####
##### Date: 02.11.2020                                                               #####
##### Usage: Script for preparing the data for statistical analyses & plotting       #####
##### Input: Raw behavioral data (logfiles) and preprocessed single-trial EEG data   #####
##### Output: Cleaned dataframe that can be used for statistical analyses & plotting #####
##########################################################################################

# Clear environment
rm(list = ls())


# Load packages
library(dplyr)


# Turn off scientific notation
options(scipen = 999)


# Define paths
path_behavioral_data <- "C:/Users/Luisa/PhD/1_PhD_Project/7_ModERN_Stimulation_Study/4_Behavioral_Data/Flanker/"   
path_ERP_data        <- "C:/Users/Luisa/PhD/1_PhD_Project/7_ModERN_Stimulation_Study/8_Analyses/1_Flanker_Analysis_with_EEGLAB/4_Single_Trial_ERPs/single_trial_MFN_0_100_FCz_with_events.csv"
path_cleaned_data    <- "C:/Users/Luisa/PhD/1_PhD_Project/7_ModERN_Stimulation_Study/8_Analyses/1_Flanker_Analysis_with_EEGLAB/B_Statistical_Analyses/OCD_tDCS/data/"



###################   Load Behavioral Data and ERP Data   ####################

# Create empty data frames to write and single-trial behavioral data in it
behavioral_data <- data.frame()   


# Create list of behavioal data files that have to be loaded
logfiles <- list.files(path_behavioral_data, pattern = ".txt")       


# Load behavioral data and concatenate data of all participants
for (participant in logfiles){                                                                                    
  one_participant <- read.table(paste0(path_behavioral_data, participant), skip = 28, fill = TRUE, nrows = 480, header = TRUE, stringsAsFactors = FALSE)
  behavioral_data <- rbind(behavioral_data, one_participant)
}  


# Load ERP data
ERP_data <- read.csv(path_ERP_data, header = TRUE, stringsAsFactors = FALSE)



####################   Merge Behavioral and ERP Data   ####################   

# In behavioral data, delete string 'Flanker_' from participant ID to have identical IDs in behavioral and ERP data for merging
behavioral_data$name <- gsub('Flanker_', '', behavioral_data$name)


# Fix missing trials (no trigger sent, e.g. due to RT = 1 ms triggers may overlap and only one is sent) in ERP data (increment trial nummer after missing trial by number of missing trials to enable matching)
ERP_data[ERP_data$participant_id == 'P_17_T1' & ERP_data$trial >= 270,]$trial <- ERP_data[ERP_data$participant_id == 'P_17_T1' & ERP_data$trial >= 270,]$trial + 1 # in P_17_T1 trial 270 is missing 

# Notes on missing trigger detection procedure
# 1) Identify approximate location of missing trigger by inspecting the single_trial_data after merging (see next step) - after these triggers, no ERP is imported but NA is inserted for this participant
# 2) Inspect eventlist_after_AR file in the participant's preprocessing folder (..\1_Flanker_Analysis_with_EEGLAB\0_Preprocessing_Files_Each_Subject)
# 3) If everything is fine, only very few rows of the ERP column in the subsequently created single_trial_data table should contain NA (NaN = artifact; NA = no eeg data present)


# Merge behavioral and ERP data (rows in behavioral_data with no match in ERP_data will have NAs in the ERP column)
single_trial_data <- left_join(behavioral_data, ERP_data, by = c('name' = 'participant_id', 'trial' = 'trial', 'resp1' = 'event'))





####################   Create Relevant Variables and Clean Data   ####################

# Add column for group, session, invalid rt, and stimulus type and response type
single_trial_data <- single_trial_data %>%  
  dplyr::mutate(group             = ifelse(substr(name, 1, 1) == "C", "HC", "OCD"),
                session           = ifelse(substr(name, 6, 7) == "T1", "T1", "T2"),
                rt_log            = log(rt1),    
                rt_invalid        = ifelse(resp1 < 71 & (rt1 < 100 | rt1 > 800), TRUE, FALSE),                         
                stimulus_type     = ifelse((resp1 == 11 | resp1 == 21 | resp1 == 71), "congruent", "incongruent"),
                response_type     = ifelse((resp1 == 11 | resp1 == 12), "correct", 
                                         ifelse((resp1 == 21 | resp1 == 22), "incorrect", "miss")),
                response_type_2nd = ifelse((resp2 == 31 | resp2 == 32), "correct", 
                                         ifelse((resp2 == 41 | resp2 == 42), "incorrect", NA))) 


# Replace RT of 0 for misses or non-existent second responses with NA (also in columns rt_log and rt_invalid)
single_trial_data$rt1[single_trial_data$response_type == "miss"]        <- NA 
single_trial_data$rt_log[single_trial_data$response_type == "miss"]     <- NA 
single_trial_data$rt_invalid[single_trial_data$response_type == "miss"] <- NA 
single_trial_data$rt2[is.na(single_trial_data$response_type_2nd)]       <- NA 


# Add stimulation condition variable 
single_trial_data$stimulation <- NA
single_trial_data[single_trial_data$name =="C_01_T1"| single_trial_data$name =="C_02_T1"| single_trial_data$name =="C_03_T2"| single_trial_data$name =="C_04_T2"| single_trial_data$name =="C_05_T1"| single_trial_data$name =="C_06_T2"| single_trial_data$name =="C_07_T1"| single_trial_data$name =="C_08_T1"| single_trial_data$name =="C_09_T2"| single_trial_data$name =="C_10_T2"| single_trial_data$name =="C_11_T2"| single_trial_data$name =="C_12_T2"| single_trial_data$name =="C_13_T1"| single_trial_data$name =="C_15_T2"| single_trial_data$name =="C_16_T1"| single_trial_data$name =="C_17_T2"| single_trial_data$name =="C_18_T2"| single_trial_data$name =="C_19_T1"| single_trial_data$name =="C_20_T1"| single_trial_data$name =="C_21_T2"| single_trial_data$name =="C_22_T2"| single_trial_data$name =="C_23_T1"| single_trial_data$name =="C_24_T1"| single_trial_data$name =="C_25_T1"| single_trial_data$name =="C_26_T1"| single_trial_data$name =="C_27_T2"| single_trial_data$name =="C_28_T2"| single_trial_data$name =="C_29_T1"| single_trial_data$name =="C_30_T2"| single_trial_data$name =="P_01_T1"| single_trial_data$name =="P_02_T2"| single_trial_data$name =="P_03_T2"| single_trial_data$name =="P_04_T1"| single_trial_data$name =="P_05_T1"| single_trial_data$name =="P_06_T1"| single_trial_data$name =="P_07_T2"| single_trial_data$name =="P_08_T2"| single_trial_data$name =="P_09_T2"| single_trial_data$name =="P_10_T1"| single_trial_data$name =="P_11_T2"| single_trial_data$name =="P_12_T2"| single_trial_data$name =="P_13_T1"| single_trial_data$name =="P_15_T1"| single_trial_data$name =="P_16_T1"| single_trial_data$name =="P_17_T2"| single_trial_data$name =="P_18_T1"| single_trial_data$name =="P_19_T1"| single_trial_data$name =="P_20_T2"| single_trial_data$name =="P_21_T2"| single_trial_data$name =="P_22_T1"| single_trial_data$name =="P_23_T1"| single_trial_data$name =="P_24_T2"| single_trial_data$name =="P_25_T1"| single_trial_data$name =="P_26_T2"| single_trial_data$name =="P_27_T1"| single_trial_data$name =="P_28_T2"| single_trial_data$name =="P_29_T2" | single_trial_data$name =="P_30_T1",]$stimulation <- "sham"
single_trial_data[single_trial_data$name =="C_01_T2"| single_trial_data$name =="C_02_T2"| single_trial_data$name =="C_03_T1"| single_trial_data$name =="C_04_T1"| single_trial_data$name =="C_05_T2"| single_trial_data$name =="C_06_T1"| single_trial_data$name =="C_07_T2"| single_trial_data$name =="C_08_T2"| single_trial_data$name =="C_09_T1"| single_trial_data$name =="C_10_T1"| single_trial_data$name =="C_11_T1"| single_trial_data$name =="C_12_T1"| single_trial_data$name =="C_13_T2"| single_trial_data$name =="C_15_T1"| single_trial_data$name =="C_16_T2"| single_trial_data$name =="C_17_T1"| single_trial_data$name =="C_18_T1"| single_trial_data$name =="C_19_T2"| single_trial_data$name =="C_20_T2"| single_trial_data$name =="C_21_T1"| single_trial_data$name =="C_22_T1"| single_trial_data$name =="C_23_T2"| single_trial_data$name =="C_24_T2"| single_trial_data$name =="C_25_T2"| single_trial_data$name =="C_26_T2"| single_trial_data$name =="C_27_T1"| single_trial_data$name =="C_28_T1"| single_trial_data$name =="C_29_T2"| single_trial_data$name =="C_30_T1"| single_trial_data$name =="P_01_T2"| single_trial_data$name =="P_02_T1"| single_trial_data$name =="P_03_T1"| single_trial_data$name =="P_04_T2"| single_trial_data$name =="P_05_T2"| single_trial_data$name =="P_06_T2"| single_trial_data$name =="P_07_T1"| single_trial_data$name =="P_08_T1"| single_trial_data$name =="P_09_T1"| single_trial_data$name =="P_10_T2"| single_trial_data$name =="P_11_T1"| single_trial_data$name =="P_12_T1"| single_trial_data$name =="P_13_T2"| single_trial_data$name =="P_15_T2"| single_trial_data$name =="P_16_T2"| single_trial_data$name =="P_17_T1"| single_trial_data$name =="P_18_T2"| single_trial_data$name =="P_19_T2"| single_trial_data$name =="P_20_T1"| single_trial_data$name =="P_21_T1"| single_trial_data$name =="P_22_T2"| single_trial_data$name =="P_23_T2"| single_trial_data$name =="P_24_T1"| single_trial_data$name =="P_25_T2"| single_trial_data$name =="P_26_T1"| single_trial_data$name =="P_27_T2"| single_trial_data$name =="P_28_T1"| single_trial_data$name =="P_29_T1" | single_trial_data$name =="P_30_T2",]$stimulation <- "verum"


#############
# Exclude P_02 (had retainer) and C_02???
# single_trial_data <- single_trial_data[single_trial_data$name != "C_02_T1" & single_trial_data$name != "C_02_T2" & single_trial_data$name != "P_02_T1" & single_trial_data$name != "P_02_T2",]
#############


# Rename columns and remove string '_T1/T2' from participant ID (to get correct number of factor levels later)
single_trial_data <- single_trial_data %>% 
  subset(select = c("name", "group", "session", "stimulation", "trial", "stimulus_type", "response_type", "rt1", "rt_log", "rt_invalid" ,"response_type_2nd", "rt2", "MFN_0_100_FCz")) %>%
  dplyr::rename(participant_id     = name,
                rt                 = rt1,
                rt_2nd             = rt2) %>% 
  dplyr::mutate(participant_id = substr(participant_id, 1, 4))


###################   Save Data   ####################

save(single_trial_data, file = paste0(path_cleaned_data, "Single_Trial_Data.rda"))
