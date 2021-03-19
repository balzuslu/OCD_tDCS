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
path_behavioral_data      <- "C:/Users/Luisa/PhD/1_PhD_Project/7_ModERN_Stimulation_Study/4_Behavioral_Data/Flanker/"   
path_ERP_data             <- "C:/Users/Luisa/PhD/1_PhD_Project/7_ModERN_Stimulation_Study/8_Analyses/1_Flanker_Analysis_with_EEGLAB/4_Single_Trial_ERPs/"
path_ERP_data_avg_ref     <- "C:/Users/Luisa/PhD/1_PhD_Project/7_ModERN_Stimulation_Study/8_Analyses/1_Flanker_Analysis_with_EEGLAB/4_Single_Trial_ERPs/additional_analysis_with_average_reference/"
path_ERP_data_stim_locked <- "C:/Users/Luisa/PhD/1_PhD_Project/7_ModERN_Stimulation_Study/8_Analyses/1_Flanker_Analysis_with_EEGLAB/4_Single_Trial_ERPs/additional_analysis_stimulus_locked/"
path_SCR_data             <- "C:/Users/Luisa/PhD/1_PhD_Project/7_ModERN_Stimulation_Study/8_Analyses/1_Flanker_Analysis_with_EEGLAB/6_SCR/3_Cleaned_SCR_Data/"
path_cleaned_data         <- "C:/Users/Luisa/PhD/1_PhD_Project/7_ModERN_Stimulation_Study/8_Analyses/1_Flanker_Analysis_with_EEGLAB/B_Statistical_Analyses/data/"



###################   Load Behavioral Data and ERP Data   ####################

# Create empty data frame to write and single-trial behavioral data and feedback infos in it
behavioral_data <- data.frame()   
feedback_infos  <- data.frame()   

# Create list of behavioal data files that have to be loaded
logfiles <- list.files(path_behavioral_data, pattern = ".txt")       


# Load behavioral data and concatenate data of all participants
for (participant in logfiles){                                                                                    
  one_participant <- read.table(paste0(path_behavioral_data, participant), skip = 28, fill = TRUE, nrows = 480, header = TRUE, stringsAsFactors = FALSE)
  behavioral_data <- rbind(behavioral_data, one_participant)
}  


# Load ERP and SCR data
ERN_data         <- read.csv(paste0(path_ERP_data,             "single_trial_MFN_0_100_FCz_with_events.csv"),                   header = TRUE, stringsAsFactors = FALSE)
ERN_baseline     <- read.csv(paste0(path_ERP_data,             "single_trial_MFN_baseline_-200_0_FCz_with_events.csv"),         header = TRUE, stringsAsFactors = FALSE)
ERN_data_avg_ref <- read.csv(paste0(path_ERP_data_avg_ref,     "single_trial_MFN_0_100_FCz_with_events_average_reference.csv"), header = TRUE, stringsAsFactors = FALSE)
Pe_data          <- read.csv(paste0(path_ERP_data,             "single_trial_Pe_200_400_Pz_with_events.csv"),                   header = TRUE, stringsAsFactors = FALSE)
Pe_baseline      <- read.csv(paste0(path_ERP_data,             "single_trial_Pe_baseline_-200_0_Pz_with_events.csv"),           header = TRUE, stringsAsFactors = FALSE)
N2_data          <- read.csv(paste0(path_ERP_data_stim_locked, "single_trial_N2_200_300_FCz_with_events.csv"),                  header = TRUE, stringsAsFactors = FALSE)
P3_data          <- read.csv(paste0(path_ERP_data_stim_locked, "single_trial_P3_300_500_CPz_with_events.csv"),                  header = TRUE, stringsAsFactors = FALSE)
SCR_data         <- read.csv(paste0(path_SCR_data,             "single_trial_SCR.csv"),                                         header = TRUE, stringsAsFactors = FALSE)


# Load feedback infos and concatenate data of all participants (read P_29_T2 in separately, because this subject had an additional task block)
for (participant in logfiles){
  if (participant != "Flanker_P_29_T2.txt") {
    one_participant_feedback <- read.table(paste0(path_behavioral_data, participant), skip = 513, sep = ",", fill = TRUE, header = FALSE, na.strings=c("","NA"), stringsAsFactors = FALSE)
  }
  else
  {
    one_participant_feedback <- read.table(paste0(path_behavioral_data, participant), skip = 593, sep = ",", nrows = 6, fill = TRUE, header = FALSE, na.strings=c("","NA"), stringsAsFactors = FALSE)
  }
  one_participant_feedback$participant_id <- substr(participant, 9, 15) # add coumn with participant_id
  feedback_infos <- rbind(feedback_infos, one_participant_feedback)
}  



####################   Merge Behavioral and ERP Data   ####################   

# In behavioral data, delete string 'Flanker_' from participant ID to have identical IDs in behavioral and ERP data for merging
behavioral_data$name <- gsub('Flanker_', '', behavioral_data$name)


# Fix missing trials (no trigger sent, e.g. due to RT = 1 ms triggers may overlap and only one is sent) in ERP data (increment trial nummer after missing trial by number of missing trials to enable matching)
ERN_data[ERN_data$participant_id == 'P_17_T1'                 & ERN_data$trial >= 270,]$trial         <- ERN_data[ERN_data$participant_id == 'P_17_T1'                 & ERN_data$trial >= 270,]$trial         + 1 # in P_17_T1 trial 270 is missing 
ERN_baseline[ERN_baseline$participant_id == 'P_17_T1'         & ERN_baseline$trial >= 270,]$trial     <- ERN_baseline[ERN_baseline$participant_id == 'P_17_T1'         & ERN_baseline$trial >= 270,]$trial     + 1 # in P_17_T1 trial 270 is missing 
ERN_data_avg_ref[ERN_data_avg_ref$participant_id == 'P_17_T1' & ERN_data_avg_ref$trial >= 270,]$trial <- ERN_data_avg_ref[ERN_data_avg_ref$participant_id == 'P_17_T1' & ERN_data_avg_ref$trial >= 270,]$trial + 1 # in P_17_T1 trial 270 is missing 
Pe_data[Pe_data$participant_id == 'P_17_T1'                   & Pe_data$trial >= 270,]$trial          <- Pe_data[Pe_data$participant_id == 'P_17_T1'                   & Pe_data$trial >= 270,]$trial          + 1 # in P_17_T1 trial 270 is missing 
Pe_baseline[Pe_baseline$participant_id == 'P_17_T1'           & Pe_baseline$trial >= 270,]$trial      <- Pe_baseline[Pe_baseline$participant_id == 'P_17_T1'           & Pe_baseline$trial >= 270,]$trial      + 1 # in P_17_T1 trial 270 is missing 

# Notes on missing trigger detection procedure
# 1) Identify approximate location of missing trigger by inspecting the single_trial_data after merging (see next step) - after these triggers, no ERP is imported but NA is inserted for this participant
# 2) Inspect eventlist_after_AR file in the participant's preprocessing folder (..\1_Flanker_Analysis_with_EEGLAB\0_Preprocessing_Files_Each_Subject)
# 3) If everything is fine, only very few rows of the ERP column in the subsequently created single_trial_data table should contain NA (NaN = artifact; NA = no eeg data present)


# Merge behavioral, ERP, and SCR data (rows in behavioral_data with no match in ERN_data will have NAs in the ERP column)
single_trial_data <- left_join(behavioral_data,   ERN_data, by = c('name' = 'participant_id', 'trial' = 'trial', 'resp1' = 'event'))
single_trial_data <- left_join(single_trial_data, ERN_baseline, by = c('name' = 'participant_id', 'trial' = 'trial', 'resp1' = 'event', 'artifact' = 'artifact'))
single_trial_data <- left_join(single_trial_data, Pe_data, by = c('name' = 'participant_id', 'trial' = 'trial', 'resp1' = 'event', 'artifact' = 'artifact'))
single_trial_data <- left_join(single_trial_data, Pe_baseline, by = c('name' = 'participant_id', 'trial' = 'trial', 'resp1' = 'event', 'artifact' = 'artifact'))
single_trial_data <- left_join(single_trial_data, SCR_data, by = c('name' = 'participant_id', 'trial' = 'trial', 'cond' = 'cond', 'resp1' = 'resp1'))
single_trial_data <- left_join(single_trial_data, ERN_data_avg_ref, by = c('name' = 'participant_id', 'trial' = 'trial', 'resp1' = 'event'))
single_trial_data <- left_join(single_trial_data, N2_data, by = c('name' = 'participant_id', 'trial' = 'trial', 'cond' = 'event'))
single_trial_data <- left_join(single_trial_data, P3_data, by = c('name' = 'participant_id', 'trial' = 'trial', 'cond' = 'event'))



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
  subset(select = c("name", "group", "session", "stimulation", "trial", "stimulus_type", "response_type", "rt1", "rt_log", "rt_invalid" ,"response_type_2nd", "rt2", "MFN_0_100_FCz.x", "MFN_0_100_FCz.y", "MFN_.200_0_FCz", "Pe_200_400_Pz", "Pe_.200_0_Pz","N2_200_300_FCz","P3_300_500_CPz","ISCR")) %>%
  dplyr::rename(participant_id     = name,
                rt                 = rt1,
                rt_2nd             = rt2,
                MFN_0_100_FCz      = MFN_0_100_FCz.x, 
                MFN_0_100_FCz_avg_ref      = MFN_0_100_FCz.y,
                MFN_baseline_pre_200_0_FCz = MFN_.200_0_FCz,
                Pe_baseline_pre_200_0_Pz   = Pe_.200_0_Pz) %>% 
  dplyr::mutate(participant_id = substr(participant_id, 1, 4))




####################   Clean Feedback Infos   ####################

# Rename columns and remove string '_T1/T2' from participant ID (to get correct number of factor levels later), create session column, and make variables factors
feedback_infos <- feedback_infos %>% 
  dplyr::rename(block    = V1,
                feedback = V4) %>% 
  dplyr::mutate(block          = as.factor(gsub('Block: ', '', block)),
                feedback       = as.factor(gsub('Feedback: ', '', feedback)),
                session        = as.factor(ifelse(substr(participant_id, 6, 7) == "T1", "T1", "T2")),
                participant_id = as.factor(substr(participant_id, 1, 4)),
                group          = as.factor(ifelse(substr(participant_id, 1, 1) == "C", "HC", "OCD"))
                ) %>%
  subset(select = c("participant_id", "session", "group", "block", "feedback")) 




###################   Save Data   ####################

save(single_trial_data, file = paste0(path_cleaned_data, "Single_Trial_Data.rda"))
save(feedback_infos, file = paste0(path_cleaned_data, "Feedback_Infos.rda"))