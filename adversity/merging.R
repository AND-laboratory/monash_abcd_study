# Load data
library(readr)
# anxiety/mental health data - long format only with rows for waves that were collected
anxiety_merged_Shared <- read_csv("C:/Users/michelle/Google Drive/Professional/Students/0_Monash Students/Chrysa Tsiapas/anxiety_merged-Shared.csv")
View(anxiety_merged_Shared)
# adversity/ACES data - baseline only, i.e., one row per ID
aces_merged_Shared <- read_csv("C:/Users/michelle/Google Drive/Professional/Students/0_Monash Students/Chrysa Tsiapas/aces_merged-Shared.csv")
View(aces_merged_Shared)
# a mostly blank dataset with all IDs and 'eventname' rows (long format) for all 3 waves (baseline, 1 yr, and 2 yr)
IDs_full <- read_csv("C:/Users/michelle/Google Drive/Professional/Students/0_Monash Students/Chrysa Tsiapas/IDs_full.csv")
View(IDs_full)

# Convert to data tables
library(data.table)
anxiety_table <- as.data.table(anxiety_merged_Shared)
aces_table <- as.data.table(aces_merged_Shared)
IDs_full_table <- as.data.table(IDs_full)

# First make the full anxiety list so we can select those that have 2 year data
anxiety_full <- merge(IDs_full_table, anxiety_table, by = c("src_subject_id","eventname"), 
                      all = TRUE, sort = TRUE)

#get rid of weird useless vars made by merge
library(dplyr)
# anxiety_full2 <- select(anxiety_full, -c("X1.x","X.x","interview_date.x",
#                                   "interview_age.x","sex.x","X1.y","X.y"))

# ----------------- Model Building Sample

all_data <- merge(anxiety_full, aces_table, by = c("src_subject_id","eventname"), 
                  all = TRUE, sort = TRUE)

# 'model_building' variable is TRUE if it's model building (and FALSE if it's experimental sample AND in a 2 year follow up row)
all_data$model_building <- all_data$eventname=='2_year_follow_up_y_arm_1' & 
  is.na(all_data$cbcl_scr_dsm5_anxdisord_t)

# These do NOT have 2 year follow up anxiety data, therefore they are for model building
model_building_2y <- all_data[ which(all_data$model_building==TRUE), ]
# just check no duplicate IDs in there
check_MB_IDs <- unique(model_building_2y$src_subject_id)
#create just IDs for MB because we don't want the 2 yr blank data in there
model_building_IDs <- select(model_building_2y, "src_subject_id")

model_building <- merge(model_building_IDs,aces_table,by="src_subject_id",all.x=TRUE)
write.csv(model_building, file = "model_building_aces.csv")

# ----------------- Experimental Sample

# Don't use the FALSE 'model_building' value because this will include baseline and 1 year follow up rows for model building IDs
exp_sample_full <- all_data[ which(all_data$eventname==
                                     '2_year_follow_up_y_arm_1' &
                                     !is.na(anxiety_full$cbcl_scr_dsm5_anxdisord_t)), ]
exp_sample_IDs <- select(exp_sample_full, "src_subject_id")
check_ES_IDs <- unique(exp_sample_IDs$src_subject_id)

# just keep rows where the ID is in the experimental sample ID list made above
exp_sample <- all_data[which(all_data$src_subject_id %in% exp_sample_full$src_subject_id), ]
length(unique(exp_sample$src_subject_id))
write.csv(exp_sample, file = "experimental_sample.csv")

# Note - at this stage we cleaned up some of the unecessary variables manually and loaded them back in. At some stage I'll fix this so it's all in the code
MB_final <- read_csv("C:/Users/michelle/Google Drive/Professional/Students/0_Monash Students/Chrysa Tsiapas/model_building_aces_final.csv")
MB_final_table <- as.data.table(MB_final)
# Split model building dataset into 3 random subsamples:
MB_final$subsample <- sample(factor(rep(1:3, length.out=nrow(MB_final)), 
                                 labels=paste0("subsample", 1:3)))
MB_subsample1 <- MB_final[which(MB_final$subsample=="subsample1"), ]
MB_subsample2 <- MB_final[which(MB_final$subsample=="subsample2"), ]
MB_subsample3 <- MB_final[which(MB_final$subsample=="subsample3"), ]

write.csv(MB_subsample1, file = "MB_subsample1.csv")
write.csv(MB_subsample2, file = "MB_subsample2.csv")
write.csv(MB_subsample3, file = "MB_subsample3.csv")