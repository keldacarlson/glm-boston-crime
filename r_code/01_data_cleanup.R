library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
library(tidyverse)
library(data.table)

setwd("/Users/kelda/OneDrive/Documents/R Code/Datasets/Boston Crime")

#Pull in data for offense code to group mapping
offense_mapping <- read.csv(file = "offense_code_group_mapping.csv", header = T)

#Pull in data from 2015 to present and put into a list
n <- 12 #number of datasets

df_list_empty <- list()
df_list <- lapply(1:n, function(x){df_list_empty[[x]] <- read.csv(file = paste0("boston_crime_20",x + 14,".csv"), header = T)})

#Check that all datasets have the same headers, number of variables and variable type
for (i in 1:(n-1)){
  df1 <- df_list[[i]]
  df2 <- df_list[[i+1]]
  
  col_check <- ncol(df1) == ncol(df2)
  header_check <- all(names(df1) == names(df2)) 
  
  if(!col_check) {break}
  if(!header_check) {break}
  
  for(c in 1:ncol(df1)){
    class_check <- class(df1[,c]) == class(df2[,c])
  }
  
  if(!class_check) {break}
  
} # close for loop

#Combine into a single dataset
combined_df <- rbindlist(df_list)

#Clean house
rm(df_list,df_list_empty,df1,df2,c,class_check,col_check,header_check,i,n)

#Replace all blanks with NAs
combined_df[combined_df == ""] <- NA

#Review overall structure
str(combined_df)
summary(combined_df)

#Remove duplicate records and create unique identifier
dup_check <- nrow(combined_df) == nrow(distinct(combined_df))
combined_df <- distinct(combined_df)
dup_check <- nrow(combined_df) == nrow(distinct(combined_df))

unique_id_check <- nrow(combined_df) == length(unique(combined_df$INCIDENT_NUMBER))

combined_df <- combined_df %>% mutate(ID = row_number())
combined_df <- combined_df %>% relocate(ID, .before = INCIDENT_NUMBER)

unique_id_check <- nrow(combined_df) == length(unique(combined_df$ID))

rm(dup_check,unique_id_check)

#rename fields in mapping df
offense_mapping <- offense_mapping %>%
  rename(
    OFFENSE_CODE = CODE,
    OFFENSE_CODE_GROUP = NAME
    )

#Uppercase OFFENSE_CODE_GROUP and OFFENSE_DESCRIPTION columns
combined_df$OFFENSE_CODE_GROUP <- toupper(combined_df$OFFENSE_CODE_GROUP)
combined_df$OFFENSE_DESCRIPTION <- toupper(combined_df$OFFENSE_DESCRIPTION)

offense_mapping$OFFENSE_CODE_GROUP <- toupper(offense_mapping$OFFENSE_CODE_GROUP)

#High level summary stats
yr_summary <- combined_df %>%
  group_by(YEAR) %>%
  summarise(
    n_rows = n(),
    missing_off_group = sum(is.na(OFFENSE_CODE_GROUP)),
    missing_off_code = sum(is.na(OFFENSE_CODE)),
    missing_hour = sum(is.na(HOUR))
  )

#Noted offense group is missing when code is not - need to add new group field
df_missing_groups <- combined_df %>% filter(is.na(OFFENSE_CODE_GROUP))
df_pop_groups <- combined_df %>% filter(!is.na(OFFENSE_CODE_GROUP))

missing_groups <- df_missing_groups %>% distinct(OFFENSE_CODE)
pop_groups <- df_pop_groups %>% distinct(OFFENSE_CODE,OFFENSE_CODE_GROUP)

new_groups_from_data <- left_join(missing_groups, pop_groups, by = "OFFENSE_CODE")

#Look at groups that are still missing
still_missing_groups <- new_groups_from_data %>%
  filter(is.na(OFFENSE_CODE_GROUP)) %>%
  distinct(OFFENSE_CODE)

new_groups_from_mapping <- left_join(still_missing_groups, offense_mapping, by = "OFFENSE_CODE")

#Combine results in final group mapping and attached new group to data
final_offense_mapping <- rbind(new_groups_from_data %>% filter(!is.na(OFFENSE_CODE_GROUP)),
                               new_groups_from_mapping %>% filter(!is.na(OFFENSE_CODE_GROUP))
                               )

colnames(final_offense_mapping) <- c("OFFENSE_CODE","MAPPED_OFFENSE_CODE_GROUP") 

missing_w_mapped_group <- left_join(df_missing_groups, final_offense_mapping, by = "OFFENSE_CODE")
df_pop_groups$MAPPED_OFFENSE_CODE_GROUP <- df_pop_groups$OFFENSE_CODE_GROUP
df_w_mapped_group <- rbind(missing_w_mapped_group,df_pop_groups)

#clean house
rm(df_missing_groups,df_pop_groups,final_offense_mapping, missing_groups,missing_w_mapped_group, new_groups_from_data, new_groups_from_mapping,offense_mapping,pop_groups,still_missing_groups, yr_summary)


#Look into records that are missing offense group but have description field
df_pop_mapped_off <- df_w_mapped_group %>% filter(!is.na(MAPPED_OFFENSE_CODE_GROUP))
df_missing_mapped_off <- df_w_mapped_group %>% filter(is.na(MAPPED_OFFENSE_CODE_GROUP))

#confirm that all records missing offense group have a description
desc_check <- nrow(df_missing_mapped_off %>% filter(is.na(OFFENSE_DESCRIPTION))) == 0

#Populate missing offense group with the description field             
off_code_desc <- df_missing_mapped_off %>% distinct(OFFENSE_CODE,OFFENSE_DESCRIPTION)
off_code_desc <- off_code_desc %>% mutate(OFFENSE_DESCRIPTION = str_replace_all(OFFENSE_DESCRIPTION, "MIGRATED REPORT - ", ""))
off_code_desc <- off_code_desc %>% rename(DESC_OFFENSE_CODE_GROUP = OFFENSE_DESCRIPTION)

df_pop_mapped_off$DESC_OFFENSE_CODE_GROUP <- df_pop_mapped_off$MAPPED_OFFENSE_CODE_GROUP
missing_w_desc_group <- left_join(df_missing_mapped_off, off_code_desc, by = "OFFENSE_CODE")

df_w_desc_group <- rbind(df_pop_mapped_off,missing_w_desc_group)

#clean house
rm(df_pop_mapped_off, df_missing_mapped_off, missing_w_desc_group,off_code_desc,desc_check)

#Summarize by offense group
df_w_desc_group <- arrange(df_w_desc_group,DESC_OFFENSE_CODE_GROUP)

summary_by_off_group <- df_w_desc_group %>%
  group_by(DESC_OFFENSE_CODE_GROUP) %>%
  summarise(
    n_rows = n()
  )

#Replace "other" groups with more useful info
df_w_other_group <- df_w_desc_group %>% filter(str_detect(DESC_OFFENSE_CODE_GROUP,regex("OTHER", ignore_case = TRUE)))
df_wo_other_group <- df_w_desc_group %>% filter(!str_detect(DESC_OFFENSE_CODE_GROUP,regex("OTHER", ignore_case = TRUE)))

off_code_other_burglary <- df_w_other_group %>% filter(str_detect(DESC_OFFENSE_CODE_GROUP,regex("OTHER BURGLARY", ignore_case = TRUE)))
off_code_other_larceny <- df_w_other_group %>% filter(str_detect(DESC_OFFENSE_CODE_GROUP,regex("OTHER LARCENY", ignore_case = TRUE)))
off_code_other_other <- df_w_other_group %>% 
  filter(!str_detect(DESC_OFFENSE_CODE_GROUP,regex("OTHER LARCENY", ignore_case = TRUE))) %>% 
  filter(!str_detect(DESC_OFFENSE_CODE_GROUP,regex("OTHER BURGLARY", ignore_case = TRUE)))

df_wo_other_group$FINAL_OFFENSE_CODE_GROUP <- df_wo_other_group$DESC_OFFENSE_CODE_GROUP
off_code_other_burglary$FINAL_OFFENSE_CODE_GROUP <- "BURGLARY"
off_code_other_larceny$FINAL_OFFENSE_CODE_GROUP <- "LARCENY"
off_code_other_other$FINAL_OFFENSE_CODE_GROUP <- off_code_other_other$OFFENSE_DESCRIPTION

df_w_offense_group <- rbind(df_wo_other_group,off_code_other_burglary,off_code_other_larceny,off_code_other_other)

#Separate out shoplifting from other larceny
df_shoplifting <- df_w_offense_group %>% filter(str_count(OFFENSE_DESCRIPTION, regex('SHOPLIFTING')) == 1)
df_non_shoplifting <- df_w_offense_group %>% filter(str_count(OFFENSE_DESCRIPTION, regex('SHOPLIFTING')) == 0)

df_shoplifting$FINAL_OFFENSE_CODE_GROUP <- "SHOPLIFTING"

df_w_final_group <- rbind(df_shoplifting,df_non_shoplifting)

#clean house
rm(df_w_desc_group,df_w_mapped_group,df_w_other_group,df_wo_other_group,off_code_other_burglary,off_code_other_larceny,off_code_other_other,summary_by_off_group)
rm(df_w_offense_group,df_shoplifting,df_non_shoplifting)

#Group offense groups in broader offense categories
df_w_final_group <- df_w_final_group %>% mutate(OFFENSE_CATEGORY = fct_recode(FINAL_OFFENSE_CODE_GROUP,
"ABDUCTION" = "ABDUCTION - INTICING",
"DISORDERLY CONDUCT" = "AFFRAY/DISTURBING THE PEACE/DISORDERLY CONDUCT",
"ASSAULT" = "AGGRAVATED ASSAULT",
"ASSAULT" = "AGGRAVATED ASSAULT/AGGRAVATED ASSAULT & BATTERY",
"NONCRIME" = "AIRCRAFT",
"ANIMAL ABUSE" = "ANIMAL ABUSE",
"ARSON" = "ARSON",
"ASSAULT" = "ASSAULT/ASSAULT & BATTERY",
"NONVIOLENT (OTHER)" = "ASSEMBLY OR GATHERING VIOLATIONS",
"AUTO THEFT" = "AUTO THEFT",
"NONCRIME" = "AUTO THEFT RECOVERY",
"BURGLARY" = "B&E NON-RESIDENCE NIGHT - ATTEMPT FORCE",
"NONCRIME" = "BALLISTICS",
"THREAT OF ATTACK" = "BIOLOGICAL THREAT",
"THREAT OF ATTACK" = "BOMB HOAX",
"BURGLARY" = "BREAKING AND ENTERING (B&E) MOTOR VEHICLE",
"BURGLARY" = "BURGLARY",
"BURGLARY" = "BURGLARY - NO PROPERTY TAKEN",
"BURGLARY" = "BURGLARY/BREAKING AND ENTERING",
"BURGLARY" = "COMMERCIAL BURGLARY",
"FRAUD" = "CONFIDENCE GAMES",
"FRAUD" = "COUNTERFEITING",
"FRAUD" = "COUNTERFEITING/FORGERY",
"HARASSMENT" = "CRIMINAL HARASSMENT",
"HOMICIDE" = "CRIMINAL HOMICIDE",
"ABDUCTION" = "CUSTODIAL KIDNAPPING",
"NONVIOLENT (OTHER)" = "DANGEROUS OR HAZARDOUS CONDITION",
"NONCRIME" = "DEATH INVESTIGATION",
"DISORDERLY CONDUCT" = "DISORDERLY CONDUCT",
"DRUG VIOLATION" = "DRUG VIOLATION",
"DRUG VIOLATION" = "DRUGS - POSSESSION/MANUFACTURING/DISTRIBUTE",
"FRAUD" = "EMBEZZLEMENT",
"NONVIOLENT (OTHER)" = "EVADING FARE",
"NONCRIME" = "EVIDENCE TRACKER INCIDENTS",
"WEAPONS" = "EXPLOSIVES",
"NONVIOLENT (OTHER)" = "EXTORTION OR BLACKMAIL",
"NONVIOLENT (OTHER)" = "FIRE RELATED REPORTS",
"WEAPONS" = "FIREARM DISCOVERY",
"WEAPONS" = "FIREARM VIOLATIONS",
"NONVIOLENT (OTHER)" = "FRAUD",
"NONVIOLENT (OTHER)" = "GAMBLING",
"HARASSMENT" = "HARASSMENT",
"NONCRIME" = "HARBOR RELATED INCIDENTS",
"BURGLARY" = "HOME INVASION",
"HOMICIDE" = "HOMICIDE",
"HUMAN TRAFFICKING" = "HUMAN TRAFFICKING",
"HUMAN TRAFFICKING" = "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE",
"NONCRIME" = "INJURED/MEDICAL/SICK ASSIST",
"HARASSMENT" = "INTIMIDATING WITNESS",
"NONCRIME" = "INVESTIGATE PERSON",
"NONCRIME" = "INVESTIGATE PROPERTY",
"NONCRIME" = "INVESTIGATION FOR ANOTHER AGENCY",
"HOMICIDE" = "JUSTIFIABLE HOMICIDE",
"ABDUCTION" = "KIDNAPPING",
"ABDUCTION" = "KIDNAPPING - ENTICING OR ATTEMPTED",
"ABDUCTION" = "KIDNAPPING/CUSTODIAL KIDNAPPING",
"ABDUCTION" = "KIDNAPPING/CUSTODIAL KIDNAPPING/ ABDUCTION",
"NONVIOLENT (OTHER)" = "LANDLORD/TENANT DISPUTES",
"THEFT (OTHER)" = "LARCENY",
"THEFT FROM CAR" = "LARCENY FROM MOTOR VEHICLE",
"THEFT FROM CAR" = "LARCENY FROM MV",
"NONVIOLENT (OTHER)" = "LICENSE PLATE RELATED INCIDENTS",
"NONVIOLENT (OTHER)" = "LICENSE VIOLATION",
"NONVIOLENT (OTHER)" = "LIQUOR VIOLATION",
"HOMICIDE" = "MANSLAUGHTER",
"HOMICIDE" = "MANSLAUGHTER - TRAIN ETC. VICTIM NON-NEGLIGENCE",
"NONCRIME" = "MEDICAL ASSISTANCE",
"NONVIOLENT (OTHER)" = "MIGRATED REPORT - AUTO LAW VIOLATION",
"NONCRIME" = "MIGRATED REPORT - OTHER PART II",
"NONCRIME" = "MIGRATED REPORT - OTHER PART III",
"NONCRIME" = "MISSING PERSON LOCATED",
"NONCRIME" = "MISSING PERSON REPORTED",
"NONCRIME" = "MOTOR VEHICLE ACCIDENT RESPONSE",
"NONCRIME" = "MOTOR VEHICLE CRASH",
"SEX OFFENSE" = "OBSCENE MATERIALS - PORNOGRAPHY",
"SEX OFFENSE" = "OFFENSES AGAINST CHILD / FAMILY",
"NONVIOLENT (OTHER)" = "OPERATING UNDER THE INFLUENCE",
"NONCRIME" = "OTHER OFFENSE",
"HARASSMENT" = "PHONE CALL COMPLAINTS",
"NONCRIME" = "POLICE SERVICE INCIDENTS",
"NONCRIME" = "POSSESSION OF BURGLARIOUS TOOLS",
"NONVIOLENT (OTHER)" = "PRISONER RELATED INCIDENTS",
"THEFT (OTHER)" = "PROPERTY - CONCEALING LEASED",
"NONCRIME" = "PROPERTY FOUND",
"NONCRIME" = "PROPERTY LOST",
"PROPERTY DAMAGE" = "PROPERTY RELATED DAMAGE",
"NONVIOLENT (OTHER)" = "PROSTITUTION",
"NONCRIME" = "RECOVERED - MV RECOVERED IN BOSTON (STOLEN IN BOSTON) MUST BE SUPPLEMENTAL",
"NONCRIME" = "RECOVERED STOLEN PROPERTY",
"NONCRIME" = "REPORT AFFECTING OTHER DEPTS.",
"BURGLARY" = "RESIDENTIAL BURGLARY",
"HARASSMENT" = "RESTRAINING ORDER VIOLATIONS",
"ROBBERY" = "ROBBERY",
"NONCRIME" = "SEARCH WARRANTS",
"NONCRIME" = "SERVICE",
"SHOPLIFTING" = "SHOPLIFTING",
"NONCRIME" = "SICK ASSIST",
"ASSAULT" = "SIMPLE ASSAULT",
"THREAT OF ATTACK" = "THREATS TO DO BODILY HARM",
"NONCRIME" = "TOWED",
"NONVIOLENT (OTHER)" = "TRESPASSING",
"NONVIOLENT (OTHER)" = "VAL - VIOLATION OF AUTO LAW - OTHER",
"PROPERTY DAMAGE" = "VANDALISM",
"PROPERTY DAMAGE" = "VANDALISM/DESTRUCTION OF PROPERTY",
"DISORDERLY CONDUCT" = "VERBAL DISPUTES",
"NONVIOLENT (OTHER)" = "VIOLATION - CITY ORDINANCE",
"NONVIOLENT (OTHER)" = "VIOLATION - CITY ORDINANCE CONSTRUCTION PERMIT",
"HARASSMENT" = "VIOLATION - HARASSMENT PREVENTION ORDER",
"NONVIOLENT (OTHER)" = "VIOLATION - HAWKER AND PEDDLER",
"NONVIOLENT (OTHER)" = "VIOLATIONS",
"NONCRIME" = "WARRANT ARREST - OUTSIDE OF BOSTON WARRANT",
"NONCRIME" = "WARRANT ARRESTS",
"WEAPONS" = "WEAPON - OTHER - OTHER VIOLATION",
"WEAPONS" = "WEAPONS VIOLATION"
) #close fct_recode
) #close mutate

#Add district code to district name
df_w_final_group$DISTRICT[is.na(df_w_final_group$DISTRICT)] <- "Missing"

df_w_final_group <- df_w_final_group %>% mutate(DISTRICT_NAME = fct_recode(DISTRICT,
"Downtown" = "A1",
"Charlestown" = "A15",
"East Boston" = "A7",
"Roxbury" = "B2",
"Mattapan" = "B3",
"South Boston" = "C6",
"Dorchester" = "C11",
"South End" = "D4",
"Brighton" = "D14",
"West Roxbury" = "E5",
"Jamaica Plain" = "E13",
"Hyde Park" = "E18",
"Outside of Boston" = "External",
"Outside of Boston" = "Outside of",
"Outside of Boston" = "Missing"
))

#Summarize by FINAL_OFFENSE_CODE_GROUP, OFFENSE_DESCRIPTION and UCR PART
summary_by_final_group <- df_w_final_group %>%
  group_by(FINAL_OFFENSE_CODE_GROUP,OFFENSE_DESCRIPTION,UCR_PART) %>%
  summarise(
    n_rows = n(),
    .groups = 'drop' # Recommended to drop the grouping structure afterward
  )

summary_by_final_group <- summary_by_final_group %>% arrange(FINAL_OFFENSE_CODE_GROUP,UCR_PART)

#Divide dataset based on UCR_PART field
df_w_pop_ucr <- df_w_final_group %>% filter(str_detect(UCR_PART,regex("Part", ignore_case = TRUE))) 
df_w_missing_ucr <- df_w_final_group %>% filter(is.na(UCR_PART) | UCR_PART == "Other")

#Replace "Other" with NA
df_w_missing_ucr$UCR_PART <- NA

#Concatenate populated UCR_PART by FINAL_OFFENSE_CODE_GROUP and OFFENSE_DESCRIPTION
group_desc_ucr <- df_w_pop_ucr %>%
  group_by(FINAL_OFFENSE_CODE_GROUP, OFFENSE_DESCRIPTION) %>%
  summarise(
    UCR_PART_MAPPED = paste(unique(UCR_PART), collapse = ", "),
    .groups = 'drop' # Recommended to drop the grouping structure afterward
  )

df_w_mapped_ucr <- left_join(df_w_missing_ucr, group_desc_ucr, by = c("FINAL_OFFENSE_CODE_GROUP", "OFFENSE_DESCRIPTION"))
df_w_pop_mapped_ucr <- df_w_mapped_ucr %>% filter(!is.na(UCR_PART_MAPPED))
df_w_missing_mapped_ucr <- df_w_mapped_ucr %>% filter(is.na(UCR_PART_MAPPED)) 

#Manually review and populate missing UCR
summary_of_missing_ucr <- df_w_missing_mapped_ucr %>%
  group_by(FINAL_OFFENSE_CODE_GROUP,OFFENSE_DESCRIPTION) %>%
  summarise(
    n_rows = n(),
    .groups = 'drop' # Recommended to drop the grouping structure afterward
  )

#Export summary of missing ucrs to excel and populate with manual UCR
#write.csv(summary_of_missing_ucr,"missing_ucr.csv")

ucr_manual_mapping <- read.csv(file = "missing_ucr_manual_mapping.csv", header = T)
ucr_manual_mapping$n_rows <- NULL

df_w_manual_ucr <- left_join(df_w_missing_mapped_ucr, ucr_manual_mapping, by = c("FINAL_OFFENSE_CODE_GROUP", "OFFENSE_DESCRIPTION"))

#Combine dfs for final UCR field
df_w_pop_ucr$UCR_PART_MAPPED <- df_w_pop_ucr$UCR_PART_MANUAL <- NA
df_w_pop_ucr$UCR_PART_FINAL <- df_w_pop_ucr$UCR_PART

df_w_pop_mapped_ucr$UCR_PART_MANUAL <- NA
df_w_pop_mapped_ucr$UCR_PART_FINAL <- df_w_pop_mapped_ucr$UCR_PART_MAPPED

df_w_manual_ucr$UCR_PART_FINAL <- df_w_manual_ucr$UCR_PART_MANUAL

df_w_ucr <- rbind(df_w_pop_ucr,df_w_pop_mapped_ucr,df_w_manual_ucr)
ucr_check <- nrow(df_w_ucr %>% filter(is.na(UCR_PART_FINAL))) == 0
  
#clean_house  
rm(df_w_final_group, df_w_manual_ucr, df_w_mapped_ucr, df_w_missing_mapped_ucr, df_w_missing_ucr,
   df_w_pop_mapped_ucr, df_w_pop_ucr, group_desc_ucr, summary_by_final_group, summary_of_missing_ucr,
   ucr_manual_mapping,ucr_check)  


#Clean up SHOOTING field (currently a mix of 1/0/Y)
df_shooting_y <- df_w_ucr %>% filter(SHOOTING == "Y")
df_shooting_other <- df_w_ucr %>% filter(is.na(SHOOTING) | SHOOTING != "Y")

df_shooting_y$SHOOTING_FINAL <- 1
df_shooting_other$SHOOTING_FINAL <- as.integer(df_shooting_other$SHOOTING)

df_w_shooting <- rbind(df_shooting_y,df_shooting_other)

#clean_house
rm(df_shooting_y,df_shooting_other,df_w_ucr)
  
#Calculate day from character date field and create new date field
df_w_day <- df_w_shooting
df_w_day$first_slash <- regexpr("/",df_w_day$OCCURRED_ON_DATE)
df_w_day$first_dash <- regexpr("-",df_w_day$OCCURRED_ON_DATE)
df_w_day$first_space <- regexpr(" ",df_w_day$OCCURRED_ON_DATE)

df_w_day_slash <- df_w_day %>% filter(first_slash > 0)
df_w_day_dash <- df_w_day %>% filter(first_dash > 0)

df_w_day_slash$day_temp <- substr(df_w_day_slash$OCCURRED_ON_DATE,df_w_day_slash$first_slash+1,df_w_day_slash$first_slash+2)
df_w_day_slash <- df_w_day_slash %>% mutate(day_temp = str_replace_all(day_temp, "/", ""))

df_w_day_dash$day_temp <- substr(df_w_day_dash$OCCURRED_ON_DATE,df_w_day_dash$first_space-2,df_w_day_dash$first_space-1)

df_w_day <- rbind(df_w_day_slash,df_w_day_dash)

df_w_day$DAY_OF_MONTH <- as.integer(df_w_day$day_temp)
df_w_day$first_slash <- df_w_day$first_dash <- df_w_day$first_space <- df_w_day$day_temp <- NULL  
  
df_w_day$DATE <- make_date(df_w_day$YEAR,df_w_day$MONTH,df_w_day$DAY_OF_MONTH )

#clean house
rm(df_w_shooting,df_w_day_slash,df_w_day_dash)

#remove intermediate or unnecessary fields
df_clean_formatted <- df_w_day

#Only keep final offense group
df_clean_formatted$OFFENSE_CODE_GROUP <- df_clean_formatted$FINAL_OFFENSE_CODE_GROUP
df_clean_formatted$MAPPED_OFFENSE_CODE_GROUP <- df_clean_formatted$DESC_OFFENSE_CODE_GROUP <- NULL  
df_clean_formatted$FINAL_OFFENSE_CODE_GROUP <- NULL

#Don't need the following fields since we have DISTRICT
df_clean_formatted$REPORTING_AREA <- df_clean_formatted$STREET <- df_clean_formatted$Lat <- NULL
df_clean_formatted$Long <- df_clean_formatted$Location <- NULL

#Replace SHOOTING with numeric field
df_clean_formatted$SHOOTING <- df_clean_formatted$SHOOTING_FINAL
df_clean_formatted$SHOOTING_FINAL <- NULL

#Don't need character date field 
df_clean_formatted$OCCURRED_ON_DATE <- NULL

#Only keep final UCR field
df_clean_formatted$UCR_PART <- df_clean_formatted$UCR_PART_FINAL
df_clean_formatted$UCR_PART_MANUAL <- df_clean_formatted$UCR_PART_MAPPED <- df_clean_formatted$UCR_PART_FINAL <- NULL

#Trim DAY_OF_WEEK to remove trailing spaces
df_clean_formatted$DAY_OF_WEEK <- str_trim(df_clean_formatted$DAY_OF_WEEK)

#Keep district name
df_clean_formatted$DISTRICT <- df_clean_formatted$DISTRICT_NAME
df_clean_formatted$DISTRICT_NAME <- NULL

#Reorder fields and sort final dataset
df_clean_formatted <- df_clean_formatted %>% relocate(OFFENSE_CATEGORY, .after = OFFENSE_CODE_GROUP)
df_clean_formatted <- df_clean_formatted %>% relocate(DAY_OF_MONTH, .after = MONTH)
df_clean_formatted <- df_clean_formatted %>% relocate(DATE, .after = SHOOTING)
df_clean_formatted <- df_clean_formatted %>% arrange(DATE,OFFENSE_CODE_GROUP)

#save raw and clean datasets to the folder
save(combined_df, file = "raw_data_all_years.RData")
save(df_clean_formatted, file = "clean_formatted_all_years.RData")

#clean house
rm(combined_df,df_w_day)  
  