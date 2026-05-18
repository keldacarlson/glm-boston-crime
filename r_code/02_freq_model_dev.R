library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(caret)
library(MASS)
library(fitdistrplus)
library(rcompanion)
library(performance)

setwd("/Users/kelda/OneDrive/Documents/R Code/Datasets/Boston Crime")

#Pull in clean dataset
load("clean_formatted_all_years.RData")

#Limit to crimes
crime_data <- df_clean_formatted  %>% filter(OFFENSE_CATEGORY != "NONCRIME")
  
#Summarize data by year
summary_by_year <- crime_data %>% 
  group_by(YEAR) %>%
  summarise(
    start_data <- min(DATE),
    end_data <- max(DATE),
    count=n()
    )

colnames(summary_by_year) <- c("year","start_date", "end_date","count")

#Remove 2015 and 2026 data since we don't have the full year
crime_data <- crime_data %>% filter(YEAR != 2015 & YEAR != 2026)

#Visualize data with many tables/plots

#Plot number of crime by category
crime_data  %>% 
  group_by(OFFENSE_CATEGORY) %>%
  summarise(count=n()) %>%
  ggplot(aes(x = reorder(OFFENSE_CATEGORY,count), y = count)) +
  geom_bar(stat = "identity") +
  labs(x ="Crime Category", y = "Crime Count") + 
  scale_y_continuous(label = comma) +
  coord_flip()

#Plot number of crimes by year
crime_data %>%
  ggplot(aes(x = YEAR)) +
  geom_bar() +
  labs(x = "Year", y= "Crime Count")

#Plot number of crimes by month
crime_data %>%
  ggplot(aes(x = MONTH)) +
  geom_bar() +
  labs(x = "Month", y= "Crime Count")

#Plot number of crimes by day of month
crime_data %>%
  ggplot(aes(x = DAY_OF_MONTH)) +
  geom_bar() +
  labs(x = "Day of Month", y= "Crime Count")

#Plot number of crimes by day of week
crime_data %>%
  ggplot(aes(x = DAY_OF_WEEK)) +
  geom_bar() +
  labs(x = "Day of Week", y= "Crime Count")

#Plot number of crimes by time of day
crime_data %>%
  ggplot(aes(x = HOUR)) +
  geom_bar() +
  labs(x = "Time of Day", y= "Crime Count")

#Plot number of crimes by district
crime_data %>%
  ggplot(aes(x = DISTRICT)) +
  geom_bar() +
  labs(x = "District", y= "Crime Count")

#Plot counts by district over time
counts_by_date_district <- crime_data %>% 
  group_by(DATE,DISTRICT) %>%
  arrange(DATE,DISTRICT) %>%
  summarise(count=n(), .groups = "drop")

ggplot(counts_by_date_district, aes(x=DATE, y=count)) + 
  geom_line() + 
  facet_wrap(~DISTRICT, ncol=1) + 
  theme_classic()

rm(counts_by_date_district)

#Takeaways from plots:
#lots of non violent, theft and drug offenses - but offense type is probably best saved for severity
#consistent from 2022-2025
#fewer crimes happen in Q1, more in Q3
#fewer crimes happen on Sunday
#fewer crimes happen from 1-8AM
#most crimes happen in Roxbury, South End and Downtown - but district is probably best saved for severity


#Summarize the dataset by day
num_dates <- length(seq(min(crime_data$DATE), max(crime_data$DATE), "days"))

count_by_date <- crime_data %>% 
  group_by(DATE,YEAR,MONTH,DAY_OF_WEEK) %>%
  arrange(DATE) %>%
  summarise(COUNT=n(), .groups = "drop")

#Check that dataset contains all dates in time range to ensure we capture days with 0 crimes
date_check <- num_dates == nrow(count_by_date)

qqnorm(count_by_date$COUNT, pch = 1, frame = FALSE)
qqline(count_by_date$COUNT, col = "steelblue", lwd = 2)

hist(count_by_date$COUNT) #NOTE - daily arrests appear to follow a normal dist.


#Summarize the dataset by day and hour

#Replace 0 with 24 to clearly denote later hour
count_by_hour_temp <- crime_data %>%
  mutate(HOUR = case_when(
    HOUR == 0 ~ 24,
    TRUE ~ HOUR  # Keep all other values as they were
  ))

count_by_hour_temp <- count_by_hour_temp %>% 
  group_by(DATE,YEAR,MONTH,DAY_OF_WEEK,HOUR) %>%
  arrange(DATE,HOUR) %>%
  summarise(COUNT=n(), .groups = "drop")

#Check that dataset contains all hours in time range to ensure we capture days with 0 crimes
hour_check <- num_dates * 24 == nrow(count_by_hour_temp)

#Since some are missing, create vector with all days/hours and merge
date_list <- rep(seq(min(crime_data$DATE), max(crime_data$DATE), "days"), each = 24)
hour_list <- rep(1:24, times = num_dates)

date_hour_df <- data.frame(cbind(date_list,hour_list))
colnames(date_hour_df) <- c("DATE","HOUR")
date_hour_df$DATE <- as.Date(date_hour_df$DATE)

count_by_hour_all <- date_hour_df %>%
  left_join(count_by_hour_temp, by = c("DATE","HOUR")) 

pop_data <- count_by_hour_all %>% filter(!is.na(COUNT))
missing_data <- count_by_hour_all %>% filter(is.na(COUNT))

missing_data$YEAR <- year(missing_data$DATE)
missing_data$MONTH <- month(missing_data$DATE) 
missing_data$DAY_OF_WEEK <- weekdays(missing_data$DATE)
missing_data$COUNT <- 0

count_by_hour <- rbind(pop_data,missing_data)
count_by_hour <- count_by_hour %>% arrange(DATE, HOUR)

hour_check <- num_dates * 24 == nrow(count_by_hour)

#High level tests for model data
qqnorm(count_by_hour$COUNT, pch = 1, frame = FALSE)
qqline(count_by_hour$COUNT, col = "steelblue", lwd = 2)

hist(count_by_hour$COUNT) #NOTE - this looks much more like a Poisson or Negbin dist.

#NOTE - Modeling on a day/hour level makes more sense given distribution and level of predictive variables
model_data <- count_by_hour

#Clean house
rm(summary_by_year,count_by_date,date_check,date_hour_df)
rm(count_by_hour,count_by_hour_all,count_by_hour_temp,missing_data,pop_data,date_list,hour_list,hour_check,num_dates)


#Create new variables based on observations from visualizing data
model_data$QUARTER <- quarters(model_data$DATE)
model_data <- model_data %>% mutate(Q1_IND = ifelse(QUARTER == "Q1",1,0))
model_data <- model_data %>% mutate(Q3_IND = ifelse(QUARTER == "Q3",1,0))
model_data <- model_data %>% mutate(SUNDAY_IND = ifelse(DAY_OF_WEEK == "Sunday",1,0))


#Separate testing and validation datasets before fitting models
t <- .75

training_rows <- createDataPartition(y=model_data$YEAR, p=t, list=FALSE)

train_df <- model_data[training_rows,]
val_df <- model_data[-training_rows,]

#Clean house
rm(training_rows,t)


#Check mean and SD to see if poisson makes sense
m <- mean(train_df$COUNT)
var <- var(train_df$COUNT) #NOTE - higher variance makes neg bin seem like a better fit

#Goodness of fit tests
fit_pois <- fitdist(train_df$COUNT, "pois")
fit_negbin <- fitdist(train_df$COUNT, "nbinom") #NOTE - negbin has lower AIC and BIC

#Test different distributions with HOUR (clearly the most predictive based on histogram)
glm_poi <- glm(COUNT ~ HOUR, family = poisson, data = train_df)
glm_qpoi <- glm(COUNT ~ HOUR, family = quasipoisson, data = train_df) 
glm_negbin <- glm.nb(COUNT ~ HOUR, data = train_df) 

compareGLM(glm_poi, glm_qpoi,glm_negbin) 

#NOTE - too overdispersed for poisson. Negbin is best fit (lower AIC and BIC). Will use negbin going forward.

#Test negative binomial with individual variables
glm_hour <- glm_negbin
glm_quarter <- glm.nb(COUNT ~ QUARTER, data = train_df)
glm_q1ind <- glm.nb(COUNT ~ Q1_IND, data = train_df)
glm_q3ind <- glm.nb(COUNT ~ Q3_IND, data = train_df)
glm_month <- glm.nb(COUNT ~ MONTH, data = train_df)
glm_dayofweek <- glm.nb(COUNT ~ DAY_OF_WEEK, data = train_df)
glm_sundayind <- glm.nb(COUNT ~ SUNDAY_IND, data = train_df)

compareGLM(glm_hour, glm_quarter, glm_q1ind,glm_q3ind, glm_month, glm_dayofweek,glm_sundayind)
compare_performance(glm_hour, glm_quarter, glm_q1ind,glm_q3ind, glm_month, glm_dayofweek,glm_sundayind, rank = TRUE, verbose = FALSE) 

#NOTE - Hour is most predictive, followed by quarter and day of week. Month is not predictive enough to use going forward.

#Add more variables with a forward stepwise function, starting with quarter and Q1 & Q3 inds
glm_hr_quarter <- glm.nb(COUNT ~ HOUR + QUARTER, data = train_df)
glm_hr_q1ind <- glm.nb(COUNT ~ HOUR + Q1_IND, data = train_df)
glm_hr_q3ind <- glm.nb(COUNT ~ HOUR + Q3_IND, data = train_df)

compareGLM(glm_hour, glm_hr_quarter,glm_hr_q1ind,glm_hr_q3ind) 
compare_performance(glm_hour, glm_hr_quarter,glm_hr_q1ind,glm_hr_q3ind, rank = TRUE, verbose = FALSE) 

#Individual quarters look best, followed by Q3 ind. Q1 ind is not predictive enough to use going forward.
anova(glm_hour, glm_hr_quarter, test = "Chisq") 
anova(glm_hour, glm_hr_q3ind, test = "Chisq") 

#NOTE - HOUR + QUARTER has a much higher LR stat than HOUR + Q1_IND. Will use this going forward.

#Current model is HOUR + QUARTER, now we test out DAY_OF_WEEK and SUNDAY_IND
glm_current <- glm_hr_quarter
glm_dayofweek <- glm.nb(COUNT ~ HOUR + QUARTER + DAY_OF_WEEK, data = train_df)
glm_sundayind <- glm.nb(COUNT ~ HOUR + QUARTER + SUNDAY_IND, data = train_df)

compareGLM(glm_current, glm_dayofweek, glm_sundayind) 
compare_performance(glm_current, glm_dayofweek, glm_sundayind, rank = TRUE, verbose = FALSE) 

#NOTE - day of week is barely better than sunday ind
anova(glm_current, glm_dayofweek, test = "Chisq") 
anova(glm_current, glm_sundayind, test = "Chisq") 

#NOTE - DAY_OF_WEEK outperforms SUNDAY_IND statistically, so this is our final frequency model. 
#     - Lower AIC indicates the additional variables/model complexity is worth it.

#FINAL FREQUENCY MODEL
frequency_model <- glm_sundayind

#Clean house
rm(m, var, fit_pois, fit_negbin)
rm(glm_poi, glm_qpoi, glm_negbin,glm_hour, glm_quarter, glm_q1ind,glm_q3ind, glm_month)
rm(glm_hr_quarter,glm_hr_q1ind,glm_hr_q3ind,glm_current, glm_dayofweek, glm_sundayind)


#Test model on validation data to check for overfiting
val_summary <- summary(update(frequency_model, data = val_df))
pseudo_r2 <- 1 - (val_summary$deviance/val_summary$null.deviance) #Equals 20%

#NOTE - an R2 of around 20% is considered significant in social sciences because human behavior is hard to predict
