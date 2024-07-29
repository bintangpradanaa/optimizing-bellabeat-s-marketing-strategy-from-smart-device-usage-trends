#==============================================================#
# PREPARE
#==============================================================#
# Install Packages
install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("ggrepel")


# Import Library
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)

#import data
daily_activity <- read_csv("C:/Users/binta/OneDrive/Dokumen/Project DA/Bellabeat Analysis/dataset/dailyActivity_merged.csv")
daily_sleep <- read.csv("C:/Users/binta/OneDrive/Dokumen/Project DA/Bellabeat Analysis/dataset/sleepDay_merged.csv")
weight_info <- read.csv("C:/Users/binta/OneDrive/Dokumen/Project DA/Bellabeat Analysis/dataset/weightLogInfo_merged.csv")
hourly_calories <- read_csv("C:/Users/binta/OneDrive/Dokumen/Project DA/Bellabeat Analysis/dataset/hourlyCalories_merged.csv")
hourly_intensities <- read_csv("C:/Users/binta/OneDrive/Dokumen/Project DA/Bellabeat Analysis/dataset/hourlyIntensities_merged.csv")
hourly_steps <- read_csv("C:/Users/binta/OneDrive/Dokumen/Project DA/Bellabeat Analysis/dataset/hourlySteps_merged.csv")

#==============================================================#
# PROCESS
#==============================================================#
# checking type of data
str(daily_activity)
str(daily_sleep)
str(weight_info)
str(hourly_calories)
str(hourly_intensities)
str(hourly_steps)

# change data type to datetime
daily_activity$Date <- as.POSIXct(daily_activity$ActivityDate, format = "%m/%d/%Y")
daily_sleep$Date <- as.POSIXct(daily_sleep$SleepDay, format = "%m/%d/%Y")
weight_info$Date <- as.POSIXct(weight_info$Date, format = "%m/%d/%Y %H:%M:%S")

hourly_calories$ActivityHour <- as.POSIXct(hourly_calories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourly_intensities$ActivityHour <- as.POSIXct(hourly_intensities$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourly_steps$ActivityHour <- as.POSIXct(hourly_steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

# change data type of Id to character
daily_activity$Id <- as.character(daily_activity$Id)
daily_sleep$Id <- as.character(daily_sleep$Id)
weight_info$Id <- as.character(weight_info$Id)

hourly_calories$Id <- as.character(hourly_calories$Id)
hourly_intensities$Id <- as.character(hourly_intensities$Id)
hourly_steps$Id <- as.character(hourly_steps$Id)

# merge the data
daily_activity <- merge(
  daily_activity %>% select(-ActivityDate),
  daily_sleep %>% select(-SleepDay),
  by = c('Id', 'Date'),
)

hourly_activity <- merge(
  merge(hourly_calories, hourly_intensities, by = c('Id', 'ActivityHour')),
  hourly_steps, by = c('Id', 'ActivityHour')
)

head(daily_activity)
head(hourly_activity)
head(weight_info)

# adding new features
daily_activity <- daily_activity %>%
  mutate(day_of_week = lubridate::wday(Date, label = TRUE, abbr = TRUE))
hourly_activity <- hourly_activity %>%
  mutate(ActivityTime = format(ActivityHour, "%H:%M:%S"))
hourly_activity <- hourly_activity %>%
  mutate(hour = hour(ActivityHour))


# checking for null values
sapply(daily_activity, function(x) sum(is.na(x)) / length(x)) * 100
sapply(hourly_activity, function(x) sum(is.na(x)) / length(x)) * 100
sapply(weight_info, function(x) sum(is.na(x)) / length(x)) * 100


# Remove duplicate rows
daily_activity_cl <- daily_activity[!duplicated(daily_activity), ]
hourly_activity <- hourly_activity[!duplicated(hourly_activity), ]
weight_info <- weight_info[!duplicated(weight_info), ]

# creating a copy dataset
daily <- daily_activity_cl
hourly <- hourly_activity
weight <- weight_info


#==============================================================#
# ANALYZE AND SHARE
#==============================================================#

# EDA
# statistic descriptive
daily %>%
  select(TotalSteps,
         TotalDistance,
         Calories) %>%
  summary()

daily %>%
  select(VeryActiveDistance,
         ModeratelyActiveDistance,
         LightActiveDistance,
         SedentaryActiveDistance) %>%
  summary()

daily %>%
  select(VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes,
         SedentaryMinutes) %>%
  summary()

daily %>%
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

weight %>%
  select(WeightKg, 
         BMI) %>%
  summary()


#==============================================================#

# Trends

# daily trends
step_calories_distance_trend <- daily %>%
  pivot_longer(cols = c(TotalSteps, Calories, TotalDistance),
               names_to = "Metric",
               values_to = "Value")

ggplot(step_calories_distance_trend, aes(x = as.Date(Date), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  labs(title = "Daily Trends",
       x = "Date",
       y = "Value",
       caption = "Data Source: FitBit Fitness Tracker Data") +
  scale_fill_manual(values = c("TotalSteps" = "pink", 
                               "Calories" = "darkred", 
                               "TotalDistance" = "maroon")) +
  theme_minimal()
  
#==============================================================#

# Average steps by day of the Week
total_steps_by_weekly <- daily_activity %>%
  group_by(day_of_week) %>%
  summarize(TotalSteps = round(mean(TotalSteps, na.rm = TRUE), 2)) %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

ggplot(total_steps_by_weekly, aes(x = day_of_week, y = TotalSteps, fill = TotalSteps)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = TotalSteps), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Average Daily Steps by Day of the Week",
       x = "Day of the Week",
       y = "Average Total Steps",
       caption = "Data Source: FitBit Fitness Tracker Data") +
  scale_fill_gradient(low = "pink", high = "darkred") +
  theme_minimal()

# total active minutes by day of the Week
# total active minutes by day of the week
total_active_by_week <- daily %>%
  group_by(day_of_week) %>%
  summarize(
    light_active = sum(LightlyActiveMinutes),
    moderate_active = sum(FairlyActiveMinutes),
    very_active = sum(VeryActiveMinutes)
  ) %>%
  pivot_longer(cols = c(light_active, moderate_active, very_active), 
               names_to = "ActivityLevel", 
               values_to = "Minutes") %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

total_active_by_week$ActivityLevel <- factor(total_active_by_week$ActivityLevel,
                                             levels = c("light_active", "moderate_active", "very_active", "sedentary"))

ggplot(total_active_by_week, aes(x = day_of_week, y = Minutes, fill = ActivityLevel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Weekly Active Minutes by Activity Level",
       x = "Day of the Week",
       y = "Minutes",
       fill = "Activity Level",
       caption = "Data Source: FitBit Fitness Tracker Data") +
  scale_fill_manual(values = c("light_active" = "salmon", 
                               "moderate_active" = "firebrick", 
                               "very_active" = "darkred")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))
        
#==============================================================#

# average of calories burned & active minutes by day of the Week
hourly %>%
  pivot_longer(cols = c(StepTotal, Calories), 
               names_to = "metric", 
               values_to = "value") %>%
  group_by(hour, metric) %>%
  summarize(average = mean(value)) %>%
  ggplot(aes(x = hour, y = average, color = metric)) +
  geom_line(size = 1) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) + 
  labs(title = "Average Steps and Calories by Hour", 
       x = "Hour", 
       y = "Average",
       caption = "Data Source: FitBit Fitness Tracker Data") +
  scale_x_continuous(breaks = 1:23, labels = paste0(1:23)) +
  scale_color_manual(values = c("StepTotal" = "salmon", 
                                "Calories" = "darkred")) +
  theme_minimal()

#==============================================================#

# Correlation

# total steps and total distance vs. total calories
daily_activity %>%
  pivot_longer(cols = c(TotalSteps, TotalDistance), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, y = Calories)) + 
  geom_point(color = 'salmon') +
  geom_smooth(method = 'gam', formula = y ~ s(x), color = 'darkred') + 
  facet_wrap(~ Variable, scales = "free_x") +
  labs(title='Total Steps and Total Distance vs. Total Calories', 
       x = 'Value', 
       y = 'Calories',
       caption = "Data Source: FitBit Fitness Tracker Data") +
  theme_minimal()

# comparing distance types vs total calories
daily_active_distance <- daily %>%
  pivot_longer(cols = c(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes), 
               names_to = "ActiveDistanceType", 
               values_to = "ActiveDistance")

ggplot(data = daily_active_distance, aes(x = ActiveDistance, y = Calories)) + 
  geom_point(color = 'salmon') +   
  geom_smooth(method = 'lm', color = 'darkred', se = TRUE) +  
  labs(title = 'Active Distance Type vs. Total Calories', 
       x = 'Distance', 
       y = 'Total Calories',
       caption = "Data Source: FitBit Fitness Tracker Data") +
  facet_wrap(~ ActiveDistanceType, scales = "free") +  
  theme_minimal()


# Comparing Active Minutes Type vs Total Calories
daily_active_minutes <- daily %>%
  pivot_longer(cols = c(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes), 
               names_to = "ActiveMinutesType", 
               values_to = "ActiveMinutes")

ggplot(data = daily_active_minutes, aes(x = ActiveMinutes, y = Calories)) + 
  geom_point(color = 'salmon') +    
  geom_smooth(method = 'lm', color = 'darkred', se = TRUE) +  
  labs(title = 'Active Minutes Type vs. Total Calories', 
       x = 'Active Minutes', 
       y = 'Total Calories',
       caption = "Data Source: FitBit Fitness Tracker Data") +
  facet_wrap(~ ActiveMinutesType, scales = "free") +  
  theme_minimal()


#==============================================================#

# Sleep Analysis

sleep_class_data <- daily %>%
  mutate(TotalActiveMinutes = rowSums(across(c(LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes))),
         SleepEfficiency = TotalMinutesAsleep / TotalTimeInBed * 100)

# Categorizing sleep efficiency and activity levels
sleep_class_data %>% 
  select(SleepEfficiency, TotalActiveMinutes) %>% 
  mutate(sleep_efficiency_category = case_when(
    SleepEfficiency < 85 ~ 'Poor',
    SleepEfficiency <= 90 ~ 'Optimal',
    TRUE ~ 'Excellent'
  )) %>%
  mutate(active_level = case_when(
    TotalActiveMinutes > 150 ~ 'High activity',
    TotalActiveMinutes >= 60 ~ 'Moderate activity',
    TRUE ~ 'Low activity'
  )) %>%
  select(-c(SleepEfficiency, TotalActiveMinutes)) %>% 
  drop_na() %>% 
  group_by(sleep_efficiency_category, active_level) %>% 
  summarise(counts = n(), .groups = 'drop') %>% 
  mutate(active_level = factor(active_level, 
                               levels = c('Low activity', 'Moderate activity', 'High activity')),
         sleep_efficiency_category = factor(sleep_efficiency_category, 
                                            levels = c('Poor', 'Optimal', 'Excellent'))) %>% 
  ggplot(aes(x = sleep_efficiency_category, 
             y = counts, 
             fill = sleep_efficiency_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("salmon", "red", "darkred")) +
  facet_wrap(~active_level, nrow = 1) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(colour = 'black', size = 8),
        strip.background = element_rect(fill = "antiquewhite", color = 'black')) +
  labs(
    title = "Sleep Efficiency by Activity Level",
    x = "Sleep Efficiency Category",
    y = "Count",
    caption = 'Data Source: FitBit Fitness Tracker Data')


# sleep quality (duration) by level of activity
sleep_class_data %>% 
  select(TotalMinutesAsleep, TotalActiveMinutes ) %>% 
  mutate(sleep_quality = case_when(
    TotalMinutesAsleep < 360 ~ 'Poor',
    TotalMinutesAsleep <= 540 ~ 'Optimal',
    TRUE ~ 'Excess'
  )) %>%
  mutate(active_level = case_when(
    TotalActiveMinutes > 150 ~ 'High activity',
    TotalActiveMinutes >= 60 ~ 'Moderate activity',
    TRUE ~ 'Low activity')) %>%
  select(-c(TotalMinutesAsleep, TotalActiveMinutes)) %>% 
  drop_na() %>% 
  group_by(sleep_quality, active_level) %>% 
  summarise(counts = n(), .groups = 'drop') %>% 
  mutate(active_level = factor(active_level, 
                               levels = c('Low activity', 'Moderate activity', 'High activity')),
         sleep_quality = factor(sleep_quality, 
                                levels = c('Poor','Optimal','Excess'))) %>% 
  ggplot(aes(x = sleep_quality, 
             y = counts, 
             fill = sleep_quality)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("salmon", "darkred", "maroon")) +
  facet_wrap(~active_level, nrow = 1) +
  theme(legend.position = "none") +
  theme(strip.text = element_text(colour = 'black', size = 8)) +
  theme(strip.background = element_rect(fill = "antiquewhite", color = 'black')) +
  labs(
    title = "Sleep Quality (Duration) by Activity Level",
    x = "Duration of Sleep",
    y = "Count",
    caption = 'Data Source: FitBit Fitness Tracker Data')

# quality of sleep by day of the Week
sleep_class_data %>%
  mutate(sleep_quality = case_when(
    TotalMinutesAsleep < 360 ~ 'Poor',
    TotalMinutesAsleep <= 540 ~ 'Optimal',
    TRUE ~ 'Excess'
  )) %>%
  group_by(day_of_week, sleep_quality) %>%
  summarise(counts = n(), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = counts, fill = sleep_quality)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  c("maroon", "darkred", "salmon")) +
  theme(legend.position = "right") +
  labs(
    title = "Quality of Sleep by Day of the Week",
    x = "Day of the Week",
    y = "Count",
    fill = "Sleep Quality",
    caption = 'Data Source: FitBit Fitness Tracker Data')

sleep_class_data %>%
  mutate(sleep_efficiency = TotalMinutesAsleep / TotalTimeInBed) %>%
  mutate(sleep_efficiency_level = case_when(
    sleep_efficiency > 0.85 ~ 'High Efficiency (85% or more)',
    sleep_efficiency >= 0.70 ~ 'Moderate Efficiency (70% - 85%)',
    TRUE ~ 'Low Efficiency (Less than 70%)'
  )) %>%
  group_by(sleep_efficiency_level) %>%
  summarise(counts = n(), .groups = 'drop') %>%
  mutate(percentage = counts / sum(counts) * 100) %>%
  ggplot(aes(x = "", y = percentage, fill = sleep_efficiency_level)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("darkred", "salmon", "maroon")) +
  theme_void() +
  labs(
    title = "Pie Chart of Sleep Efficiency",
    fill = "Sleep Efficiency Level",
    caption = 'Data Source: FitBit Fitness Tracker Data') +
  geom_text_repel(aes(label = paste0(round(percentage, 2), "%")), 
                  position = position_stack(vjust = 0.5), 
                  size = 4, 
                  color = 'white',
                  show.legend = FALSE)

#==============================================================#

# calculate distribution of each BMI category
bmi_distribution <- weight %>%
  mutate(BMI_Category = case_when(
    BMI < 18.5 ~ 'Underweight',
    BMI >= 18.5 & BMI < 24.9 ~ 'Normal weight',
    BMI >= 25 & BMI < 29.9 ~ 'Overweight',
    BMI >= 30 ~ 'Obesity'
  )) %>%
  count(BMI_Category) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(bmi_distribution, aes(x = "", y = Percentage, fill = BMI_Category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Underweight" = "pink", 
                               "Normal weight" = "salmon", 
                               "Overweight" = "maroon", 
                               "Obesity" = "darkred")) +
  labs(title = "BMI Category Distribution", 
       fill = "BMI Category", 
       y = "Percentage",
       caption = 'Data Source: FitBit Fitness Tracker Data') +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4, 
            color = "white")


# frequency of manual report
weight %>%
  ggplot(aes(x = IsManualReport, fill = IsManualReport)) +
  geom_bar(fill = 'darkred') +
  labs(
    title = "Frequency of Manual Report", 
    x = "Manual Report", 
    y = "Frequency",
    caption = 'Data Source: FitBit Fitness Tracker Data'
  ) +
  geom_text(stat = 'count', 
            aes(label = ..count..), 
            vjust = -0.5, 
            size = 4, 
            color = 'black') +
  theme_minimal()
 
