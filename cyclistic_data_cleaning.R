# Load necessary libraries
library(tidyverse)
library(lubridate)
library(hms)
library(skimr)
library(dplyr)
library(scales)
library(cluster)
# ==============================================================
# Data Cleaning
# ==============================================================

# Load the data
c24_05 <- read.csv("202405-divvy-tripdata.csv")
c24_06 <- read.csv("202406-divvy-tripdata.csv")
c24_07 <- read.csv("202407-divvy-tripdata.csv")
c24_08 <- read.csv("202408-divvy-tripdata.csv")
c24_09 <- read.csv("202409-divvy-tripdata.csv")
c24_10 <- read.csv("202410-divvy-tripdata.csv")

# vertically merge dataset from 6 months
bike_trips <- bind_rows(c24_05, c24_06, c24_07, c24_08, c24_09, c24_10)

# drop rows with NA and duplicate values
bike_trips_1 <- drop_na(bike_trips) %>% 
  distinct()

# summary for the data
skim_without_charts(bike_trips_1) 

# Convert columns to appropriate data types
bike_trips_1 <- bike_trips_1 %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    member_casual = as.factor(member_casual),
    rideable_type = as.factor(rideable_type))

# filter out data that does not match the month and year of the analysis report
bike_trips_1 <- bike_trips_1 %>%
  filter(year(started_at) == 2024 & month(started_at) %in% 5:10,
         rowSums(is.na(.)) == 0) 

# verifying data mutation 
bike_trips_1 %>% 
  select(started_at) %>% 
  head()

bike_trips_1 %>% 
  select(started_at) %>% 
  tail()

# calculate ride duration in seconds
bike_trips_2 <- bike_trips_1 %>% 
  mutate(
    ride_duration = as.numeric(difftime(ended_at, 
                                        started_at, units = "secs")))

# remove rides with duration less than 60 seconds (considered invalid)
bike_trips_2 <- bike_trips_2 %>% 
  filter(ride_duration >= 60)

# separate date, time, and cleaned data
bike_trips_2 <- bike_trips_2 %>% 
  mutate( 
    started_date = as.Date(started_at),
    started_time = format(started_at, "%H:%M:%S"),
    ended_date = as.Date(ended_at),
    ended_time = format(ended_at, "%H:%M:%S")) %>%
  mutate( # removing milisecond on started_time and ended_time
    started_time = sub("\\.\\d{3}$", "", started_time),
    ended_time = sub("\\.\\d{3}$", "", ended_time))

# verifying data mutation 
bike_trips_2 %>% 
  select(started_time) %>% 
  head()

bike_trips_2 %>% 
  select(started_time) %>% 
  tail()

# remove unnecessary columns
bike_trips_2 <- bike_trips_2 %>% 
  select(-start_lat, -start_lng, -end_lat, -end_lng)

# mutate data to create days of the week and month
bike_trips_3 <- bike_trips_2 %>% 
  mutate( 
    started_day = wday(started_date, label = TRUE, week_start =  1),
    ended_day = wday(ended_date, label = TRUE, week_start = 1),
    month_of_ride =  month(started_date, label = TRUE, abbr = FALSE)) # mutate the data to include month people using the bike

# filter out of range month
bike_trips_3 <- bike_trips_3 %>%
  filter(month_of_ride %in% c("May", "June", "July", "August", 
                              "September", "October"))

# standardized text data
bike_trips_3 <- bike_trips_3 %>% 
  mutate(
    start_station_name = trimws(tolower(start_station_name)),
    end_station_name = trimws(tolower(end_station_name)))

# Handling outliers in ride duration
ride_duration_summary <- bike_trips_3 %>%
  summarise(
    min_duration = min(ride_duration),
    max_duration = max(ride_duration),
    mean_duration = mean(ride_duration),
    sd_duration = sd(ride_duration))

# Remove outliers based on standard deviation
bike_trips_3_out <- bike_trips_3 %>% 
  filter(ride_duration >= ride_duration_summary$min_duration,
         ride_duration >= ride_duration_summary$mean_duration + 3
         * ride_duration_summary$sd_duration)

# Data type check
str(bike_trips_3$ride_duration)
str(bike_trips_3$started_time)

# Format date and time
bike_trips_3 <- bike_trips_3 %>% 
  mutate(
    started_time = hms:: as_hms(started_time),
    ended_time = hms:: as_hms(ended_time))

# Additional features quantifying duration and time of started
bike_trips_4 <- bike_trips_3 %>% 
  mutate(
    ride_duration_category = case_when(
      ride_duration < 600 ~ "short",
      ride_duration < 1800 ~ "medium",
      TRUE ~ "long"),
    time_of_day = case_when(
      hour(started_time) < 6 ~ "night",
      hour(started_time) < 12 ~ "morning",
      hour(started_time) < 18 ~ "afternoon",
      TRUE ~ "evening"))

# Renaming column
bike_trips_4 <- bike_trips_4 %>% 
  rename(
    user_type = member_casual,
    trip_category = ride_duration_category,
    bike_type = rideable_type)

# Identify rows with missing values
missing_rows <- bike_trips_4[!complete.cases(bike_trips_4), ]

# Identify duplicate rows
duplicate_rows <- bike_trips_4[duplicated(bike_trips_4), ]

# Remove rows with blank values
bike_trips_5 <- bike_trips_4 %>%
  filter(!(start_station_name %in% c("", " ", "NA") |
             start_station_id %in% c("", " ", "NA") |
             end_station_name %in% c("", " ", "NA") |
             end_station_id %in% c("", " ", "NA")))

# Remove rows with inconsistencies
bike_trips_5 <- bike_trips_5 %>%
  filter(started_at < ended_at)
