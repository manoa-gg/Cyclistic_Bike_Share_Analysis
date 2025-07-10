# 🚀 Business Task
# The goal of this analysis is to understand the usage patterns of Cyclistic bikes by different customer types (members vs. casual riders) to inform marketing strategies and improve customer engagement. Specifically, we aim to:
# - Identify trends in ride duration and frequency.
# - Determine the most popular times and days for bike usage.
# - Compare the behavior of members and casual riders.

# ==============================================================
# Data Profiling
# ==============================================================

# Number of rows and columns
dim(bike_trips_5)

# Structure of data frame
str(bike_trips_5)

# Comprehensive summary statistics
skim(bike_trips_5)

# Summarize missing values
sum(is.na(bike_trips_5))
colSums(is.na(bike_trips_5))

# Distribution of ride duration
summary(bike_trips_5$ride_duration)

# Distribution of start times
table(bike_trips_5$time_of_day)

# Distribution of user types
table(bike_trips_5$user_type)

# Frequency of categorical variables
count(bike_trips_5, user_type)
count(bike_trips_5, bike_type)
count(bike_trips_5, started_day)
count(bike_trips_5, month_of_ride)

# Identify outliers in ride duration
quantile(bike_trips_5$ride_duration, probs = c(0.01, 0.99))

# Filter short rides
short_rides <- bike_trips_5[bike_trips_5$ride_duration < 100, ]

# Correlation matrix for numerical variables
cor(bike_trips_5 %>%  select(where(is.numeric)))

# Recheck data consistency
sum(bike_trips_5$started_at > bike_trips_5$ended_at)

# Remove unnecessary dataframe
rm(blank_check, c24_05, c24_06, c24_07, c24_08, c24_09, c24_10, clean_rows, missing_rows, blank_rows)

# summary for the data
skim_without_charts(bike_trips_5)

# ==============================================================
# Data Aggregation
# ==============================================================

# Aggregation by user type
avg_ride_duration_by_user <- bike_trips_5 %>% 
  group_by(user_type) %>% 
  summarise(avg_duration = mean(ride_duration, na.rm = TRUE))

# Aggregation by time of day
rides_by_time_of_day <- bike_trips_5 %>% 
  count(time_of_day)

# Aggregation by month
rides_by_month <- bike_trips_5 %>% 
  count(month_of_ride)

# Aggregation by weekday
rides_by_day <- bike_trips_5 %>% 
  count(user_type, started_day)

# Aggregation of ride duration | start by create mode function
get_mode <- function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) return(NA)
  freq <- table(x_clean)
  as.numeric(names(freq)[which.max(freq)])  # Convert to number
}

overview_ride_duration <- bike_trips_5 %>% 
  summarise(
    total_duration = sum(ride_duration, na.rm = TRUE),
    average_duration = mean(ride_duration, na.rm = TRUE),
    median_duration = median(ride_duration, na.rm = TRUE),
    mode_duration = get_mode(ride_duration),
    min_duration = min(ride_duration, na.rm = TRUE),
    max_duration = max(ride_duration, na.rm = TRUE),
    sd_duration = sd(ride_duration, na.rm = TRUE))

# Aggregation by station
count_station <- bike_trips_5 %>% 
  group_by(start_station_name, end_station_name) %>% 
  summarise(
    count_trips = n(), .groups = 'drop') %>% 
  arrange(start_station_name, desc(count_trips))

# Aggregation by bike type
avg_bike_type_duration <- bike_trips_5 %>% 
  group_by(bike_type) %>% 
  summarise(avg_duration = mean(ride_duration, na.rm = TRUE))


# ==============================================================
# Exploratory Visualization
# ==============================================================

# Ride duration distribution
ggplot(bike_trips_5, aes(x = ride_duration/60)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of Ride Durations (Minutes)",
       x = "Duration (Min)", y = "Count") +
  xlim(0, 120) + 
  scale_y_continuous(labels = scales :: comma)

# Rides by Hour of Day
bike_trips_5 %>% 
  mutate(hour = hour(started_time)) %>% 
  count(hour) %>% 
  ggplot(aes(x = hour, y = n)) + 
  geom_line(color = "tomato", linewidth =1) +
  labs(title = "Rides by Hour of Day", x = "Hour", 
       y = "Number of Rides") +
  scale_y_continuous(labels = comma)

# Number of rides by day of week
ggplot(
  data = bike_trips_5 %>%
    mutate(day = started_day, label = TRUE, week_start = 1) %>% 
    count(user_type, day), 
  aes(x = day, y= n, fill = user_type)) +
  geom_col(position = position_dodge(preserve = "single")) +
  geom_text(
    aes(label = ifelse(n >= 1000, paste0(
      format(round(n/1000, 2), nsmall = 2), 
      "K"), paste(n))),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("casual" = "#F8766D", "member" = "#00BFC4"),
    labels = c("casual", "member")) +
  labs(title = "Rides by Day of Week",
       x = NULL,
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top") +
  scale_y_continuous(labels = function(x) {
    ifelse(x >= 1000, 
           paste0(format(round(x/1000, 2), nsmall = 2), "K"),
           as.character(x))
  },
  expand = expansion(mult = c(0, 0.1)))

# Heatmap: Ride frequency by time and day
heatmap_data <- bike_trips_5 %>% 
  mutate(
    hour = hour(started_time),
    day = started_day, label = TRUE, week_start = 1) %>% 
  count(user_type, day, hour)

ggplot(heatmap_data, aes(x = hour, y = day, fill = n)) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_wrap(~user_type) +
  scale_fill_gradient(
    low = "#f7fbff",
    high = "#08306b",
    labels = label_number(scale = 1/1000, suffix = "K",
                          accuracy = 0.1)) +
  labs(
    title = "Heatmap: Ride frequency by time and day",
    x = "Hour of day",
    y = NULL,
    fill = "Rides (K)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"))

# ==============================================================
# Enhanced Analysis
# ==============================================================

# Segmentation Analysis
# Pattern Visualizing
bike_trips_5 %>% 
  filter(user_type == "member") %>% 
  mutate(
    is_commute = hour(started_time) %in% c(6:9, 15:19),
    is_weekday = started_day %in% c("mon", "tue", "wed", "thu", "fri")) %>% 
  count(is_commute, is_weekday) %>% 
  ggplot(aes(x = is_weekday, y = n, fill = is_commute)) +
  geom_col(position = "dodge") +
  labs(title = "Member Rides: Commute vs. Weekday Patterns")

bike_trips_5 %>% 
  filter(user_type == "member") %>%
  count(trip_category, time_of_day) %>% 
  ggplot(aes(x = trip_category, y = n, fill = time_of_day)) +
  geom_col(position = "dodge") +
  labs(title = "Member Rides: Trip duration vs Time of the day") +
  scale_y_continuous(labels = scales :: comma)

# Member profiling
member_behavior <- bike_trips_5 %>% 
  filter(user_type == "member", 
         started_day %in% c("mon", "tue", "wed", "thu", "fri"),
         started_time %in% c(6:9, 15:19),
         trip_category == "short") %>% 
  summarise(
    top_start_stations = list(bike_trips_5 %>%
                                filter(user_type == "member") %>% 
                                count(start_station_name) %>% 
                                filter(n >= 300) %>% 
                                pull(start_station_name)),
    top_end_stations = list(bike_trips_5 %>%
                              filter(user_type == "member") %>% 
                              count(end_station_name) %>% 
                              filter(n >= 300) %>% 
                              pull(end_station_name)))

# I'm using weekday data because this dataset does not have unique customer ID instead
# it only have ride_id which are unique to every ride
# so I'm trying to match which one is suitable target on ride by ride basis

# Member thresholds
member_stations <- list(
  start = member_behavior$top_start_stations[[1]],
  end = member_behavior$top_end_stations[[1]])

# Time windows
member_weekdays <- c("mon", "tue", "wed", "thu", "fri")
member_commute_hours <- c(6:9, 15:19)

# Subset of casual users matching member patterns
# strict result in 0
target_casuals <- bike_trips_5 %>% 
  filter(
    user_type == "casual",
    started_day %in% member_weekdays,
    started_time %in% member_commute_hours,
    trip_category == "short",
    end_station_name %in% member_stations$end,
    start_station_name %in% member_stations$start)

# less strict filter result shown 210699 obs
target_casuals <- bike_trips_5 %>% 
  filter(
    user_type == "casual",
    trip_category == "short",
    hour(started_time) %in% member_commute_hours,
    end_station_name %in% member_stations$end,
    start_station_name %in% member_stations$start)

# Targets by station popularity
target_station_scored <- target_casuals %>% 
  group_by(start_station_name) %>% 
  summarise(
    n_rides = n(),
    n_member_rides = first(
      bike_trips_5 %>% 
        filter(start_station_name %in% member_stations$start) %>% 
        count(start_station_name) %>% 
        pull(n)), .groups = "drop") %>% 
  mutate(station_score = n_rides * n_member_rides) %>% 
  arrange(desc(station_score))

# Final segmentation subset
top_targets <- target_casuals %>% 
  semi_join(target_station_scored %>% 
              head(50), by = "start_station_name") %>% 
  select(everything())

## % of casual subset vs entire casual population
# calculate percentages
casual_comparison <- bike_trips_5 %>% 
  filter(user_type == "casual") %>% 
  summarise(
    total_casual = n(),
    target_casual = sum(
      trip_category == "short" &
        hour(started_time) %in% c(6:9, 15:19) &
        start_station_name %in% member_stations$start &
        end_station_name %in% member_stations$end),
    pct_target = target_casual/ total_casual)

# Viz proper grouping
viz_data <- data.frame(
  group = c("Target Casual", "Other Casual"),
  value = c(casual_comparison$pct_target, 1 - casual_comparison$pct_target))

# Viz
ggplot(viz_data, aes(x = "", y = value, fill = group)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = percent(value, accuracy = 0.1)),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Casual Riders: Member-Like vs Others",
       fill = "Group") +
  scale_fill_manual(values = c("#4285F4", "#EA4335")) +
  theme_void()

## Top target stations
# Prepare comparison data
top_station_viz <- target_station_scored %>% 
  head(50) %>% 
  mutate(label = paste0(start_station_name, "\n",
                        "Casual Rides: ", comma(n_rides), "\n",
                        "Member Rides: ", comma(n_member_rides)))

# Lollipop chart
top_station_viz %>% 
  arrange(desc(station_score)) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(start_station_name, station_score),
             y = station_score)) +
  geom_segment(aes(xend = start_station_name, yend = 0), 
               color = "grey50") +
  geom_point(aes(size = n_rides, color = n_member_rides), alpha = 0.8) +
  geom_text(aes(label = scales :: comma(station_score)),
            hjust = -0.2) +
  coord_flip() +
  labs(
    title = "Top 20 Conversions Target Stations",
    subtitle = "Size = Casual rides | Member Rides",
    x = NULL,
    y = "Priority Score (Casual Rides x Member Rides)") +
  scale_color_gradient(low = "#FBBC05", high = "#EA4335") + 
  scale_size_continuous(range = c(3,10)) +
  theme_minimal()

## Member-like casual rider profile
# Cluster casual riders
casual_cluster <- bike_trips_5 %>% 
  filter(user_type == "casual") %>% 
  mutate(
    is_commute = hour(started_time) %in% c(6:9, 15:19),
    is_weekday = started_day %in% c("mon", "tue", "wed", "thu", "fri"),
    is_short_trip = trip_category == "short") %>% 
  select(is_commute, is_weekday, is_short_trip) %>% 
  kmeans(centers = 3)

# Add clusters back to data
bike_trips_5$cluster <- ifelse(bike_trips_5$user_type == "casual",
                               casual_cluster$cluster, NA)

# Visualize clusters
ggplot(bike_trips_5 %>% 
         filter(!is.na(cluster)),
       aes(x = started_day, fill = factor(cluster))) +
  geom_bar(position = "dodge") +
  facet_wrap(~time_of_day) +
  labs(title = "Casual Rider Clusters by Time Patterns",
       x = "Day of Week",
       fill = "Cluster") +
  scale_fill_manual(values = c("#ff9999", "#ff6666", "#ff0000"))

# ============================================
# Key Findings and Strategic Recommendations
# ============================================

# Key findings:
# 1. Casual users tend to have longer rides and use the service more frequently on weekends.
# 2. While annual members have shorter rides and consistent usage throughout the week
# 3. Peak usage times and days differ significantly between the two groups of user type

# Strategic Recommendations:
# Design campaigns targeting casual users who have usage patterns similar to members
# Offer incentives such as discounts or free rides to high-potential casual users identified by clustering analysis
# Use social media and email to reach casual riders during peak weekday times

# ==============================================
# Usage pattern visualization
# Compare average ride duration between casual and member users
avg_ride_duration_by_user <- bike_trips_5 %>% 
  group_by(user_type) %>% 
  summarise(avg_duration = mean(ride_duration, na.rm = TRUE))

# Plot average ride duration by user type
ggplot(avg_ride_duration_by_user, aes(x = user_type, y = avg_duration, 
                                      fill = user_type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Ride Duration by User Type",
    x = "User Type",
    y = "Average Ride Duration (seconds)") +
  scale_fill_manual(values = c("casual" = "#f8766d", "member" = "#00bfc4"))

# Plot rides by day of the week
rides_by_day <- bike_trips_5 %>% 
  count(user_type, started_day)

ggplot(rides_by_day, aes(x = started_day, y = n, fill = user_type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Rides by Day of the Week",
    x = "Day of the Week",
    y = "Number of Rides") +
  scale_fill_manual(values = c("casual" = "#f8766d", "member" = "#00bfca")) +
  scale_y_continuous(labels = comma)

# =================================================
# Targeted Marketing Campaigns
# Identify casual users who show weekday usage patterns similar to member
target_casuals_1 <- bike_trips_5 %>% 
  filter(
    user_type == "casual",
    trip_category == "short",
    hour(started_time) %in% member_commute_hours,
    start_station_name %in% member_stations$start)

# Calculate percentage of target casual users
casual_comparison_1 <- bike_trips_5 %>% 
  filter(user_type == "casual") %>% 
  summarise(
    total_casual = n(),
    target_casual = nrow(target_casuals_1),
    pct_target = target_casual/ total_casual)

# Visualize the percentage of target casual riders
viz_data_1 <- data.frame(
  group = c("Target Casual", "Other Casual"),
  value = c(casual_comparison_1$pct_target, 1 - 
              casual_comparison_1$pct_target))

# Actual Visualization
ggplot(viz_data_1, aes(x = "", y = value, fill = group)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = percent(value, accuracy = 0.1)),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Casual Riders: Member-Like vs Others",
       fill = "Group") +
  scale_fill_manual(values = c("#4285F4", "#EA4335")) +
  theme_void()

# ===================================================

# Personalized Offers
# Offer incentives to high potential casual riders
# Example: Discount or free rides for a limited period
# This can be implemented through email campaigns or in-app notifications

# ===================================================

# Optimized Communication Channels
# Use social media and email to reach casual users during peak weekday times
# Example : Scheduled social media posts and email campaigns targeting weekdays

# ===================================================

# Summary and Conclusion
# Analysis has provided actionable insights into the usage patterns of casual users and members.
# These insights can inform targeted marketing strategies to convert casual users into members
# Future work could include more detailed segmentation and advanced predictive modeling to further refine marketing efforts