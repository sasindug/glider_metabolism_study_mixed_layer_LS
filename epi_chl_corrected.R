### Chlorophyll A concentration at nigh calculations

# Libraries used
library(readr)
library(dplyr)
library(rLakeAnalyzer)
library(tidyr) 
library(ggpubr)
library(lubridate)


## Set the WD##
setwd("C:/Users/sasin/Sync/Sasindu/Trent Metabolism/Lake Superior/Outputs/Input tables")

####################     2014_09_03  #######################
### Input data
in2014_09_03 <- read_csv('input.data_2014_09_03.csv')


### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2014_09_03 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2014_09_03, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2014_09_03 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2014_09_03 %>%
  left_join(epilim_depth_2014_09_03, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2014_09_03 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))

####################     2014_09_12  #######################
### Input data
in2014_09_12 <-read_csv('input.data_2014_09_12.csv')
### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2014_09_12 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2014_09_12, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2014_09_12 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2014_09_12 %>%
  left_join(epilim_depth_2014_09_12, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2014_09_12 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2014_09_25  #######################
### Input data
in2014_09_25 <-read_csv('input.data_2014_09_25.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2014_09_25 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2014_09_25, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2014_09_25 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2014_09_25 %>%
  left_join(epilim_depth_2014_09_25, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2014_09_25 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2014_10_30  #######################
### Input data
in2014_10_30 <-read_csv('input.data_2014_10_30.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2014_10_30 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2014_10_30, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2014_10_30 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2014_10_30 %>%
  left_join(epilim_depth_2014_10_30, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2014_10_30 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2015_07_01  #######################
### Input data
in2015_07_01 <-read_csv('input.data_2015_07_01.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2015_07_01 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2015_07_01, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2015_07_01 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2015_07_01 %>%
  left_join(epilim_depth_2015_07_01, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2015_07_01 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2016_05_03  #######################
### Input data
in2016_05_03 <-read_csv('input.data_2016_05_03.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2016_05_03 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2016_05_03, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2016_05_03 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2016_05_03 %>%
  left_join(epilim_depth_2016_05_03, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2016_05_03 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2016_05_27  #######################
### Input data
in2016_05_27 <-read_csv('input.data_2016_05_27.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2016_05_27 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2016_05_27, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2016_05_27 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2016_05_27 %>%
  left_join(epilim_depth_2016_05_27, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2016_05_27 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))

####################     2016_08_24  #######################
### Input data
in2016_08_24 <-read_csv('input.data_2016_08_24.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2016_08_24 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2016_08_24, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2016_08_24 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2016_08_24 %>%
  left_join(epilim_depth_2016_08_24, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2016_08_24 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2016_09_12  #######################
### Input data
in2016_09_12 <-read_csv('input.data_2016_09_12.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2016_09_12 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2016_09_12, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2016_09_12 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2016_09_12 %>%
  left_join(epilim_depth_2016_09_12, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2016_09_12 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2017_06_20  #######################
### Input data
in2017_06_20 <-read_csv('input.data_2017_05_20.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2017_06_20 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2017_06_20, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2017_06_20 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2017_06_20 %>%
  left_join(epilim_depth_2017_06_20, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2017_06_20 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))



####################     2017_07_14  #######################
### Input data
in2017_07_14 <-read_csv('input.data_2017_07_14.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2017_07_14 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2017_07_14, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2017_07_14 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2017_07_14 %>%
  left_join(epilim_depth_2017_07_14, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2017_07_14 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2018_05_10  #######################
### Input data
in2018_05_10 <-read_csv('input.data_2018_05_10.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2018_05_10 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2018_05_10, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2018_05_10 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2018_05_10 %>%
  left_join(epilim_depth_2018_05_10, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2018_05_10 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2018_08_28  #######################
### Input data
in2018_08_28 <-read_csv('input.data_2018_08_28.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2018_08_28 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2018_08_28, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2018_08_28 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2018_08_28 %>%
  left_join(epilim_depth_2018_08_28, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2018_08_28 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2019_08_08  #######################
### Input data
in2019_08_08 <-read_csv('input.data_2019_08_08.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2019_08_08 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2019_08_08, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2019_08_08 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2019_08_08 %>%
  left_join(epilim_depth_2019_08_08, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2019_08_08 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2021_08_16  #######################
### Input data
in2021_08_16 <-read_csv('input.data_2021_08_16.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2021_08_16 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2021_08_16, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, DateTime, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )

smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth, avg_temperature)$y,
    smooth_depth = supsmu(rounded_depth, avg_temperature)$x
  ) 

# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2021_08_16 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2021_08_16 %>%
  left_join(epilim_depth_2021_08_16, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2021_08_16 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))


####################     2021_08_05  #######################
### Input data
in2021_08_05 <-read_csv('input.data_2021_08_05.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2021_08_05 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2021_08_05, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, Date, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )


smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  filter(!is.nan(avg_temperature)) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth[!is.nan(avg_temperature)], avg_temperature[!is.nan(avg_temperature)])$y,
    smooth_depth = supsmu(rounded_depth[!is.nan(avg_temperature)], avg_temperature[!is.nan(avg_temperature)])$x
  )


# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2021_08_05 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2021_08_05 %>%
  left_join(epilim_depth_2021_08_05, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2021_08_05 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))

####################     2021_09_25  #######################
### Input data
in2021_09_25 <-read_csv('input.data_2021_09_25.csv')

### Get depth descending data
# Calculate the average difference in depths within each profile
trend_data <- in2021_09_25 %>%
  group_by(profile_id) %>%
  summarise(average_depth_diff = mean(diff(depth, na.rm = TRUE), na.rm = TRUE))

# Filter profiles where the average depth difference is positive (indicating increasing depth)
descending_data <- trend_data %>%
  filter(average_depth_diff > 0) %>%
  left_join(in2021_09_25, by = "profile_id")

########### Epi depth #######################
# Calculate average temperature for every meter in each profile
average_temperature <- descending_data %>%
  group_by(profile_id, Date, depth) %>%
  mutate(rounded_depth = round(depth * 2) / 2) %>%
  group_by(profile_id, Date, rounded_depth) %>%
  summarise(
    avg_temperature = mean(temperature, na.rm = TRUE)
  )


smooth_temperature <- average_temperature %>%
  group_by(profile_id, Date) %>%
  arrange(rounded_depth) %>%
  filter(!is.nan(avg_temperature)) %>%
  summarise(
    smoothed_temperature = supsmu(rounded_depth[!is.nan(avg_temperature)], avg_temperature[!is.nan(avg_temperature)])$y,
    smooth_depth = supsmu(rounded_depth[!is.nan(avg_temperature)], avg_temperature[!is.nan(avg_temperature)])$x
  )


# Calculate epilimnetic depth using meta.depths for each profile ID
epilimnetic_depths <- average_temperature %>%
  group_by(profile_id, Date) %>%
  summarise(
    epilim_depth = ifelse(all(!is.na(meta.depths(wtr = avg_temperature, depths = rounded_depth))),
                          meta.depths(wtr = avg_temperature, depths = rounded_depth)[1],
                          15),
    epilim_depth = max(3, epilim_depth)  # Set a minimum epilim depth of 3
  )

# Calculate daily mean epilimnetic depth
epilim_depth_2021_09_25 <- epilimnetic_depths %>%
  group_by(Date) %>%
  summarise(mean_epilim_depth = mean(epilim_depth[!is.na(epilim_depth)]))

# Merge with epilimnetic depth dataframe
merged_data <- in2021_09_25 %>%
  left_join(epilim_depth_2021_09_25, by = "Date")

# Filter rows where depth is less than or equal to epilim_depth
epidata2021_09_25 <- merged_data %>%
  filter(depth <= mean_epilim_depth) %>% 
  select(Date, DateTime, chl) %>%
  mutate(DTime = as.POSIXct(DateTime)) %>% 
  filter(hour(DTime) >= 18 & hour(DTime) <= 23) %>%  # Filter data between 8 PM and 11:59 PM for each day
  group_by(Date) %>%
  summarise(mean_chl = mean(chl, na.rm = TRUE))







epi_chl_all <- rbind(epidata2014_09_03,epidata2014_09_12,epidata2014_09_25,epidata2014_10_30, epidata2015_07_01,
                     epidata2016_05_03,epidata2016_05_03, epidata2016_05_27, epidata2016_08_24,epidata2016_09_12,
                     epidata2017_06_20,epidata2017_07_14, epidata2018_05_10, epidata2018_08_28,epidata2019_08_08,
                     epidata2021_08_16,epidata2021_08_05,epidata2021_09_25)   


write.csv(epi_chl_all, "C:/Users/sasin/Sync/Sasindu/Trent Metabolism/Lake Superior/Vachan_P_Output/Other_water quality/epi_chl_all.csv")                                        


#epi_chl_all <- read.csv(file.choose(), header = T)
epi_chl_all$Date<- as.Date(epi_chl_all$Date, format = "%Y-%m-%d")
ALL.LS <- merge(ALL.LS, epi_chl_all, by = "Date")
