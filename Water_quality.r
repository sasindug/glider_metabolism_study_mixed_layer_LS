####### RTRM Calculations 

# Libraries used
library(readr)
library(dplyr)
library(rLakeAnalyzer)
library(tidyr) 
library(ggpubr)



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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2014_09_03 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2014_09_03 <- epi2014_09_03 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2014_09_03 <- rtrm2014_09_03 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )


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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2014_09_12 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2014_09_12 <- epi2014_09_12 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2014_09_12 <- rtrm2014_09_12 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )



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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2014_09_25 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2014_09_25 <- epi2014_09_25 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2014_09_25 <- rtrm2014_09_25 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )



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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2014_10_30 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2014_10_30 <- epi2014_10_30 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2014_10_30 <- rtrm2014_10_30 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )


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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2015_07_01 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2015_07_01 <- epi2015_07_01 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2015_07_01 <- rtrm2015_07_01 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )



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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2016_05_03 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2016_05_03 <- epi2016_05_03 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2016_05_03 <- rtrm2016_05_03 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )



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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2016_05_27 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2016_05_27 <- epi2016_05_27 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2016_05_27 <- rtrm2016_05_27 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )

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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2016_08_24 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2016_08_24 <- epi2016_08_24 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2016_08_24 <- rtrm2016_08_24 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )

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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2016_09_12 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2016_09_12 <- epi2016_09_12 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2016_09_12 <- rtrm2016_09_12 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )


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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2017_06_20 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2017_06_20 <- epi2017_06_20 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2017_06_20 <- rtrm2017_06_20 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )



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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2017_07_14 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2017_07_14 <- epi2017_07_14 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2017_07_14 <- rtrm2017_07_14 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )


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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2018_05_10 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2018_05_10 <- epi2018_05_10 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2018_05_10 <- rtrm2018_05_10 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )



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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2018_08_28 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2018_08_28 <- epi2018_08_28 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2018_08_28 <- rtrm2018_08_28 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )

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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2019_08_08 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2019_08_08 <- epi2019_08_08 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2019_08_08 <- rtrm2019_08_08 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )


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
  group_by(profile_id, Date, depth) %>%
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

######### RTRM ############################

epi2021_08_16 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2021_08_16 <- epi2021_08_16 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2021_08_16 <- rtrm2021_08_16 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )

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

######### RTRM ############################

epi2021_08_05 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2021_08_05 <- epi2021_08_05 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2021_08_05 <- rtrm2021_08_05 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )

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

######### RTRM ############################

epi2021_09_25 <- smooth_temperature %>%
  rename(temperature = smoothed_temperature,
         depth = smooth_depth) %>% 
  inner_join(epilimnetic_depths, by = c("profile_id", "Date")) %>%
  filter(depth <= epilim_depth) %>%
  group_by(Date)


rtrm2021_09_25 <- epi2021_09_25 %>%
  group_by(profile_id, Date) %>%
  arrange(depth) %>%
  mutate(
    Water_Density = 1000 * (1 - ((temperature + 288.9414) / (508929.2 * (temperature + 68.12963))) * (temperature - 3.9863)^2) / 1000,
    RTRM = ((lead(Water_Density) - Water_Density) / (1 - 0.9999919))
  ) %>%
  filter(!is.na(RTRM) & RTRM >= 0) %>%
  select(profile_id, Date, depth, temperature, RTRM)


RTRM_2021_09_25 <- rtrm2021_09_25 %>%
  group_by(Date) %>%
  summarise(
    avg_RTRM = mean(as.numeric(RTRM), na.rm = TRUE)
  )


################### Combine all Water quality ###############################
rtrm_2014 <- rbind (RTRM_2014_09_03, RTRM_2014_09_12, RTRM_2014_09_25, RTRM_2014_10_30)
rtrm_2015 <-RTRM_2015_07_01
rtrm_2016 <- rbind (RTRM_2016_05_03, RTRM_2016_05_27, RTRM_2016_08_24, RTRM_2016_09_12) 
rtrm_2017 <- rbind(RTRM_2017_06_20, RTRM_2017_07_14) 
rtrm_2018 <- rbind(RTRM_2018_05_10, RTRM_2018_08_28) 
rtrm_2019 <- RTRM_2019_08_08
rtrm_2021 <- rbind(RTRM_2021_08_16, RTRM_2021_08_05, RTRM_2021_09_25)

rtrm_all <- rbind(rtrm_2014, rtrm_2015, rtrm_2016, rtrm_2017, rtrm_2018, rtrm_2019, rtrm_2021)


write.csv(rtrm_all, "C:/Users/sasin/Sync/Sasindu/Trent Metabolism/Lake Superior/Vachan_P_Output/Other_water quality/rtrm_all.csv")                                        
