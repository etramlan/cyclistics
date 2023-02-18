#load all the necessary csv files & merging them
library(dplyr)
folder_path <- "D:\\Eki\\Projects\\Cyclistics\\Raw Data"
files <- list.files(folder_path, pattern = ".*2022.*\\.csv")
data_list <- lapply(files, function(x) read.csv(file.path(folder_path, x)))
merged_data <- do.call(rbind, data_list)
head(merged_data)
nrow(merged_data)
glimpse(merged_data)
summary(merged_data)

#Now lets clean the data to be able to properly work with it:
merged_data_clean <- na.omit(merged_data)

merged_data_clean$date <- as.Date(merged_data_clean$started_at) 
merged_data_clean$month <- format(as.Date(merged_data_clean$date), "%m")
merged_data_clean$day <- format(as.Date(merged_data_clean$date), "%d")
merged_data_clean$year <- format(as.Date(merged_data_clean$date), "%Y")
merged_data_clean$day_of_week <- format(as.Date(merged_data_clean$date), "%A")

colnames(merged_data_clean)

#find average ride length and average ride between members and casual riders

#to make it easier i will install geosphere package
install.packages("geosphere")
library(geosphere)

# convert started_at and ended_at to POSIXct format
merged_data_clean$started_at <- as.POSIXct(merged_data_clean$started_at, format = "%Y-%m-%d %H:%M:%S")
merged_data_clean$ended_at <- as.POSIXct(merged_data_clean$ended_at, format = "%Y-%m-%d %H:%M:%S")

# calculate ride time in minutes
merged_data_clean$ride_time <- difftime(merged_data_clean$ended_at, merged_data_clean$started_at, units = "mins")

# calculate ride length in km
merged_data_clean$ride_length <- distHaversine(merged_data_clean[c("start_lng", "start_lat")], merged_data_clean[c("end_lng", "end_lat")]) / 1000

#total ride per member type

ggplot(merged_data_clean, aes(x = member_casual, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(x = "Membership Type", y = "Number of Riders", fill = "Membership Type") +
  scale_x_discrete(limits = c("member", "casual"))

# group by member_casual and calculate average ride time and ride length
avg_distance_time <- merged_data_clean %>%
  group_by(member_casual) %>%
  summarise(avg_ride_time = mean(ride_time),
            avg_ride_length = mean(ride_length))

total_distance_time <- merged_data_clean %>%
  group_by(member_casual) %>%
  summarise(total_ride_time = sum(ride_time),
            total_ride_length = sum(ride_length))

# display the result
head(avg_distance_time)

#bar graph for those results

ggplot(avg_distance_time, aes(x=member_casual, y=avg_ride_time, fill=member_casual)) +
  geom_bar(stat="identity") +
  labs(title="Average Ride Time by Member Type", x="Member Type", y="Average Ride Time")

ggplot(avg_distance_time, aes(x=member_casual, y=avg_ride_length, fill=member_casual)) +
  geom_bar(stat="identity") +
  labs(title="Average Ride Length by Member Type", x="Member Type", y="Average Ride Length")

#average ride per day of the week

#bar chart

merged_data_clean$day_of_week <- factor(merged_data_clean$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(merged_data_clean, aes(x = day_of_week, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(x = "Day of the Week", y = "Number of Riders", fill = "Membership Type") +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# See in map
install.packages("leaflet")
library(leaflet)

# Create a map centered on Chicago
chicago_map <- leaflet() %>%
  setView(lng = -87.623177, lat = 41.881832, zoom = 11)

# Add markers for the start and end points of each ride
chicago_map %>% addMarkers(lng = merged_data_clean$start_lng, lat = merged_data_clean$start_lat)

# most popular starting station

merged_data_clean%>%
  filter(start_station_name != "" & end_station_name != "") %>%
  group_by(member_casual, start_station_name, end_station_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  group_by(member_casual) %>%
  slice_head(n = 1)
