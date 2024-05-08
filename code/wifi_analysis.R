library(ggplot2)
library(sf)
library(tmap)
library(data.table)
library(tidycensus)
library(dplyr)
library(MASS)



# Set the directory path
dir_path <- "C:/Users/Dropbox (Personal)/working paper/purpleair/wifi_all_sensors"

# List all CSV files in the directory
csv_files <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
data_list <- list()

# Loop through each CSV file and read it into a data frame
for (file in csv_files) {
  data <- read.csv(file)
  data_list[[file]] <- data
}

# Combine all data frames into one dataframe
combined_df <- do.call(rbind, data_list)

# View the combined dataframe
head(combined_df)
rownames(combined_df) <- NULL


###
df<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/US_sensor_data.csv")
df$uptime<-round(df$uptime/60, 2)

df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)  # 4326 is the CRS for WGS 84


load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_census_coverage.RData")

US_cbg_msa_census<-US_cbg_msa_census[,c(1,2,19)]

##find the percentage of cbgs with sensor
df <- st_transform(df, st_crs(cbg))
US_sensor_cbg <- st_join(df, cbg)
US_sensor_cbg <- US_sensor_cbg[,c("sensor_index","GEOID")]
US_sensor_cbg <- st_drop_geometry(US_sensor_cbg)

combined_df <- merge(combined_df,US_sensor_cbg, by="sensor_index")
combined_df <- merge(combined_df, US_cbg_msa_census, by="GEOID")

# Convert Unix timestamp to POSIXct object
combined_df$DateTime <- as.POSIXct(combined_df$time, origin = "1970-01-01")

# Extract day of the week
combined_df$DayOfWeek <- weekdays(combined_df$DateTime)

# Extract time of day
combined_df$TimeOfDay <- format(combined_df$DateTime, "%H:%M:%S")
# Extract first two characters of TimeOfDay
combined_df$TimeOfDay <- substr(combined_df$TimeOfDay, 1, 2)

load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/combined_df_wifi.RData")

combined_df$DayOfWeek <- factor(combined_df$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
combined_df$TimeOfDay <- factor(combined_df$TimeOfDay, levels=c("01","02","03","04","05","06",
                                                                "07","08","09","10","11","12",
                                                                "13","14","15","16","17","18",
                                                                "19","20","21","22","23","00"))

####visualize the day of week RSSI
DOW_rssi <-combined_df %>% 
  group_by(type,DayOfWeek)%>% summarise(mean_rssi=mean(rssi), std_rssi=sd(rssi))

DOW_rssi$type <- as.factor(DOW_rssi$type)


ggplot(DOW_rssi, aes(x = DayOfWeek, y = mean_rssi, color = type, group = type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Day of Week", y = "Received Signal Strength Indicator", fill = "Type") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_color_brewer(palette = "Set1")

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/rssi_sensor_DOW.pdf"
ggsave(png_file0, width = 7, height = 3)


####visualize the time of day RSSI
TOD_rssi <-combined_df %>% 
  group_by(type,TimeOfDay)%>% summarise(mean_rssi=mean(rssi), std_rssi=sd(rssi))

TOD_rssi$type <- as.factor(TOD_rssi$type)


ggplot(TOD_rssi, aes(x = TimeOfDay, y = mean_rssi, color = type, group = type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Time of Day", y = "Received Signal Strength Indicator", fill = "Type") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_color_brewer(palette = "Set1")

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/rssi_sensor_TOD.pdf"
ggsave(png_file0, width = 7, height = 3)




###aggregate all for different neighborhoods regarding the RSSI
ggplot(combined_df, aes(x = rssi, y = type, fill = type)) +
  #geom_boxplot(position = position_dodge(width = 0.9), outlier.shape = NA) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  theme_minimal() +
  labs(y = "Neighborhood Categories", x = "Received Signal Strength Indicator", fill = "Type") +
  scale_fill_manual(values = c("Nonpoor-Black" = "#f8766c",
                               "Nonpoor-Hispanic" = "#b79f00",
                               "Nonpoor-White" = "#00b938",
                               "Poor-Black" = "#00bec4",
                               "Poor-Hispanic" = "#609cff",
                               "Poor-White" = "#f563e2"))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")  


boxplot_stats <- combined_df %>%
  group_by(type) %>%
  summarize(
    Lower_Whisker = quantile(rssi, probs = 0.0),
    First_Quartile = quantile(rssi, probs = 0.25),
    Median = median(rssi),
    Mean = mean(rssi),
    Third_Quartile = quantile(rssi, probs = 0.75),
    Upper_Whisker = quantile(rssi, probs = 1.0),
    IQR = IQR(rssi)
  )

###aggregate all for different neighborhoods regarding the latency
#combined_df$latency_slow <- ifelse(combined_df$latency > 500, 1, 0)

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/rssi_sensor.pdf"
ggsave(png_file0, width = 5, height =5)

















####low latency

neighborhood_latency_results <- data.frame(
  type = c(
    'Nonpoor-Black',
    'Nonpoor-Hispanic',
    'Nonpoor-White',
    'Poor-Black',
    'Poor-Hispanic',
    'Poor-White'
  ),
  stringsAsFactors = FALSE
)

threshold_values <- seq(500, 1000, by = 100)

for (threshold in threshold_values) {
  print(threshold)
  
  ####only consider major MSAs based on threshold
  combined_df$latency_slow <- ifelse(combined_df$latency > threshold, 1, 0)

  latency_sum <-combined_df %>% 
    group_by(type)%>% summarise(latency_slow_percent=mean(latency_slow, na.rm = TRUE))
  
  neighborhood_latency_results <- merge(neighborhood_latency_results, latency_sum, by="type")
  names(neighborhood_latency_results)[ncol(neighborhood_latency_results)] <- threshold
}


data_long <- reshape2::melt(neighborhood_latency_results, id.vars = "type", variable.name = "number", value.name = "low_percent")
data_long$number <- as.numeric(as.character(data_long$number))
data_long$low_percent <- 100* data_long$low_percent

# Plotting all lines on one chart
ggplot(data_long, aes(x = number, y = low_percent, color = type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  xlab("Threshold of Low Latency (milliseconds)") +
  ylab("Percentage of low-latency sensors")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank())  

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/low_latency_sensor.pdf"
ggsave(png_file0, width = 5, height = 4)
