library(ggplot2)
library(sf)
library(tmap)
library(data.table)
library(tidycensus)
library(dplyr)
library(MASS)

# Set the directory path
data <- read.csv("C:/Users/Dropbox (Personal)/working paper/purpleair/sensor_stations/sensor_stations_all.csv")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_census_coverage.RData")
US_cbg_msa_census<-US_cbg_msa_census[,c(1,2,19)]

##find the percentage of cbgs with sensor
df<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/US_sensor_data.csv")
df$uptime<-round(df$uptime/60, 2)

df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)  # 4326 is the CRS for WGS 84


df <- st_transform(df, st_crs(cbg))
US_sensor_cbg <- st_join(df, cbg)
US_sensor_cbg <- US_sensor_cbg[,c("sensor_index","GEOID")]
US_sensor_cbg <- st_drop_geometry(US_sensor_cbg)

data <- merge(data,US_sensor_cbg, by="sensor_index")
data <- merge(data, US_cbg_msa_census, by="GEOID")

data <- data[data$stats_b != "", ]


library(stringr)
# Function to extract the pm2.5_24hour value from the string
extract_pm25_24hour <- function(data_string) {
  # Use a regular expression to match the pm2.5_24hour value
  matches <- str_match(data_string, "'pm2\\.5_24hour': ([0-9\\.]+)")
  # If there's a match, return the numeric value, otherwise return NA
  if (length(matches) > 1 && nchar(matches[1,2]) > 0) {
    as.numeric(matches[1,2])
  } else {
    NA
  }
}

# Function to extract the pm2.5_6hour value from the string
extract_pm25_6hour <- function(data_string) {
  # Use a regular expression to match the pm2.5_6hour value
  matches <- str_match(data_string, "'pm2\\.5_6hour': ([0-9\\.]+)")
  # If there's a match, return the numeric value, otherwise return NA
  if (length(matches) > 1 && nchar(matches[1,2]) > 0) {
    as.numeric(matches[1,2])
  } else {
    NA
  }
}

# Function to extract the pm2.5_1week value from the string
extract_pm25_1week <- function(data_string) {
  # Use a regular expression to match the pm2.5_1week value
  matches <- str_match(data_string, "'pm2\\.5_1week': ([0-9\\.]+)")
  # If there's a match, return the numeric value, otherwise return NA
  if (length(matches) > 1 && nchar(matches[1,2]) > 0) {
    as.numeric(matches[1,2])
  } else {
    NA
  }
}

# Apply the function to the data column and create a new column with the extracted values
data$pm25_24hour_a <- sapply(data$stats_a, extract_pm25_24hour)
data$pm25_24hour_b <- sapply(data$stats_b, extract_pm25_24hour)
data$pm25_6hour_a <- sapply(data$stats_a, extract_pm25_6hour)
data$pm25_6hour_b <- sapply(data$stats_b, extract_pm25_6hour)
data$pm25_1week_a <- sapply(data$stats_a, extract_pm25_1week)
data$pm25_1week_b <- sapply(data$stats_b, extract_pm25_1week)




df_reliability <- data[,c(2, 111, 112, 113, 114, 115, 116, 117, 118)]
df_reliability$diff_pct_24 <- 100*abs(2*(df_reliability$pm25_24hour_a- df_reliability$pm25_24hour_b)/(df_reliability$pm25_24hour_a+df_reliability$pm25_24hour_b))
df_reliability$diff_pct_1w <- 100*abs(2*(df_reliability$pm25_1week_a- df_reliability$pm25_1week_b)/(df_reliability$pm25_1week_a+df_reliability$pm25_1week_b))
df_reliability$diff_pct_6h <- 100*abs(2*(df_reliability$pm25_6hour_a- df_reliability$pm25_6hour_b)/(df_reliability$pm25_6hour_a+df_reliability$pm25_6hour_b))

df_reliability$diff_24h <- abs(df_reliability$pm25_24hour_a-df_reliability$pm25_24hour_b)
df_reliability$diff_1w <- abs(df_reliability$pm25_1week_a-df_reliability$pm25_1week_b)
df_reliability$diff_6h <- abs(df_reliability$pm25_6hour_a-df_reliability$pm25_6hour_b)

df_reliability <- df_reliability %>%
  mutate(invalid_24h = ifelse(diff_pct_24 > 100 & diff_24h > 5, 1, 0))

df_reliability <- df_reliability %>%
  mutate(invalid_1w = ifelse(diff_pct_1w > 100 & diff_1w > 5, 1, 0))

df_reliability <- df_reliability %>%
  mutate(invalid_6h = ifelse(diff_pct_6h > 100 & diff_6h > 5, 1, 0))


df_error_sum <- df_reliability %>% group_by(type) %>% summarise(error_6h = mean(invalid_6h, na.rm = TRUE),
                                                                error_24h = mean(invalid_24h, na.rm = TRUE),
                                                                error_1w = mean(invalid_1w, na.rm = TRUE))



df_long <- df_error_sum %>%
  pivot_longer(
    cols = starts_with("error_"),
    names_to = "time_period",
    values_to = "error_value"
  )

df_long$error_value <- 100*df_long$error_value

# Now create the grouped bar plot
ggplot(df_long, aes(x = error_value, y = type, fill = time_period)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  scale_fill_manual(values = c("error_6h" = "#EDF4F5", 
                             "error_24h" = "#C5E3E2", 
                             "error_1w" = "#9EC6DB"))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank())  

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/accuracy_different_percent.pdf"
ggsave(png_file0, width = 5, height =5)


write.csv(df_error_sum, "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/df_error_sum.csv", row.names = FALSE)




# Now we can plot with ggplot2
ggplot(df_long, aes(x = mean_value, y = type, fill = timeframe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_errorbar(
    aes(xmin = mean_value - std_dev, xmax = mean_value + std_dev, group = timeframe),
    position = position_dodge(width = 0.7),
    width = 0.25,
    col='grey'
  ) +
  scale_fill_manual(values = c("diff_6h" = "#EDF4F5", 
                               "diff_24h" = "#C5E3E2", 
                               "diff_1w" = "#9EC6DB"))+
  theme_minimal()+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank())  

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/error_percent.pdf"
ggsave(png_file0, width = 6, height =5)



####without error bar


df_long <- df_reliability_sum %>% 
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "timeframe",
    values_to = "value"
  )
dodge_width <- 0.9 / length(unique(df_long$timeframe))

# Now, create the bar plot
ggplot(df_long, aes(x = value, y = type, fill = timeframe, label = round(value, 1))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(position = position_dodge(width = dodge_width), vjust = 0, size = 4) +
  theme_minimal() +
  labs(x = "Difference_percent", fill = "Time Interval") +
  scale_fill_manual(values = c("diff_6h" = "#EDF4F5", 
                               "diff_24h" = "#C5E3E2", 
                               "diff_1w" = "#9EC6DB"))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank())  


png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/accuracy_different_percent.pdf"
ggsave(png_file0, width = 6, height =5)






####nonpoor_black######
#######################
data_nonpoor_black <- data[data$type=='Nonpoor-Black',]


##1 week pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_1week_b ~ pm25_1week_a, data = data_nonpoor_black)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)

ggplot(data_nonpoor_black, aes(x = data_nonpoor_black$pm25_1week_a, y = data_nonpoor_black$pm25_1week_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0, 40))

  
##24h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_24hour_b ~ pm25_24hour_a, data = data_nonpoor_black)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_nonpoor_black, aes(x = data_nonpoor_black$pm25_24hour_a, y = data_nonpoor_black$pm25_24hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 60)) +
  scale_y_continuous(limits = c(0, 60))

##6h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_6hour_b ~ pm25_6hour_a, data = data_nonpoor_black)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_nonpoor_black, aes(x = data_nonpoor_black$pm25_6hour_a, y = data_nonpoor_black$pm25_6hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0, 40))





####poor_black#########
#######################
data_poor_black <- data[data$type=='Poor-Black',]

##1 week pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_1week_b ~ pm25_1week_a, data = data_poor_black)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)

ggplot(data_poor_black, aes(x = data_poor_black$pm25_1week_a, y = data_poor_black$pm25_1week_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 20))


##24h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_24hour_b ~ pm25_24hour_a, data = data_poor_black)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_poor_black, aes(x = data_poor_black$pm25_24hour_a, y = data_poor_black$pm25_24hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 20))

##6h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_6hour_b ~ pm25_6hour_a, data = data_poor_black)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_poor_black, aes(x = data_poor_black$pm25_6hour_a, y = data_poor_black$pm25_6hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(0, 30))




####Nonpoor-White#########
##########################
data_nonpoor_white <- data[data$type=='Nonpoor-White',]

##1 week pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_1week_b ~ pm25_1week_a, data = data_nonpoor_white)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)

ggplot(data_nonpoor_white, aes(x = data_nonpoor_white$pm25_1week_a, y = data_nonpoor_white$pm25_1week_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 20))


##24h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_24hour_b ~ pm25_24hour_a, data = data_nonpoor_white)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_nonpoor_white, aes(x = data_nonpoor_white$pm25_24hour_a, y = data_nonpoor_white$pm25_24hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 20))

##6h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_6hour_b ~ pm25_6hour_a, data = data_nonpoor_white)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_nonpoor_white, aes(x = data_nonpoor_white$pm25_6hour_a, y = data_nonpoor_white$pm25_6hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(0, 30))




####Poor-White#########
##########################
data_poor_white <- data[data$type=='Poor-White',]

##1 week pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_1week_b ~ pm25_1week_a, data = data_poor_white)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)

ggplot(data_poor_white, aes(x = data_poor_white$pm25_1week_a, y = data_poor_white$pm25_1week_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 16)) +
  scale_y_continuous(limits = c(0, 16))


##24h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_24hour_b ~ pm25_24hour_a, data = data_poor_white)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_poor_white, aes(x = data_poor_white$pm25_24hour_a, y = data_poor_white$pm25_24hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 20))

##6h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_6hour_b ~ pm25_6hour_a, data = data_poor_white)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_poor_white, aes(x = data_poor_white$pm25_6hour_a, y = data_poor_white$pm25_6hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(limits = c(0, 25))





####Nonpoor-hispanic#########
##########################
data_nonpoor_hispanic <- data[data$type=='Nonpoor-Hispanic',]

##1 week pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_1week_b ~ pm25_1week_a, data = data_nonpoor_hispanic)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)

ggplot(data_nonpoor_hispanic, aes(x = data_nonpoor_hispanic$pm25_1week_a, y = data_nonpoor_hispanic$pm25_1week_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 20))


##24h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_24hour_b ~ pm25_24hour_a, data = data_nonpoor_hispanic)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_nonpoor_hispanic, aes(x = data_nonpoor_hispanic$pm25_24hour_a, y = data_nonpoor_hispanic$pm25_24hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(limits = c(0, 25))


##6h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_6hour_b ~ pm25_6hour_a, data = data_nonpoor_hispanic)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_nonpoor_hispanic, aes(x = data_nonpoor_hispanic$pm25_6hour_a, y = data_nonpoor_hispanic$pm25_6hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(limits = c(0, 25))




####Poor-hispanic#########
##########################
data_poor_hispanic <- data[data$type=='Poor-Hispanic',]

##1 week pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_1week_b ~ pm25_1week_a, data = data_poor_hispanic)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)

ggplot(data_poor_hispanic, aes(x = data_poor_hispanic$pm25_1week_a, y = data_poor_hispanic$pm25_1week_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 20))


##24h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_24hour_b ~ pm25_24hour_a, data = data_poor_hispanic)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_poor_hispanic, aes(x = data_poor_hispanic$pm25_24hour_a, y = data_poor_hispanic$pm25_24hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 25)) +
  scale_y_continuous(limits = c(0, 25))


##6h pm2.5 visualization
# Fit linear regression model
lm_model <- lm(pm25_6hour_b ~ pm25_6hour_a, data = data_poor_hispanic)
# Extract R-squared value
rsquared <- summary(lm_model)$r.squared
print(rsquared)
ggplot(data_poor_hispanic, aes(x = data_poor_hispanic$pm25_6hour_a, y = data_poor_hispanic$pm25_6hour_b)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Channel A", 
       y="Channel B")+
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 20))



###plot the R square for different groups
##read the summary
library(tidyr)
library(ggplot2)
library(dplyr)

df<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/Rsquare_accuracy.csv")
accuracy_long <- df %>% 
  pivot_longer(
    cols =-type,
    names_to = "time_interval",
    values_to = "Rsquare"
  )

ggplot(accuracy_long, aes(y = type, x = Rsquare, fill = time_interval)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Rsquare", fill = "Time Interval") +
  scale_fill_manual(values = c("6 h" = "#EDF4F5", 
                               "24 h" = "#C5E3E2", 
                               "1 week" = "#9EC6DB"))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank())  

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/accuracy_RSquared.pdf"
ggsave(png_file0, width = 4, height =5)
