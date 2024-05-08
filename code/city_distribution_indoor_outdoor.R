library(ggplot2)
library(sf)
library(tmap)
library(data.table)
library(tidycensus)
library(dplyr)
library(MASS)

#read MSA in the whole USA
US_msa<-st_read("C:/Users/Dropbox (Personal)/working paper/purpleair/tl_2023_us_cbsa/tl_2023_us_cbsa.shp")
US_msa<- US_msa[US_msa$LSAD=="M1",]
US_msa<-US_msa[,'NAME']

load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_sum.RData")

indoor <- fread('C:/Users/Dropbox (Personal)/working paper/purpleair/in_US_cbg_msa_sum.csv')

outdoor <- fread('C:/User/Dropbox (Personal)/working paper/purpleair/out_US_cbg_msa_sum.csv')

epa <- fread('C:/Users/Dropbox (Personal)/working paper/purpleair/EPA_US_cbg_msa_sum.csv')

# Assuming your column is in a data frame called 'df'
# Convert values greater than 10 to a single category
indoor$sum_sensor_grouped <- ifelse(indoor$sum_sensor > 10, "10+", as.character(indoor$sum_sensor))

# Create a factor to ensure the levels are ordered correctly
indoor$sum_sensor_grouped <- factor(indoor$sum_sensor_grouped, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "10+"))


# Plot
# Create a plot
p <- ggplot(indoor, aes(x = sum_sensor_grouped)) +
  geom_bar(stat = "count", aes(y = ..count..)) + # geom_bar with a count stat
  xlab("Coount of Sensors") +
  ylab("Frequency") +
  ggtitle("Distribution of Indoor Sensors") +
  theme_minimal()+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
      panel.grid.major = element_blank(),  # Set color of major grid lines to grey
      panel.grid.minor = element_blank(),
      panel.background = element_blank()) 

# Add labels on top of the bars
p + geom_text(stat = 'count', aes(label = ..count.., y = ..count..), vjust = -0.10)

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/indoor_distribution.pdf"
ggsave(png_file0, width = 6, height =3.5)





######outdoor sensor##########
# Convert values greater than 10 to a single category
outdoor$sum_sensor_grouped <- ifelse(outdoor$sum_sensor > 10, "10+", as.character(outdoor$sum_sensor))

# Create a factor to ensure the levels are ordered correctly
outdoor$sum_sensor_grouped <- factor(outdoor$sum_sensor_grouped, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "10+"))




# Plot
# Create a plot
p <- ggplot(outdoor, aes(x = sum_sensor_grouped)) +
  geom_bar(stat = "count", aes(y = ..count..)) + # geom_bar with a count stat
  xlab("Coount of Sensors") +
  ylab("Frequency") +
  ggtitle("Distribution of Outdoor Sensors") +
  theme_minimal()+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
      panel.grid.major = element_blank(),  # Set color of major grid lines to grey
      panel.grid.minor = element_blank(),
      panel.background = element_blank()) 

# Add labels on top of the bars
p + geom_text(stat = 'count', aes(label = ..count.., y = ..count..), vjust = -0.1)
png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/outdoor_distribution.pdf"
ggsave(png_file0, width = 6, height =3.5)




######EPA sensor##########
# Convert values greater than 10 to a single category
epa$sum_sensor_grouped <- ifelse(epa$sum_sensor > 10, "10+", as.character(epa$sum_sensor))

# Create a factor to ensure the levels are ordered correctly
epa$sum_sensor_grouped <- factor(epa$sum_sensor_grouped, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "10+"))




# Plot
# Create a plot
p <- ggplot(epa, aes(x = sum_sensor_grouped)) +
  geom_bar(stat = "count", aes(y = ..count..)) + # geom_bar with a count stat
  xlab("Coount of Sensors") +
  ylab("Frequency") +
  ggtitle("Distribution of EPA Sensors") +
  theme_minimal()+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

# Add labels on top of the bars
p + geom_text(stat = 'count', aes(label = ..count.., y = ..count..), vjust = -0.1)
png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/epa_distribution.pdf"
ggsave(png_file0, width = 6, height =3.5)
