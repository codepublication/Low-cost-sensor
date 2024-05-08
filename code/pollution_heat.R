#get the sensor and environment mismatch
library(ggplot2)
library(sf)
library(tmap)
library(data.table)
library(tidycensus)
library(dplyr)
library(MASS)

load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_sum.RData")


#pollution
pm<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/air_pollution/EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.csv")
pm<- pm[,c("ID","PM25")]
pm<-pm[complete.cases(pm$PM25),]
names(pm)[1] <- "GEOID"

library(stringr)

# Assuming your dataframe is named df and the column with GEOID is named GEOID
pm$GEOID <- str_pad(pm$GEOID, width = 12, pad = "0")


load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa.RData")

US_cbg_msa$sum_count<- NULL
US_cbg_msa$n <- NULL

pm <- merge(pm,US_cbg_msa, by="GEOID")
msa_pm <-pm %>% 
  group_by(NAME)%>% summarise(city_pm=mean(PM25))


#heat
heat<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/heat/Census_UHI_US_Urbanized_recalculated.csv")
heat <- heat[,c(3,49,50)]
names(heat)[1] <- "CT"
heat$CT <- str_pad(heat$CT, width = 11, pad = "0")

US_cbg_msa$CT <- substr(US_cbg_msa$GEOID, 1, nchar(US_cbg_msa$GEOID) - 1)
US_cbg_msa <- merge(US_cbg_msa, heat, by="CT")
msa_heat <-US_cbg_msa %>% 
  group_by(NAME)%>% summarise(city_heat=mean(UHI_annual_day))


##merge and visualize
US_cbg_msa_sum <- merge(US_cbg_msa_sum, msa_pm, by='NAME')
US_cbg_msa_sum <- merge(US_cbg_msa_sum, msa_heat, by='NAME')

###heat PDF
# Calculate medians
median_below <- median(US_cbg_msa_sum[US_cbg_msa_sum$percent < 0.03, "city_heat"], na.rm = TRUE)
median_above <- median(US_cbg_msa_sum[US_cbg_msa_sum$percent > 0.03, "city_heat"], na.rm = TRUE)

# Create the plot with density curves
density_plot <- ggplot() +
  geom_density(data = US_cbg_msa_sum[US_cbg_msa_sum$percent < 0.03,], aes(x = city_heat), fill = "#551F33", alpha = 0.5) +
  geom_density(data = US_cbg_msa_sum[US_cbg_msa_sum$percent > 0.03,], aes(x = city_heat), fill = "#81B3A9", alpha = 0.5) +
  geom_vline(xintercept = median_below, linetype = "dashed", color = "#551F33", size = 0.2) +
  geom_vline(xintercept = median_above, linetype = "dashed", color = "#81B3A9", size = 0.2) +
  annotate("text", x = median_below, y = 0, label = paste("Median=", round(median_below,2)), color = "#551F33", vjust = -4.5, angle = 0, size = 4) +
  annotate("text", x = median_above, y = 0, label = paste("Median=", round(median_above,2)), color = "#81B3A9", vjust = -1.5, angle = 0, size = 4) +
  labs(x = "City Heat", y = "Density") +
  theme_minimal() +
  scale_x_continuous(name = "SUHI") +
  scale_y_continuous(name = "Probability Density")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "right")  

density_plot
png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/heat_density_plot_0.03.pdf"
ggsave(png_file0, width = 3, height =2)



###PM2.5 PDF
# Calculate medians
median_below <- median(US_cbg_msa_sum[US_cbg_msa_sum$percent < 0.03, "city_pm"], na.rm = TRUE)
median_above <- median(US_cbg_msa_sum[US_cbg_msa_sum$percent > 0.03, "city_pm"], na.rm = TRUE)

# Plot the densities
# Create the plot with density curves
density_plot <- ggplot() +
  geom_density(data = US_cbg_msa_sum[US_cbg_msa_sum$percent < 0.03,], aes(x = city_pm), fill = "#551F33", alpha = 0.5) +
  geom_density(data = US_cbg_msa_sum[US_cbg_msa_sum$percent > 0.03,], aes(x = city_pm), fill = "#81B3A9", alpha = 0.5) +
  geom_vline(xintercept = median_below, linetype = "dashed", color = "#551F33", size = 0.2) +
  geom_vline(xintercept = median_above, linetype = "dashed", color = "#81B3A9", size = 0.2) +
  annotate("text", x = median_below, y = 0, label = paste("Median=", round(median_below,2)), color = "#551F33", vjust = -4.5, angle = 0, size = 4) +
  annotate("text", x = median_above, y = 0, label = paste("Median=", round(median_above,2)), color = "#81B3A9", vjust = -1.5, angle = 0, size = 4) +
  labs(x = "City PM", y = "Density") +
  theme_minimal() +
  scale_x_continuous(name = "PM2.5") +
  scale_y_continuous(name = "Probability Density")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "right")  

density_plot

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/PM_density_plot_0.03.pdf"
ggsave(png_file0, width = 3, height =2)




####scatter plot
lm_model <- lm(sensor_density ~ percent, data = US_cbg_msa_sum)

# Extract R-squared value
rsquared <- summary(lm_model)$r.squared

US_cbg_msa_sum <- US_cbg_msa_sum[complete.cases(US_cbg_msa_sum$city_heat),]

library(ggExtra)

fig <-ggplot(US_cbg_msa_sum, aes(x = percent, y = sensor_density)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x="Neighborhood coverage (%)", 
       y="Sensors/1000 people") +
  annotate("text", x = max(US_cbg_msa_sum$percent), y = min(US_cbg_msa_sum$sensor_density), 
           label = paste("R^2 =", round(rsquared, digits = 3)), 
           hjust = 1, vjust = 0, color = "black")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank())  
fig

fig_with_marginals <- ggMarginal(fig, type = "histogram", fill="transparent")
png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/scatter_histogram.pdf"
ggsave(png_file0, fig_with_marginals, width = 6, height = 5)



fig <- ggplot(US_cbg_msa_sum, aes(x = percent, y = sensor_density, size = city_pm, color = city_heat, alpha = 1 / (city_heat + 1))) +
  geom_point(stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_size_continuous(name = "PM2.5", range = c(1, 6)) + 
  scale_color_gradient(low = "red", high = "black", name = "UHI") +
  scale_alpha_continuous(range = c(0.4, 1), guide = 'none') +  # Set the range for alpha and remove the legend for alpha
  labs(x = "Neighborhood coverage (%)", y = "Sensors/1000 people") +
  annotate("text", x = max(US_cbg_msa_sum$percent), y = min(US_cbg_msa_sum$sensor_density), 
           label = paste("R^2 =", round(rsquared, digits = 3)),  # Make sure rsquared is computed and available
           hjust = 1, vjust = 0, color = "black")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "right")

fig




png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/scatter_new.pdf"
ggsave(png_file0, width = 5, height = 4)
