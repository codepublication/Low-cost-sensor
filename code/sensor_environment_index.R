#get the sensor and environment mismatch
library(ggplot2)
library(sf)
library(tmap)
library(data.table)
library(tidycensus)
library(dplyr)
library(MASS)


pm<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/air_pollution/EJSCREEN_2023_BG_with_AS_CNMI_GU_VI.csv")
pm<- pm[,c("ID","PM25")]
pm<-pm[complete.cases(pm$PM25),]
names(pm)[1] <- "GEOID"

library(stringr)

# Assuming your dataframe is named df and the column with GEOID is named GEOID
pm$GEOID <- str_pad(pm$GEOID, width = 12, pad = "0")




#read sensor data
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa.RData")

#read sensor data
df<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/US_sensor_data.csv")
df$uptime<-round(df$uptime/60, 2)

df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)  # 4326 is the CRS for WGS 84

#start from here
df <- st_transform(df, st_crs(cbg))
US_sensor_cbg <- st_join(df, cbg)

US_cbg_msa_census<-merge(US_cbg_msa, cbg, by="GEOID")



#the coverage of cities
sum_count2 <-US_sensor_cbg %>% 
  group_by(GEOID)%>% summarise(sum_count=n())
sum_count2 <- st_drop_geometry(sum_count2)
sum_count2 <- sum_count2[!is.na(sum_count2$GEOID),]
sum_count2$n <-1

#get the total number of each neighborhood
US_cbg_msa <-merge(US_cbg_msa,sum_count2, by="GEOID", all.x=TRUE)
US_cbg_msa$n[is.na(US_cbg_msa$sum_count)] <- 0
US_cbg_msa$sum_count[is.na(US_cbg_msa$sum_count)] <- 0



cbg_pop <-US_cbg_msa_census[,c('GEOID','poptotal','incomeE')]

US_cbg_msa <- merge(US_cbg_msa, cbg_pop, by="GEOID")

US_cbg_msa <- merge(US_cbg_msa, pm, by="GEOID")
US_cbg_msa$density <- US_cbg_msa$sum_count/US_cbg_msa$poptotal



load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_sum.RData")

##cities more than 100 neighborhoods, 20% sensor coverage
cities<- c('Santa Rosa-Petaluma, CA',
           'Santa Cruz-Watsonville, CA',
           'Bend, OR',
           'San Francisco-Oakland-Fremont, CA',
           'Redding, CA',
           'San Jose-Sunnyvale-Santa Clara, CA',
           'Bellingham, WA',
           'Napa, CA',
           'Grand Junction, CO',
           'Boulder, CO',
           'Eugene-Springfield, OR',
           'Medford, OR',
           'Sacramento-Roseville-Folsom, CA',
           'Grants Pass, OR',
           'San Luis Obispo-Paso Robles, CA')

for (city in cities){
print(city)
city_cbg <- US_cbg_msa[US_cbg_msa$NAME==city,]
city_cbg <- city_cbg[city_cbg$poptotal>0,]
city_cbg <-city_cbg[complete.cases(city_cbg$density),]
lm_model <- lm(density ~PM25+incomeE, data = city_cbg)
# Extract R-squared value

print(summary(lm_model))
}

######sensor and heat island###########
#######################################
#######################################

heat<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/heat/Census_UHI_US_Urbanized_recalculated.csv")
heat <- heat[,c(3,49,50)]
names(heat)[1] <- "CT"
heat$CT <- str_pad(heat$CT, width = 11, pad = "0")

US_cbg_msa$CT <- substr(US_cbg_msa$GEOID, 1, nchar(US_cbg_msa$GEOID) - 1)
US_cbg_msa <- merge(US_cbg_msa, heat, by="CT")

print("UHI_daytime")
for (city in cities){
  print(city)
  city_cbg <- US_cbg_msa[US_cbg_msa$NAME==city,]
  city_cbg <- city_cbg[city_cbg$poptotal>0,]
  city_cbg <-city_cbg[complete.cases(city_cbg$density),]
  lm_model <- lm(density ~UHI_annual_day+incomeE, data = city_cbg)
  # Extract R-squared value
  
  print(summary(lm_model))
}



######visualize sensor and heat island###########
#################################################
#################################################

# Read the heat file
coeff_data_heat <- fread("C:/Users/Dropbox (Personal)/working paper/purpleair/coeff_sensor_heat.csv")

# Assuming the data frame 'coeff_data' has a column named 'coeff' and another column for the categories (e.g., 'category')
# Create a bar chart
ggplot(coeff_data_heat, aes(x = coeff, y = city)) +
  geom_bar(stat = "identity",fill = "#4A7298") +
  geom_errorbar(aes(xmin = coeff - SE, xmax = coeff + SE), width = 0.2) +
  theme_minimal() +
  labs(x = "Coefficient")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")  

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/coeff_heat_density.pdf"
ggsave(png_file0, width = 3, height =3.5)



# Read the pollution file
coeff_data_pollution <- fread("C:/Users/Dropbox (Personal)/working paper/purpleair/coeff_sensor_pollution.csv")

# Assuming the data frame 'coeff_data' has a column named 'coeff' and another column for the categories (e.g., 'category')
# Create a bar chart
ggplot(coeff_data_pollution, aes(x = coeff, y = city)) +
  geom_bar(stat = "identity",fill = "#F3C846") +
  geom_errorbar(aes(xmin = coeff - SE, xmax = coeff + SE), width = 0.2) +
  theme_minimal() +
  labs(x = "Coefficient")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") 
  
png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/coeff_pollution_density.pdf"
ggsave(png_file0, width = 3, height =3.5)



