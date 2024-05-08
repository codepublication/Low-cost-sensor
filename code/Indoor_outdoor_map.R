#
library(ggplot2)
library(sf)
library(tmap)
library(data.table)
library(tidycensus)
library(dplyr)
library(MASS)
library(tidyr)

#read MSA in the whole USA
US_msa<-st_read("C:/Users/Dropbox (Personal)/working paper/purpleair/tl_2023_us_cbsa/tl_2023_us_cbsa.shp")
US_msa<- US_msa[US_msa$LSAD=="M1",]
US_msa<-US_msa[,'NAME']
US_msa <- st_centroid(US_msa)



#read sensor data
df<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/US_sensor_data.csv")
df$uptime<-round(df$uptime/60, 2)

df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)  # 4326 is the CRS for WGS 84

load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa.RData")


##find the percentage of cbgs with outdoor sensor
df_out <- df[df$location_type==0,]
df_out <- st_transform(df_out, st_crs(cbg))
US_outsensor_cbg <- st_join(df_out, cbg)
US_outsensor_cbg$NAME <- NULL

US_outsensor_MSA <- merge(US_outsensor_cbg, US_cbg_msa, by="GEOID")


out_sum_count <-US_outsensor_cbg %>% 
  group_by(GEOID)%>% summarise(sum=n())
out_sum_count <- st_drop_geometry(out_sum_count)
out_sum_count$n <-1

out_US_cbg_msa <-merge(US_cbg_msa,out_sum_count, by="GEOID", all.x=TRUE)
out_US_cbg_msa$n[is.na(out_US_cbg_msa$n)] <- 0
out_US_cbg_msa$sum[is.na(out_US_cbg_msa$sum)] <- 0


out_US_cbg_msa_sum <- out_US_cbg_msa %>% 
  group_by(NAME)%>% 
  summarise(sensor_neighborhood= sum(n), 
            total_neighborhood=n(),
            sum_sensor=sum(sum))

out_US_cbg_msa_sum$percent <- 100*out_US_cbg_msa_sum$sensor_neighborhood/out_US_cbg_msa_sum$total_neighborhood

out_US_cbg_msa_sum <- merge(US_msa, out_US_cbg_msa_sum, by= "NAME")



states<-st_read("C:/Users/Dropbox (Personal)/working paper/purpleair/tl_2023_us_state/tl_2023_us_state.shp")
states<- states[!(states$NAME=='American Samoa'),]
states<- states[!(states$NAME=='United States Virgin Islands'),]
states<- states[!(states$NAME=='Guam'),]
states<- states[!(states$NAME=='Commonwealth of the Northern Mariana Islands'),]
states<- states[!(states$NAME=='Puerto Rico'),]
states<- states[!(states$NAME=='Alaska'),]
states<- states[!(states$NAME=='Hawaii'),]


#####map the outdoor sensor
#tmap_mode("view") 
tmap_mode("plot")
states <- st_transform(states, 2163)
out_US_cbg_msa_sum <- st_transform(out_US_cbg_msa_sum, 2163)

map <-tm_shape(states) +
  tm_borders(col = "gray", lwd = 1)+
  tm_shape(out_US_cbg_msa_sum) + 
  tm_bubbles(col = "percent", 
             palette = "viridis", 
             size = 'sum_sensor', 
             scale = 3,
             border.col = "transparent",
             alpha = 0.7,
             legend.size.show = TRUE) + 
  tm_layout(legend.position = c("left", "bottom"),  # Position of the default legend
            legend.outside = TRUE) +
  tm_scale_bar(position = c("left", "bottom"))  

map
png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/outdoor_msa_map.pdf"
tmap_save(map, filename = png_file0, width = 8, height = 5, dpi = 300)




###############indoor sensor#####################
##find the percentage of cbgs with outdoor sensor
df_in <- df[df$location_type==1,]
df_in <- st_transform(df_in, st_crs(cbg))
US_insensor_cbg <- st_join(df_in, cbg)
US_insensor_cbg$NAME <- NULL

US_insensor_MSA <- merge(US_insensor_cbg, US_cbg_msa, by="GEOID")


in_sum_count <-US_insensor_cbg %>% 
  group_by(GEOID)%>% summarise(sum=n())
in_sum_count <- st_drop_geometry(in_sum_count)
in_sum_count$n <-1

in_US_cbg_msa <-merge(US_cbg_msa,in_sum_count, by="GEOID", all.x=TRUE)
in_US_cbg_msa$n[is.na(in_US_cbg_msa$n)] <- 0
in_US_cbg_msa$sum[is.na(in_US_cbg_msa$sum)] <- 0


in_US_cbg_msa_sum <- in_US_cbg_msa %>% 
  group_by(NAME)%>% 
  summarise(sensor_neighborhood= sum(n), 
            total_neighborhood=n(),
            sum_sensor=sum(sum))

in_US_cbg_msa_sum$percent <- 100*in_US_cbg_msa_sum$sensor_neighborhood/in_US_cbg_msa_sum$total_neighborhood

in_US_cbg_msa_sum <- merge(US_msa, in_US_cbg_msa_sum, by= "NAME")



load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/sensor_POI_1_398.RData")
new_category <-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/refined_categories.csv")
sensor_POI <- st_drop_geometry(sensor_POI)
sensor_POI <- merge(sensor_POI, new_category, by="TOP_CATEGORY")

sensor_POI <- sensor_POI[!duplicated(sensor_POI$sensor_index), ]


nonhome_summary <-sensor_POI %>% 
  group_by(NAME)%>% summarise(city_sum=n())

in_US_cbg_msa_sum <- merge(in_US_cbg_msa_sum,nonhome_summary, by="NAME", all.x=TRUE)

#####map the indoor sensor
#tmap_mode("view") 
tmap_mode("plot")

# Transform the spatial data to the desired projection
states <- st_transform(states, 2163)
in_US_cbg_msa_sum <- st_transform(in_US_cbg_msa_sum, 2163)

map <-tm_shape(states) +
  tm_borders(col = "gray", lwd = 1)+
  #tm_text("STUSPS", size = 0.5, col="grey") +  # Add this line for labels
  tm_shape(in_US_cbg_msa_sum) + 
  tm_bubbles(col = "percent", 
             palette = "viridis", 
             size = 'sum_sensor', 
             scale = 3,
             border.col = "transparent",
             alpha = 0.7,
             legend.size.show = TRUE) + 
  tm_layout(legend.position = c("left", "bottom"),  # Position of the default legend
            legend.outside = TRUE) +
  tm_scale_bar(position = c("left", "bottom"))  

map
png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/indoor_msa_map.pdf"
tmap_save(map, filename = png_file0, width = 8, height = 5, dpi = 300)



###EPA sensors

aqs<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/data/aqs_sites/aqs_sites.csv")
aqs <- aqs[is.na(aqs$`Site Closed Date`), ]
aqs <- aqs[aqs$Latitude!=0, ]
aqs <- aqs[!is.na(aqs$Latitude), ]

aqs <- st_as_sf(aqs, coords = c("Longitude", "Latitude"), crs = 4326)  # 4326 is the CRS for WGS 84
aqs <- aqs[,c('Address')]


load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa.RData")


##find the percentage of cbgs with outdoor sensor
aqs <- st_transform(aqs, st_crs(cbg))
US_aqs_cbg <- st_join(aqs, cbg)
US_aqs_cbg$NAME <- NULL


US_aqs_MSA <- merge(US_aqs_cbg, US_cbg_msa, by="GEOID")


aqs_sum_count <-US_aqs_cbg %>% 
  group_by(GEOID)%>% summarise(sum=n())
aqs_sum_count <- st_drop_geometry(aqs_sum_count)
aqs_sum_count$n <-1

aqs_US_cbg_msa <-merge(US_cbg_msa,aqs_sum_count, by="GEOID", all.x=TRUE)
aqs_US_cbg_msa$sum[is.na(aqs_US_cbg_msa$sum)] <- 0
aqs_US_cbg_msa$n[is.na(aqs_US_cbg_msa$n)] <- 0


aqs_US_cbg_msa_sum <- aqs_US_cbg_msa %>% 
  group_by(NAME)%>% 
  summarise(sensor_neighborhood= sum(n), 
            total_neighborhood=n(),
            sum_sensor=sum(sum))

aqs_US_cbg_msa_sum$percent <- 100*aqs_US_cbg_msa_sum$sensor_neighborhood/aqs_US_cbg_msa_sum$total_neighborhood

aqs_US_cbg_msa_sum <- merge(US_msa, aqs_US_cbg_msa_sum, by= "NAME")

write.csv(aqs_US_cbg_msa_sum, 'C:/Users/Dropbox (Personal)/working paper/purpleair/EPA_US_cbg_msa_sum.csv')

states<-st_read("C:/Users/Dropbox (Personal)/working paper/purpleair/tl_2023_us_state/tl_2023_us_state.shp")
states<- states[!(states$NAME=='American Samoa'),]
states<- states[!(states$NAME=='United States Virgin Islands'),]
states<- states[!(states$NAME=='Guam'),]
states<- states[!(states$NAME=='Commonwealth of the Northern Mariana Islands'),]
states<- states[!(states$NAME=='Puerto Rico'),]
states<- states[!(states$NAME=='Alaska'),]
states<- states[!(states$NAME=='Hawaii'),]

#####map the EPA sensor sites
#tmap_mode("view") 
tmap_mode("plot")
states <- st_transform(states, 2163)
aqs_US_cbg_msa_sum <- st_transform(aqs_US_cbg_msa_sum, 2163)

map <-tm_shape(states) +
  tm_borders(col = "gray", lwd = 1)+
  tm_shape(aqs_US_cbg_msa_sum) + 
  tm_bubbles(col = "percent", 
             palette = "viridis", 
             size = 'sum_sensor', 
             scale = 2,
             border.col = "transparent",
             alpha = 0.7,
             legend.size.show = TRUE) + 
  tm_layout(legend.position = c("left", "bottom"),  # Position of the default legend
            legend.outside = TRUE) +
  tm_scale_bar(position = c("left", "bottom"))  

map
png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/EPA_msa_map.pdf"
tmap_save(map, filename = png_file0, width = 8, height = 5, dpi = 300)




#####percent of indoor sensor
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_census_coverage.RData")
US_cbg_msa_census <- US_cbg_msa_census[,c(1,19)]

US_insensor_MSA <- merge(US_insensor_MSA, US_cbg_msa_census, by="GEOID")
sum_indoor_pct <- US_insensor_MSA %>% group_by(type) %>%   summarise(count=n())
sum_indoor_pct$pct <- sum_indoor_pct$count/3970

type_counts2 <- table(US_insensor_MSA$type)

type_counts22 <- table(US_cbg_msa_census$type)
type_percentages2 <- (type_counts22 / sum(type_counts22))
percentage_df2 <- data.frame(type = names(type_percentages2), Percentage = type_percentages2)
percentage_df2$Percentage.Var1 <- NULL

sum_indoor_pct <- merge(sum_indoor_pct, percentage_df2, by="type")
sum_indoor_pct$relative_indoor_residential <- sum_indoor_pct$pct / sum_indoor_pct$Percentage.Freq

# Your specific values
new_values <- c(0.15, 0.41, 1.65, 0.05, 0.15, 0.58)

# Assign the new values as a column in your dataframe
sum_indoor_pct$relative_indoor_nonresidential <- new_values


df_long <- sum_indoor_pct %>% pivot_longer(cols = c(relative_indoor_residential, relative_indoor_nonresidential),
                               names_to = "variable", 
                               values_to = "value")


fig <- ggplot(df_long, aes(x = value, y = type, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c(relative_indoor_residential = "#C1DBF0", relative_indoor_nonresidential = "#F0C1A1")) +
  theme_minimal() +
  labs(x = "Type", y = "Value", title = "Combined Bar Chart for pct and PercentageFreq") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+ # Rotate x labels if needed
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "right")

fig

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/indoor_relative_ratio.pdf"
ggsave(png_file0, width = 6, height = 4)
