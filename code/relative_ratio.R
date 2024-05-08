library(ggplot2)
library(sf)
library(tmap)
library(data.table)
library(tidycensus)
library(dplyr)
library(MASS)
library(tidyr)



###indoor sensor
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_census_coverage.RData")
US_cbg_msa_census <- US_cbg_msa_census[,c(1,19)]

df<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/US_sensor_data.csv")
df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)  # 4326 is the CRS for WGS 84

load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa.RData")

##Outdoor
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




###get the proportion of neighborhoods for all cities
in_US_cbg_msa <- merge(in_US_cbg_msa, US_cbg_msa_census, by="GEOID")
sum_cbg_type <- in_US_cbg_msa %>%
  group_by(type) %>%
  summarise(count = n())
sum_cbg_type$sum_pct <-sum_cbg_type$count/154850

###get the proportion of neighborhoods with sensors
in_US_cbg_msa_withsensor <- in_US_cbg_msa[in_US_cbg_msa$n>0,]
sum_cbg_type_sensor_in <- in_US_cbg_msa_withsensor %>%
  group_by(type) %>%
  summarise(count = n())

sum_cbg_type_sensor_in$sensor_pct <-sum_cbg_type_sensor_in$count/2535

sum_cbg_type_sensor_in <- merge(sum_cbg_type_sensor_in, sum_cbg_type, by="type")
sum_cbg_type_sensor_in$rr_in<- sum_cbg_type_sensor_in$sensor_pct/sum_cbg_type_sensor_in$sum_pct
sum_cbg_type_sensor_in <- sum_cbg_type_sensor_in[,c('type','rr_in')]



##############################outdoor sensor################################
############################################################################
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



###
out_US_cbg_msa <- merge(out_US_cbg_msa, US_cbg_msa_census, by="GEOID")


###get the proportion of neighborhoods with sensors
out_US_cbg_msa_withsensor <- out_US_cbg_msa[out_US_cbg_msa$n>0,]

sum_cbg_type_sensor_out <- out_US_cbg_msa_withsensor %>%
  group_by(type) %>%
  summarise(count = n())

sum_cbg_type_sensor_out$sensor_pct <-sum_cbg_type_sensor_out$count/7311
sum_cbg_type_sensor_out <- merge(sum_cbg_type_sensor_out, sum_cbg_type, by="type")
sum_cbg_type_sensor_out$rr_out<- sum_cbg_type_sensor_out$sensor_pct/sum_cbg_type_sensor_out$sum_pct
sum_cbg_type_sensor_out <- sum_cbg_type_sensor_out[,c('type','rr_out')]


######merge for all neighborhoods##########
rr_combine <- merge(sum_cbg_type_sensor_in, sum_cbg_type_sensor_out, by="type")

df_long <- rr_combine %>% pivot_longer(cols = c(rr_in, rr_out),
                                           names_to = "variable", 
                                           values_to = "value")


fig <- ggplot(df_long, aes(x = value, y = type, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c(rr_in = "#C1DBF0", rr_out = "#F0C1A1")) +
  theme_minimal() +
  #labs(x = "Type", y = "Value", title = "Combined Bar Chart for pct and PercentageFreq") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+ # Rotate x labels if needed
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "right")

fig

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/in_and_out_relative_ratio.pdf"
ggsave(png_file0, width = 4.8, height = 4)





############city examples####################

cities_in <-c('San Francisco-Oakland-Fremont, CA',
'San Jose-Sunnyvale-Santa Clara, CA',
'Sacramento-Roseville-Folsom, CA',
'Los Angeles-Long Beach-Anaheim, CA',
'Seattle-Tacoma-Bellevue, WA',
'Santa Rosa-Petaluma, CA',
'Pittsburgh, PA',
'Portland-Vancouver-Hillsboro, OR-WA',
'New York-Newark-Jersey City, NY-NJ',
'Salt Lake City-Murray, UT',
'Santa Cruz-Watsonville, CA',
'Boulder, CO',
'Reno, NV',
'Eugene-Springfield, OR',
'Bend, OR')
i <- 0

# Create a vector with the types
types <- c("Nonpoor-Black", "Nonpoor-Hispanic", "Nonpoor-White",
           "Poor-Black", "Poor-Hispanic", "Poor-White")

# Convert the vector into a dataframe
base <- data.frame(type = types)
base$value <- 1

for(city_name in cities_in){
print(city_name)
i <-i+1
#indoor
city_cbg <- in_US_cbg_msa[in_US_cbg_msa$NAME==city_name,]
city_cbg_in <- city_cbg %>%
  group_by(type) %>%
  summarise(count = n())
city_cbg_in$sum_pct <-city_cbg_in$count/nrow(city_cbg)


city_cbg_in_sensor <- city_cbg[city_cbg$n>0,]
sum_city_cbg_in_sensor <- city_cbg_in_sensor %>%
  group_by(type) %>%
  summarise(count = n())
sum_city_cbg_in_sensor$sensor_pct <- sum_city_cbg_in_sensor$count/nrow(city_cbg_in_sensor)


sum_cbg_type_sensor_in <- merge(sum_city_cbg_in_sensor, city_cbg_in, by="type", all.y = TRUE)
sum_cbg_type_sensor_in <- merge(sum_cbg_type_sensor_in, base, by="type", all.y = TRUE)
sum_cbg_type_sensor_in[is.na(sum_cbg_type_sensor_in)] <- 0
sum_cbg_type_sensor_in$rr_in<- sum_cbg_type_sensor_in$sensor_pct/sum_cbg_type_sensor_in$sum_pct


ggplot(sum_cbg_type_sensor_in, aes(y = type, x = rr_in, fill = type)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("Nonpoor-Black" = "#C1DBF0", "Nonpoor-Hispanic" = "#C1DBF0", 
                               "Nonpoor-White" = "#C1DBF0", "Poor-Black" = "#C1DBF0", 
                               "Poor-Hispanic" = "#C1DBF0", "Poor-White" = "#C1DBF0"))  +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey") + # Add this line for the dashed line
  theme_minimal() +
  ggtitle(city_name)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "right")


png_file0 <- paste0("C:/Users/Dropbox (Personal)/working paper/purpleair/figures/multiple_cities_nonmobility/in/relative_ratio_", i, ".pdf")
ggsave(png_file0, width = 5, height = 4)
}





###outdoor
cities_out <-c('San Francisco-Oakland-Fremont, CA',
              'San Jose-Sunnyvale-Santa Clara, CA',
              'Sacramento-Roseville-Folsom, CA',
              'Seattle-Tacoma-Bellevue, WA',
              'Los Angeles-Long Beach-Anaheim, CA',
              'Pittsburgh, PA',
              'Santa Rosa-Petaluma, CA',
              'Portland-Vancouver-Hillsboro, OR-WA',
              'Washington-Arlington-Alexandria, DC-VA-MD-WV',
              'Salt Lake City-Murray, UT',
              'Santa Cruz-Watsonville, CA',
              'New York-Newark-Jersey City, NY-NJ',
              'Riverside-San Bernardino-Ontario, CA',
              'Reno, NV',
              'Bend, OR',
              'San Diego-Chula Vista-Carlsbad, CA',
              'Dallas-Fort Worth-Arlington, TX',
              'Austin-Round Rock-San Marcos, TX',
              'Eugene-Springfield, OR',
              'Boston-Cambridge-Newton, MA-NH',
              'Fresno, CA',
              'Denver-Aurora-Centennial, CO',
              'Minneapolis-St. Paul-Bloomington, MN-WI',
              'Chicago-Naperville-Elgin, IL-IN')
# Create a vector with the types
types <- c("Nonpoor-Black", "Nonpoor-Hispanic", "Nonpoor-White",
           "Poor-Black", "Poor-Hispanic", "Poor-White")

# Convert the vector into a dataframe
base <- data.frame(type = types)
base$value <- 1

i <- 0
for(city_name in cities_out){
  print(city_name)
  i <-i+1
city_cbg <- out_US_cbg_msa[out_US_cbg_msa$NAME==city_name,]

city_cbg_out <- city_cbg %>%
  group_by(type) %>%
  summarise(count = n())

city_cbg_out$sum_pct <-city_cbg_out$count/nrow(city_cbg)


city_cbg_out_sensor <- city_cbg[city_cbg$n>0,]
sum_city_cbg_out_sensor <- city_cbg_out_sensor %>%
  group_by(type) %>%
  summarise(count = n())
sum_city_cbg_out_sensor$sensor_pct <- sum_city_cbg_out_sensor$count/nrow(city_cbg_out_sensor)


sum_cbg_type_sensor_out <- merge(sum_city_cbg_out_sensor, city_cbg_out, by="type", all.y = TRUE)
sum_cbg_type_sensor_out <- merge(sum_cbg_type_sensor_out, base, by="type", all.y = TRUE)
sum_cbg_type_sensor_out[is.na(sum_cbg_type_sensor_out)] <- 0
sum_cbg_type_sensor_out$rr_out<- sum_cbg_type_sensor_out$sensor_pct/sum_cbg_type_sensor_out$sum_pct


ggplot(sum_cbg_type_sensor_out, aes(y = type, x = rr_out, fill = type)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("Nonpoor-Black" = "#F0C1A1", "Nonpoor-Hispanic" = "#F0C1A1", 
                               "Nonpoor-White" = "#F0C1A1", "Poor-Black" = "#F0C1A1", 
                               "Poor-Hispanic" = "#F0C1A1", "Poor-White" = "#F0C1A1"))  +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey") + # Add this line for the dashed line
  theme_minimal() +
  ggtitle(city_name)+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "right")


png_file0 <- paste0("C:/Users/Dropbox (Personal)/working paper/purpleair/figures/multiple_cities_nonmobility/out/relative_ratio_", i, ".pdf")
ggsave(png_file0, width = 5, height = 4)
}
