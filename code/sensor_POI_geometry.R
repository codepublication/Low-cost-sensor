library(data.table)
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
library(tidycensus)
library(MASS)
library(tidyr)

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
US_sensor_cbg$NAME <- NULL
US_sensor_cbg <- merge(US_sensor_cbg, US_cbg_msa, by='GEOID')
US_sensor_cbg <-US_sensor_cbg[,c('GEOID','sensor_index','location_type', 'NAME')]
US_sensor_indoor <- US_sensor_cbg[US_sensor_cbg$location_type==1,]
#get state



# Set the path of the directory
######################################
######################################
######################################
######################################
directory_path <- "E:/POI_geometry"


# Get a vector of all file names in the directory
file_list <- list.files(path = directory_path, full.names = TRUE)

sensor_POI <- data.frame()

for (i in 1:405){
print(i)
geometry <- fread(file_list[i])
geometry <- geometry[geometry$ISO_COUNTRY_CODE=='US',]
geometry <- geometry[geometry$GEOMETRY_TYPE=='POLYGON',]
geometry <- geometry[,c('PLACEKEY','TOP_CATEGORY','POLYGON_WKT')]
# Initialize a vector to keep track of rows with errors
has_error <- rep(FALSE, nrow(geometry))

# Loop through each WKT string and try to convert it to an sfc object
for(i in seq_along(geometry$POLYGON_WKT)) {
  wkt_string <- geometry$POLYGON_WKT[i]
  # Use tryCatch to test for errors
  has_error[i] <- tryCatch({
    st_as_sfc(wkt_string, crs = 4326)
    FALSE # Return FALSE if there was no error
  }, error = function(e) {
    TRUE # Return TRUE if there was an error
  })
}

# Exclude rows with errors
geometry <- geometry[!has_error, ]

# Convert WKT to an sf object
print('Convert WKT to an sf object')
geometry$POLYGON_WKT_SF <- st_as_sfc(geometry$POLYGON_WKT, crs = 4326)

# Now, make sure that 'geometry' is an sf object
geometry <- st_as_sf(geometry, sf_column_name= "POLYGON_WKT_SF")

US_sensor_indoor <- st_transform(US_sensor_indoor, st_crs(geometry))

geometry$POLYGON_WKT_SF <- st_make_valid(geometry$POLYGON_WKT_SF)

overlap_sensor <- st_join(US_sensor_indoor, geometry)
overlap_sensor <- overlap_sensor[complete.cases(overlap_sensor$PLACEKEY),]

sensor_POI <- rbind(sensor_POI,overlap_sensor)
}



############visualize non-residential indoor sensors########## 
##############################################################

load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/sensor_POI_1_398.RData")
new_category <-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/refined_categories.csv")
sensor_POI <- st_drop_geometry(sensor_POI)
sensor_POI <- merge(sensor_POI, new_category, by="TOP_CATEGORY")

POI_count_summary <-sensor_POI %>% 
  group_by(NAME,New_category)%>% summarise(city_category=n())

city_summary <-sensor_POI %>% 
  group_by(NAME)%>% summarise(city_sum=n())


library(dplyr)

# Define the vector of city names
cities <- c(
  "San Francisco-Oakland-Fremont, CA",
  "San Jose-Sunnyvale-Santa Clara, CA",
  "Sacramento--Roseville--Folsom, CA",
  "Los Angeles-Long Beach-Anaheim, CA",
  "Seattle-Tacoma-Bellevue, WA",
  "Santa Rosa-Petaluma, CA",
  "Pittsburgh, PA",
  "Portland-Vancouver-Hillsboro, OR-WA",
  "New York-Newark-Jersey City, NY-NJ",
  "Salt Lake City-Murray, UT",
  "Santa Cruz-Watsonville, CA",
  "Boulder, CO",
  "Reno, NV",
  "Eugene-Springfield, OR",
  "Bend, OR"
)

# Filter rows that match the city names
POI_count_summary <- POI_count_summary %>% 
  filter(NAME %in% cities)

category_sum <- POI_count_summary%>% 
  group_by(New_category)%>% summarise(sum_count=sum(city_category))


POI_count_summary <-merge(POI_count_summary,city_summary, by="NAME")
POI_count_summary$percent <-100* POI_count_summary$city_category/POI_count_summary$city_sum
POI_count_summary <- POI_count_summary %>% 
  filter(New_category != "")

POI_count_summary$city_category <-NULL
POI_count_summary$city_sum <- NULL

name_changes <- c(
  "San Francisco-Oakland-Fremont, CA" = "San Francisco",
  "San Jose-Sunnyvale-Santa Clara, CA" = "San Jose",
  "Sacramento-Roseville-Folsom, CA" = "Sacramento",
  "Los Angeles-Long Beach-Anaheim, CA" = "Los Angeles",
  "Seattle-Tacoma-Bellevue, WA" = "Seattle",
  "Santa Rosa-Petaluma, CA" = "Santa Rosa",
  "Pittsburgh, PA" = "Pittsburgh",
  "Portland-Vancouver-Hillsboro, OR-WA" = "Portland",
  "New York-Newark-Jersey City, NY-NJ" = "New York",
  "Salt Lake City-Murray, UT" = "Salt Lake City",
  "Santa Cruz-Watsonville, CA" = "Santa Cruz",
  "Boulder, CO" = "Boulder",
  "Reno, NV" = "Reno",
  "Eugene-Springfield, OR" = "Eugene",
  "Bend, OR" = "Bend"
)

category_abbreviations <- c(
  "Arts, Entertainment, and Recreation" = "ER",
  "Education Services" = "ED",
  "Financial Services" = "FS",
  "Food and Accommodation" = "FA",
  "Health and Social Care" = "HS",
  "Information and Communication" = "IS",
  "Manufacturing" = "MF",
  "Miscellaneous Services" = "MS",
  "Personal Services" = "PS",
  "Professional and Technical Services" = "PR",
  "Public Administration and Defense" = "PA",
  "Real Estate and Rental Services" = "RE",
  "Retail and Wholesale" = "RW",
  "Transportation and Logistics" = "TL",
  "Construction and Engineering" = "CE"
)

POI_count_summary<- POI_count_summary%>%
  mutate(NAME = recode(NAME, !!!name_changes))

POI_count_summary<- POI_count_summary%>%
  mutate(New_category = recode(New_category, !!!category_abbreviations))


library(tidyr)
library(dplyr)
df_wide <- POI_count_summary %>%
  spread(key = New_category, value = percent)
df_wide[is.na(df_wide)] <- 0


#load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/POI_home_sum.RData")

# Convert the data to a long format for plotting
df_long <- melt(df_wide, id.vars = "NAME")
# Reorder the NAME factor levels based on the sum
# Define the order as shown in the image
ordered_names <- rev(c(
  "San Francisco", 
  "San Jose",
  "Sacramento",
  "Los Angeles",
  "Seattle",
  "Santa Rosa",
  "Pittsburgh",
  "Portland",
  "New York",
  "Salt Lake City",
  "Santa Cruz",
  "Boulder",
  "Reno",
  "Eugene",
  "Bend"
))

# Reorder the factor levels for df_long$NAME
df_long$NAME <- factor(df_long$NAME, levels = ordered_names)


# Create the heatmap
ggplot(data = df_long, aes(x = variable, y = NAME, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  labs(x = '', y = '', fill = 'Value')+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/indoor_POI_types.pdf"
ggsave(png_file0, width = 6, height =3.5)



#find the residential sensor
############################
############################
############################
US_sensor_indoor <- st_drop_geometry(US_sensor_indoor)


POI_nonhome <- sensor_POI %>% distinct(sensor_index, .keep_all = TRUE)
POI_nonhome$nonhome <-'nonhome'
POI_nonhome <- POI_nonhome[,c('sensor_index','nonhome')]


US_sensor_indoor<- merge(US_sensor_indoor, POI_nonhome, by="sensor_index", all.x= TRUE)
US_sensor_indoor$nonhome[is.na(US_sensor_indoor$nonhome)] <- "home"
#US_sensor_indoor <- US_sensor_indoor[US_sensor_indoor$nonhome==0,]
POI_home_sum <- US_sensor_indoor %>% group_by(NAME,nonhome) %>% summarise(city_category=n())
POI_sum <- US_sensor_indoor %>% group_by(NAME) %>% summarise(sum=n())
POI_sum <- st_drop_geometry(POI_sum)


POI_home_sum <- merge(POI_home_sum, POI_sum, by="NAME")
POI_home_sum <- POI_home_sum[POI_home_sum$nonhome=="nonhome",]
POI_home_sum$nonhome_percent <- 100*POI_home_sum$city_category/POI_home_sum$sum

###indoor sensors above 50 counts
POI_home_sum <- POI_home_sum[POI_home_sum$sum>50,]
POI_home_sum <- POI_home_sum[order(-POI_home_sum$sum), ]

POI_home_sum$NAME <- factor(POI_home_sum$NAME, levels = POI_home_sum$NAME[order(POI_home_sum$sum)])
#save(POI_home_sum, file="C:/Users/Dropbox (Personal)/working paper/purpleair/data/POI_home_sum.RData")

name_changes <- c(
  "San Francisco-Oakland-Fremont, CA" = "San Francisco",
  "San Jose-Sunnyvale-Santa Clara, CA" = "San Jose",
  "Sacramento-Roseville-Folsom, CA" = "Sacramento",
  "Los Angeles-Long Beach-Anaheim, CA" = "Los Angeles",
  "Seattle-Tacoma-Bellevue, WA" = "Seattle",
  "Santa Rosa-Petaluma, CA" = "Santa Rosa",
  "Pittsburgh, PA" = "Pittsburgh",
  "Portland-Vancouver-Hillsboro, OR-WA" = "Portland",
  "New York-Newark-Jersey City, NY-NJ" = "New York",
  "Salt Lake City-Murray, UT" = "Salt Lake City",
  "Santa Cruz-Watsonville, CA" = "Santa Cruz",
  "Boulder, CO" = "Boulder",
  "Reno, NV" = "Reno",
  "Eugene-Springfield, OR" = "Eugene",
  "Bend, OR" = "Bend"
)

# Use mutate with recode to rename the values
POI_home_sum <- POI_home_sum %>%
  mutate(NAME = recode(NAME, !!!name_changes))



ggplot(POI_home_sum, aes(x = sum, y = NAME, fill = nonhome_percent)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(x = "Indoor Sensor Count")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/indoor_sensor_percent.pdf"
ggsave(png_file0, width = 4, height =3.5)



#summary statistics
POI_home_sum <- POI_home_sum[POI_home_sum$sum>10,]
