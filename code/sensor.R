#
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


# Check validity of geometries individually
invalid_indices <- which(!st_is_valid(msa_city))

if (length(invalid_indices) > 0) {
  msa_city[invalid_indices, ] <- st_make_valid(msa_city[invalid_indices, ])
}





#read sensor data
df<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/US_sensor_data.csv")
df$uptime<-round(df$uptime/60, 2)

df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)  # 4326 is the CRS for WGS 84

#----------Use the census API key to get the decennial data-----------#
#https://api.census.gov/data/2020/acs/acs5/variables.html

# census_var <- load_variables(2021, 'acs5', cache = TRUE)
# states <- state.abb
# states <- c(states, "DC")
# 
# var <- c(poptotal='B03002_001E', 
#          hispanic='B03002_012E',
#          white='B03002_003E',
#          black='B03002_004E', 
#          asian='B03002_006E',
#          poptotal2='B17017_001E',
#          poverty='B17017_002E',
#          income='B19013_001') 
#'B03002_012E	Estimate!!Total:!!Hispanic or Latino:	HISPANIC OR LATINO ORIGIN BY RACE'
#'B03002_003E	Estimate!!Total:!!Not Hispanic or Latino:!!White alone	HISPANIC OR LATINO ORIGIN BY RACE'
#'B03002_004E	Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone	HISPANIC OR LATINO ORIGIN BY RACE'
#'B03002_006E	Estimate!!Total:!!Not Hispanic or Latino:!!Asian alone	HISPANIC OR LATINO ORIGIN BY RACE'
#'B17001F_001  Estimate!!Total: POVERTY STATUS IN THE PAST 12 MONTHS BY HOUSEHOLD TYPE BY AGE OF HOUSEHOLDER
#'B17001F_002' Estimate!!Total:!!Income in the past 12 months below poverty level: POVERTY STATUS IN THE PAST 12 MONTHS BY HOUSEHOLD TYPE BY AGE OF HOUSEHOLDER

# cbg <- get_acs(geography = "block group", variables = var, 
#                state = states,output="wide", year = 2021, geometry = TRUE)
# cbg$B03002_001M<- NULL
# cbg$B03002_012M<- NULL
# cbg$B03002_003M<- NULL
# cbg$B03002_004M<- NULL
# cbg$B03002_006M<- NULL
# cbg$B17017_001M<- NULL
# cbg$B17017_002M<- NULL

#save(cbg, file="C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")

# US_cbg_point <- st_centroid(cbg)
# US_cbg_point<-US_cbg_point[,c(1)]
# US_cbg_msa <- st_join(US_cbg_point, US_msa)
# US_cbg_msa<-st_drop_geometry(US_cbg_msa)
# US_cbg_msa<- US_cbg_msa[complete.cases(US_cbg_msa$NAME),]

##save the cbg within all US MSAs
#save(US_cbg_msa, file="C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa.RData")



##find the percentage of cbgs with sensor
df <- st_transform(df, st_crs(cbg))
US_sensor_cbg <- st_join(df, cbg)
US_sensor_cbg$NAME <- NULL
US_sensor_MSA <- merge(US_sensor_cbg, US_cbg_msa, by="GEOID")

US_msa_indoor <- US_sensor_MSA %>%
  group_by(NAME)%>% 
  summarise(indoor_pct =mean(location_type, na.rm = TRUE))
US_msa_indoor$indoor_pct <- 100*US_msa_indoor$indoor_pct
US_msa_indoor <- st_drop_geometry(US_msa_indoor)


###
sum_count <-US_sensor_cbg %>% 
  group_by(GEOID)%>% summarise(n=n())
sum_count <- st_drop_geometry(sum_count)
sum_count$n <-1

US_cbg_msa <-merge(US_cbg_msa,sum_count, by="GEOID", all.x=TRUE)

US_cbg_msa$n[is.na(US_cbg_msa$n)] <- 0

US_cbg_msa_sum <- US_cbg_msa %>% 
  group_by(NAME)%>% 
  summarise(sensor_neighborhood= sum(n), 
            total_neighborhood=n())

US_cbg_msa_sum$percent <- US_cbg_msa_sum$sensor_neighborhood/US_cbg_msa_sum$total_neighborhood

##get the income of MSAs
cbg <- st_drop_geometry(cbg)
cbg$NAME <- NULL

US_cbg_msa_census<-merge(US_cbg_msa, cbg, by="GEOID")

US_msa_income <- US_cbg_msa_census %>%
  group_by(NAME)%>% 
  summarise(income =mean(incomeE, na.rm = TRUE))

##get the population of MSAs
US_msa_pop <- US_cbg_msa_census %>%
  group_by(NAME)%>% 
  summarise(poptotal =sum(poptotal, na.rm = TRUE))
US_msa_pop$poptotal <-US_msa_pop$poptotal/1000




#find the days of sensors from the day of setup
US_sensor_cbg$last_modified <-as.integer(US_sensor_cbg$last_modified)
US_sensor_cbg$last_modified <- as.POSIXct(US_sensor_cbg$last_modified, origin = "1970-01-01")
US_sensor_cbg$time_difference <- difftime(Sys.time(), US_sensor_cbg$last_modified)
US_sensor_cbg$time_difference <- as.numeric(US_sensor_cbg$time_difference)

US_sensor_cbg <-merge(US_sensor_cbg, US_cbg_msa, by="GEOID")
US_sensor_cbg$NAME.x <- NULL
names(US_sensor_cbg)[21] <- "NAME"


US_cbg_msa_sum2 <- US_sensor_cbg %>% 
  group_by(NAME)%>% 
  summarise(usage_days= mean(time_difference))
US_cbg_msa_sum2 <- st_drop_geometry(US_cbg_msa_sum2)

#merge day of usage
US_cbg_msa_sum <-merge(US_cbg_msa_sum, US_cbg_msa_sum2, by="NAME")

#merge income
US_cbg_msa_sum <-merge(US_cbg_msa_sum, US_msa_income, by="NAME")
US_cbg_msa_sum$log_income <- log10(US_cbg_msa_sum$income)

#merge pop
US_cbg_msa_sum <-merge(US_cbg_msa_sum, US_msa_pop, by="NAME")

sum_count_sensor <-US_sensor_cbg %>% 
  group_by(GEOID)%>% summarise(n=n())

sum_count_sensor<- merge(sum_count_sensor, US_cbg_msa, by="GEOID")
sum_count_sensor <-sum_count_sensor %>% 
  group_by(NAME)%>% summarise(sensor_sum=sum(n.x))

US_cbg_msa_sum <-merge(US_cbg_msa_sum, sum_count_sensor, by="NAME")
US_cbg_msa_sum$sensor_density <-US_cbg_msa_sum$sensor_sum/US_cbg_msa_sum$poptotal

#save(US_cbg_msa_sum, file="C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_sum.RData")

load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_sum.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_msa.RData")

##making maps for the US
US_msa <- st_centroid(US_msa)
US_msa <- merge(US_msa, US_cbg_msa_sum, by= "NAME")
US_msa$percent <- 100*US_msa$percent
US_msa$usage_years <- US_msa$usage_days/365




#save(US_msa, file="C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_msa.RData")


states<-st_read("C:/Users/Dropbox (Personal)/working paper/purpleair/tl_2023_us_state/tl_2023_us_state.shp")
states<- states[!(states$NAME=='American Samoa'),]
states<- states[!(states$NAME=='United States Virgin Islands'),]
states<- states[!(states$NAME=='Guam'),]
states<- states[!(states$NAME=='Commonwealth of the Northern Mariana Islands'),]
states<- states[!(states$NAME=='Puerto Rico'),]
states<- states[!(states$NAME=='Alaska'),]
states<- states[!(states$NAME=='Hawaii'),]



#tmap_mode("view") 
tmap_mode("plot")

map <-tm_shape(states) +
  tm_borders(col = "gray", lwd = 1)+
  tm_shape(US_msa) + 
  tm_bubbles(col = "sensor_density", 
             palette = "viridis", 
             size = 'percent', 
             scale = 2,
             border.col = "transparent",
             alpha = 0.7,
             legend.size.show = FALSE) + 
  tm_layout(legend.position = c("left", "bottom"),  # Position of the default legend
            legend.outside = TRUE) +
  tm_scale_bar(position = c("left", "bottom"))  

map

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/msa_map.pdf"
tmap_save(map, filename = png_file0, width = 8, height = 5, dpi = 300)

#US_msa_new <- US_msa[US_msa$sensor_neighborhood>10,]



####scatter plot showing the association between density and coverage######
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_sum.RData")

US_cbg_msa_sum$percent <- 100* US_cbg_msa_sum$percent
US_cbg_msa_sum$usage_years <- US_cbg_msa_sum$usage_days/365
US_cbg_msa_sum <- merge(US_cbg_msa_sum, US_msa_indoor, by="NAME")
# Fit linear regression model
lm_model <- lm(sensor_density ~ percent, data = US_cbg_msa_sum)

# Extract R-squared value
rsquared <- summary(lm_model)$r.squared

library(ggExtra)

fig <-ggplot(US_cbg_msa_sum, aes(x = percent, y = sensor_density, size = indoor_pct, color =usage_years)) +
  geom_point(alpha = 0.9, stroke = 0) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_size_continuous(name = "indoor_pct") + 
  scale_color_gradient(low = "yellow", high = "blue", name = "usage_years")+
  labs(x="Neighborhood coverage (%)", 
       y="Sensors/1000 people") +
  annotate("text", x = max(US_cbg_msa_sum$percent), y = min(US_cbg_msa_sum$sensor_density), 
           label = paste("R^2 =", round(rsquared, digits = 3)), 
           hjust = 1, vjust = 0, color = "black")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")  
fig

fig_with_marginals <- ggMarginal(fig, type = "histogram",fill="transparent")
fig_with_marginals


png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/scatter.pdf"
ggsave(png_file0, width = 5, height = 4)

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/scatter_histogram.pdf"
ggsave(png_file0, fig_with_marginals, width = 5, height = 5)











#########neighborhood level outdoor sensor analysis#########
#############################################
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_sum.RData")
#read sensor data
df<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/US_sensor_data.csv")
df$uptime<-round(df$uptime/60, 2)
df <- df[df$location_type==0,]

df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)  # 4326 is the CRS for WGS 84

#start from here
df <- st_transform(df, st_crs(cbg))
US_sensor_cbg <- st_join(df, cbg)

#the coverage of cities
sum_count2 <-US_sensor_cbg %>% 
  group_by(GEOID)%>% summarise(sum_count=n())
sum_count2 <- st_drop_geometry(sum_count2)
sum_count2 <- sum_count2[!is.na(sum_count2$GEOID),]
sum_count2$n <-1


#get the total number of each neighborhood
US_cbg_msa <-merge(US_cbg_msa,sum_count2, by="GEOID", all.x=TRUE)
US_cbg_msa$n[is.na(US_cbg_msa$n)] <- 0
US_cbg_msa$sum_count[is.na(US_cbg_msa$sum_count)] <- 0


##connect the demographics with sensor
US_cbg_msa_census<-merge(US_cbg_msa, cbg, by="GEOID")
US_cbg_msa_census$NAME.y <- NULL

US_cbg_msa_census <- US_cbg_msa_census[!is.na(US_cbg_msa_census$incomeE),]


###define black, poor neighborhoods
US_cbg_msa_census$black_pct <-US_cbg_msa_census$black/US_cbg_msa_census$poptotal
US_cbg_msa_census$white_pct <- US_cbg_msa_census$white/US_cbg_msa_census$poptotal
US_cbg_msa_census$hispanic_pct <- US_cbg_msa_census$hispanic/US_cbg_msa_census$poptotal
US_cbg_msa_census$poverty_pct <- US_cbg_msa_census$poverty /US_cbg_msa_census$poptotal2


US_cbg_msa_census$Poor <- ifelse(US_cbg_msa_census$poverty_pct > 0.3, "Poor", "Nonpoor")

US_cbg_msa_census$Race <- "Other"  # Default value
US_cbg_msa_census$Race[US_cbg_msa_census$white_pct > 0.5] <- "White"
US_cbg_msa_census$Race[US_cbg_msa_census$black_pct > 0.5] <- "Black"
US_cbg_msa_census$Race[US_cbg_msa_census$hispanic_pct > 0.5] <- "Hispanic"

US_cbg_msa_census$type <- paste0(US_cbg_msa_census$Poor, "-", US_cbg_msa_census$Race)

US_cbg_msa_census <- US_cbg_msa_census[!US_cbg_msa_census$Race=="Other",]
US_cbg_msa_census$density <- (US_cbg_msa_census$sum_count/US_cbg_msa_census$poptotal)*1000
names(US_cbg_msa_census)[2] <- "NAME"
#remove neighborhoods with no sensor
#US_cbg_msa_census_0 <- US_cbg_msa_census[US_cbg_msa_census$n>0,]

#save(US_cbg_msa_census, file="C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_census_coverage.RData")


####only consider major MSAs that are selected
##sensor coverage
neighborhood_count_sensor_results <- data.frame(
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

threshold_values <- seq(0, 0.45, by = 0.05)
for (threshold in threshold_values) {
print(threshold)
  
####only consider major MSAs based on threshold
selected_msa <- US_cbg_msa_sum[US_cbg_msa_sum$percent>threshold,]
selected_msa <- st_drop_geometry(selected_msa)
selected_msa <- selected_msa[,c(1,4,11)]
US_cbg_msa_census2 <- merge(US_cbg_msa_census, selected_msa, by="NAME")
#US_cbg_msa_census2 <- US_cbg_msa_census2[US_cbg_msa_census2$n>0,]
##group based on neighborhood types
neighborhood_count_sensor <-US_cbg_msa_census2 %>% 
  group_by(type)%>% summarise(sensor_count=mean(n))

neighborhood_count_sensor_results <- merge(neighborhood_count_sensor_results, neighborhood_count_sensor, by="type")
names(neighborhood_count_sensor_results)[ncol(neighborhood_count_sensor_results)] <- threshold
}


write.csv(neighborhood_count_sensor_results, 'C:/Users/Dropbox (Personal)/working paper/purpleair/neighborhood_count_sensor_results.csv') 


data_long <- reshape2::melt(neighborhood_count_sensor_results, id.vars = "type", variable.name = "number", value.name = "value")
data_long$number <- as.numeric(as.character(data_long$number))
data_long$number <- 100*data_long$number
data_long$value <- 100*data_long$value

# Plotting all lines on one chart
ggplot(data_long, aes(x = number, y = value, color = type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  xlab("Threshold of America Cities (%)") +
  ylab("Sensor Coverage (%)")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
      panel.grid.major = element_blank(),  # Set color of major grid lines to grey
      panel.grid.minor = element_blank(),
      panel.background = element_blank())  

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/Percent_equity.pdf"
ggsave(png_file0, width = 5, height = 4)


##sensor density over six types of neighborhoods
neighborhood_count_sensor_results <- data.frame(
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

threshold_values <- seq(0.1, 0.45, by = 0.05)

for (threshold in threshold_values) {
  print(threshold)
  
  ####only consider major MSAs based on threshold
  selected_msa <- US_cbg_msa_sum[US_cbg_msa_sum$percent>threshold,]
  selected_msa <- st_drop_geometry(selected_msa)
  selected_msa <- selected_msa[,c(1,4,11)]
  US_cbg_msa_census2 <- merge(US_cbg_msa_census, selected_msa, by="NAME")
  #US_cbg_msa_census2 <- US_cbg_msa_census2[US_cbg_msa_census2$n>0,]
  ##group based on neighborhood types
  neighborhood_count_sensor <-US_cbg_msa_census2 %>% 
    group_by(type)%>% summarise(sensor_density=mean(density))
  
  neighborhood_count_sensor_results <- merge(neighborhood_count_sensor_results, neighborhood_count_sensor, by="type")
  names(neighborhood_count_sensor_results)[ncol(neighborhood_count_sensor_results)] <- threshold
}


data_long <- reshape2::melt(neighborhood_count_sensor_results, id.vars = "type", variable.name = "number", value.name = "value")
data_long$number <- as.numeric(as.character(data_long$number))
data_long$number <- 100*data_long$number


# Plotting all lines on one chart
ggplot(data_long, aes(x = number, y = value, color = type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  xlab("Threshold of America Cities (%)") +
  ylab("Sensor Count per 1000 People")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank())  

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/Density_equity.pdf"
ggsave(png_file0, width = 5, height = 4)


########The coverage over years for neighborhoods###########
############################################################
############################################################
############################################################

load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa.RData")
load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_census_coverage.RData")

#read sensor data
df<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/US_sensor_data.csv")
df$uptime<-round(df$uptime/60, 2)

df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)  # 4326 is the CRS for WGS 84

#start from here
df <- st_transform(df, st_crs(cbg))
US_sensor_cbg <- st_join(df, cbg)

US_sensor_cbg$last_modified <-as.integer(US_sensor_cbg$last_modified)
US_sensor_cbg$last_modified <- as.POSIXct(US_sensor_cbg$last_modified, origin = "1970-01-01")
# Extracting the year from the POSIXct datetime
US_sensor_cbg$year <- format(US_sensor_cbg$last_modified, "%Y")

###merge with equity
US_sensor_cbg <- st_drop_geometry(US_sensor_cbg)
US_cbg_msa_census <-US_cbg_msa_census[,c(1,2,19)]
US_sensor_cbg <- merge(US_sensor_cbg,US_cbg_msa_census, by="GEOID")
US_sensor_cbg$NAME.x <- NULL
names(US_sensor_cbg)[21] <- "NAME"

###2019 data summary
US_sensor_cbg_2019 <- US_sensor_cbg[US_sensor_cbg$year==2019 | 
                                      US_sensor_cbg$year==2018 |
                                      US_sensor_cbg$year==2017|
                                      US_sensor_cbg$year==2016,]

neighborhood_count_sensor_2019 <-US_sensor_cbg_2019 %>% 
  group_by(type)%>% summarise(sensor_count=n())
neighborhood_count_sensor_2019$percent_count <- neighborhood_count_sensor_2019$sensor_count/nrow(US_sensor_cbg_2019)
names(neighborhood_count_sensor_2019)[3] <-'Before 2019'
names(neighborhood_count_sensor_2019)[2] <-'Before 2019 count'


###2020 data summary
US_sensor_cbg_2020 <- US_sensor_cbg[US_sensor_cbg$year==2020,]
neighborhood_count_sensor_2020 <-US_sensor_cbg_2020 %>% 
  group_by(type)%>% summarise(sensor_count=n())
neighborhood_count_sensor_2020$percent_count<- neighborhood_count_sensor_2020$sensor_count/nrow(US_sensor_cbg_2020)
names(neighborhood_count_sensor_2020)[3] <-'2020'
names(neighborhood_count_sensor_2020)[2] <-'2020 count'

###2021 data summary
US_sensor_cbg_2021 <- US_sensor_cbg[US_sensor_cbg$year==2021,]
neighborhood_count_sensor_2021 <-US_sensor_cbg_2021 %>% 
  group_by(type)%>% summarise(sensor_count=n())
neighborhood_count_sensor_2021$percent_count<- neighborhood_count_sensor_2021$sensor_count/nrow(US_sensor_cbg_2021)
names(neighborhood_count_sensor_2021)[3] <-'2021'
names(neighborhood_count_sensor_2021)[2] <-'2021 count'


###2022 data summary
US_sensor_cbg_2022 <- US_sensor_cbg[US_sensor_cbg$year==2022,]
neighborhood_count_sensor_2022 <-US_sensor_cbg_2022 %>% 
  group_by(type)%>% summarise(sensor_count=n())
neighborhood_count_sensor_2022$percent_count<- neighborhood_count_sensor_2022$sensor_count/nrow(US_sensor_cbg_2022)
names(neighborhood_count_sensor_2022)[3] <-'2022'
names(neighborhood_count_sensor_2022)[2] <-'2022 count'


###2023 data summary
US_sensor_cbg_2023 <- US_sensor_cbg[US_sensor_cbg$year==2023,]
neighborhood_count_sensor_2023 <-US_sensor_cbg_2023 %>% 
  group_by(type)%>% summarise(sensor_count=n())
neighborhood_count_sensor_2023$percent_count<- neighborhood_count_sensor_2023$sensor_count/nrow(US_sensor_cbg_2023)
names(neighborhood_count_sensor_2023)[3] <-'2023'
names(neighborhood_count_sensor_2023)[2] <-'2023 count'


#combine all data
neighborhood_count_years <- data.frame(
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

neighborhood_count_years <- merge(neighborhood_count_years, neighborhood_count_sensor_2019, by="type")
neighborhood_count_years <- merge(neighborhood_count_years, neighborhood_count_sensor_2020, by="type")
neighborhood_count_years <- merge(neighborhood_count_years, neighborhood_count_sensor_2021, by="type")
neighborhood_count_years <- merge(neighborhood_count_years, neighborhood_count_sensor_2022, by="type")
neighborhood_count_years <- merge(neighborhood_count_years, neighborhood_count_sensor_2023, by="type")

neighborhood_count_years<- neighborhood_count_years[, c(1,3,5,7,9,11)]


library(tidyr)
library(ggplot2)
library(dplyr)

neighborhood_count_years_long <- neighborhood_count_years %>% 
  pivot_longer(
    cols = starts_with("Before"):`2023`,
    names_to = "Year",
    values_to = "Percent"
  )



neighborhood_count_years_long$Percent <- 100*neighborhood_count_years_long$Percent

# Create a table of counts for the categorical variable
category_counts <- table(US_cbg_msa_census$type)
# Convert counts to proportions
category_proportions <- prop.table(category_counts)
# Convert proportions to percentages
category_percentages <- category_proportions * 100
# If you want a data frame for percent of all neighborhoods
result <- data.frame(Category = names(category_percentages), Percentage = category_percentages)
result$Percentage.Var1<-NULL
names(result)[1] <- "type"

neighborhood_count_years_long$Year <- factor(neighborhood_count_years_long$Year, levels = c("Before 2019", "2019", "2020", "2021", "2022", "2023"))
neighborhood_count_years_long <- merge(neighborhood_count_years_long, result, by="type")
neighborhood_count_years_long$relative_percent <-neighborhood_count_years_long$Percent/neighborhood_count_years_long$Percentage.Freq

# Plot the data
ggplot(neighborhood_count_years_long, aes(x = Year, y = relative_percent, group = type, color = type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Relative Ratio", color = "Type") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add frame around the plot
        panel.grid.major = element_blank(),  # Set color of major grid lines to grey
        panel.grid.minor = element_blank(),
        panel.background = element_blank())  

png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/coverage_years.pdf"
ggsave(png_file0, width = 5, height = 4)
