#get the sensor and environment mismatch
library(ggplot2)
library(sf)
library(ggalt)
library(tmap)
library(data.table)
library(tidycensus)
library(dplyr)
library(MASS)


aqs<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/data/aqs_sites/aqs_sites.csv")
aqs <- aqs[is.na(aqs$`Site Closed Date`), ]
aqs <- aqs[aqs$Latitude!=0, ]
aqs <- aqs[!is.na(aqs$Latitude), ]

aqs <- st_as_sf(aqs, coords = c("Longitude", "Latitude"), crs = 4326)  # 4326 is the CRS for WGS 84
aqs <- aqs[,c('Address')]

load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/cbg.RData")


#start from here
aqs <- st_transform(aqs, st_crs(cbg))
aqs_cbg <- st_join(aqs, cbg)
aqs_cbg <- aqs_cbg[,c(1,2)]
aqs_cbg <- st_drop_geometry(aqs_cbg)

load("C:/Users/Dropbox (Personal)/working paper/purpleair/data/US_cbg_msa_census_coverage.RData")
US_cbg_msa_census <- US_cbg_msa_census[,c("GEOID", "type")]

aqs_cbg <- merge(aqs_cbg, US_cbg_msa_census, by="GEOID")

# Get the count of each type
type_counts <- table(aqs_cbg$type)

count_df <- data.frame(Type = names(type_counts), Count = as.integer(type_counts))



# Calculate the percentage of each type
type_percentages <- (type_counts / sum(type_counts)) * 100

# If you want to have it as a dataframe:
percentage_df <- data.frame(Type = names(type_percentages), Percentage = type_percentages)

# Display the percentages
percentage_df


###
#read sensor data
df<-fread("C:/Users/Dropbox (Personal)/working paper/purpleair/US_sensor_data.csv")
df$uptime<-round(df$uptime/60, 2)

df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)  # 4326 is the CRS for WGS 84
df <- st_transform(df, st_crs(cbg))
US_sensor_cbg <- st_join(df, cbg)
US_sensor_cbg <- US_sensor_cbg[,c(1,10)]
US_sensor_cbg <- st_drop_geometry(US_sensor_cbg)
US_sensor_cbg <- merge(US_sensor_cbg, US_cbg_msa_census, by="GEOID")

# Get the count of each type
type_counts2 <- table(US_sensor_cbg$type)
count_df2 <- data.frame(Type = names(type_counts2), Count = as.integer(type_counts2))


new_df <- merge(count_df, count_df2, by="Type")
new_df$sum <-new_df$Count.x+new_df$Count.y

new_df$EPA_pt <-new_df$Count.x/2612
new_df$new_pct <-new_df$sum/17915


type_counts22 <- table(US_cbg_msa_census$type)

# Calculate the percentage of each type
type_percentages2 <- (type_counts22 / sum(type_counts22))

# If you want to have it as a dataframe:
percentage_df2 <- data.frame(Type = names(type_percentages2), Percentage = type_percentages2)

# Display the percentages
percentage_df2$Percentage.Var1 <- NULL

new_df <- merge(new_df, percentage_df2, by="Type")
new_df$relative_EPA <- new_df$EPA_pt/new_df$Percentage.Freq
new_df$relative_new <- new_df$new_pct/new_df$Percentage.Freq

new_df$Type <- factor(new_df$Type, levels= c("Poor-White", 
                                             "Poor-Black", 
                                             "Poor-Hispanic",
                                             "Nonpoor-White",
                                             "Nonpoor-Hispanic",
                                             "Nonpoor-Black"))

new_df <- new_df %>% 
  arrange(match(Type, levels(Type)))


##visualize dumbbell
gg <- ggplot(new_df, aes(x=relative_EPA, xend=relative_new, y=Type)) + 
  geom_dumbbell(colour_x ="#0e668b", color="#e3e2e1",
                size=3, 
                colour_xend ="red")+
        labs(x=NULL, 
             y=NULL) +
        theme(
              plot.background=element_blank(),
              panel.background=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major.y=element_blank(),
              panel.grid.major.x=element_line(),
              axis.ticks=element_blank(),
              legend.position="top",
              panel.border=element_rect(colour = "black", fill=NA, size=1))


gg
png_file0 <- "C:/Users/Dropbox (Personal)/working paper/purpleair/figures/dumbbell.pdf"
ggsave(png_file0, width = 4.5, height =5)
