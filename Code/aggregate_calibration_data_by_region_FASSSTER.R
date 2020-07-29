rm(list=ls()) #remove previous variable assignments

# load library
library(tidyverse)
library(ggplot2)

# load data
uploadDate <- "20200629"
fileName <- paste0("FASSSTER_Data/", uploadDate, "_fasssterCleaned_noQuotes.csv")
data <- read.csv(fileName, head = T, stringsAsFactors = F) 

# format dates
data$Date.Admitted <- as.Date(data$Date.Admitted, "%m/%d/%Y")
data$Report.Date <- as.Date(data$Report.Date, "%m/%d/%Y")
data$Date.Recovered <- as.Date(data$Date.Recovered, "%m/%d/%Y")
data$Date.Died <- as.Date(data$Date.Died, "%m/%d/%Y")
startdate <- as.Date("2019-12-31", "%Y-%m-%d")

# impute dates where needed
data$person_days_adm_to_report <- difftime(data$Report.Date, data$Date.Admitted, unit = "days")

data <- data %>%
  group_by(Region) %>%
  summarise(region_days_adm_to_report = round(mean(person_days_adm_to_report, na.rm=T))) %>%
  left_join(data, by = "Region") %>%
  filter(Region == "NCR" |
           Region == "4A" | # Calabarzon
           Region == "5" | # Bicol
           Region == "7") # Central Visayas

data$Date.Admitted.adjusted <- if_else(is.na(data$Date.Admitted), (data$Report.Date - data$region_days_adm_to_report), data$Date.Admitted)

# rename regions
data$Region[data$Region == "4A"] <- "Region_IV_A_Calabarzon"
data$Region[data$Region == "5"] <- "Region_V_Bicol"
data$Region[data$Region == "7"] <- "Region_VII_Central_Visayas"
regions <- unique(data$Region)

# deaths --------------------------------------------------------------------------
# aggregate daily death data
deaths_aggregate <- data %>%
  group_by(Region, Date.Died) %>%
  summarise(deaths_values = length(Date.Died)) %>%
  filter(!is.na(Date.Died))

# plot
death <- ggplot(data=deaths_aggregate, aes(x=Date.Died, y=deaths_values, group=Region)) +
  geom_line()+
  geom_point() +
  facet_wrap(~Region, scales="free") +
  theme_bw() +
  ylab("Deaths") +
  xlab("Date")

ggsave("Figures/deaths_by_region.tiff")

# calibration times
deaths_aggregate$deaths_times <- difftime(deaths_aggregate$Date.Died, startdate, units = "days")

# add commas and brackets for python formatting, save by region
for(i in regions){
  tmpdf <- subset(deaths_aggregate, Region == i & deaths_times > 40)
  tmpdf <- tmpdf[,c("deaths_times", "deaths_values")]
  tmpdf <- tmpdf[complete.cases(tmpdf),]
  tmpdf$deaths_times2 <- paste(tmpdf$deaths_times, collapse=", ")
  tmpdf$deaths_times <- paste0(tmpdf$deaths_times, ",")
  deaths_values2 <- paste("[", tmpdf$deaths_values, "]", collapse=",")
  deaths_values2 <- gsub(" ", "", deaths_values2)
  tmpdf$deaths_values2 <- deaths_values2
  tmpdf$deaths_values <- paste0(tmpdf$deaths_values, ",")
  filename <- paste0("Output/", i, "_calibration_deaths.csv")
  write.csv(tmpdf, filename, row.names=F)
}

# notifications ------------------------------------------------------------------
# aggregate daily notifications
notifications_aggregate <- data %>%
  group_by(Region, Date.Admitted.adjusted) %>%
  summarise(notifications_values = length(Date.Admitted.adjusted)) 

# plot
notification <- ggplot(data=notifications_aggregate, aes(x=Date.Admitted.adjusted, y=notifications_values, group=Region)) +
  geom_line()+
  geom_point() +
  facet_wrap(~Region, scales="free") +
  theme_bw() +
  ylab("Notified cases") +
  xlab("Date")

ggsave("Figures/notifications_by_region.tiff")

# calibration times
notifications_aggregate$notifications_times <- difftime(notifications_aggregate$Date.Admitted.adjusted, startdate, units = "days")

# add commas and brackets for python formatting, save by region
for(i in regions){
  tmpdf <- subset(notifications_aggregate, Region == i & notifications_times > 40)
  tmpdf <- tmpdf[,c("notifications_times", "notifications_values")]
  tmpdf <- tmpdf[complete.cases(tmpdf),]
  tmpdf$notifications_times2 <- paste(tmpdf$notifications_times, collapse=", ")
  tmpdf$notifications_times <- paste0(tmpdf$notifications_times, ",")
  notifications_values2 <- paste("[", tmpdf$notifications_values, "]", collapse=",")
  notifications_values2 <- gsub(" ", "", notifications_values2)
  tmpdf$notifications_values2 <- notifications_values2
  tmpdf$notifications_values <- paste0(tmpdf$notifications_values, ",")
  filename <- paste0("Output/", i, "_calibration_notifications.csv")
  write.csv(tmpdf, filename, row.names=F)
}
