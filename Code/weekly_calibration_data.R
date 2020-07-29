# Philippines weekly calibration data aggregation -------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)
library(tidyquant)
library(ggplot2)

# load data
uploadDate <- "20200722"
fileName <- paste0("FASSSTER_Data/fassster_linelist.", uploadDate, ".csv")
data <- read.csv(fileName, head = T, stringsAsFactors = F) 

# format dates
data$Date.Admitted <- as.Date(data$Date.Admitted, "%Y-%m-%d")
data$Report.Date <- as.Date(data$Report.Date, "%Y-%m-%d")
data$Date.Recovered <- as.Date(data$Date.Recovered, "%Y-%m-%d")
data$Date.Died <- as.Date(data$Date.Died, "%Y-%m-%d")
startdate <- as.Date("2019-12-31", "%Y-%m-%d")

# impute dates where needed
data$person_days_adm_to_report <- difftime(data$Report.Date, data$Date.Admitted, unit = "days")

df1 <- data %>%
  group_by(Region) %>%
  summarise(region_days_adm_to_report = round(mean(person_days_adm_to_report, na.rm=T))) %>%
  left_join(data, by = "Region") 

df1$Date.Admitted.adjusted <- if_else(is.na(df1$Date.Admitted), (df1$Report.Date - df1$region_days_adm_to_report), df1$Date.Admitted)

# National --------------------------------------------------------------------------------------
notifications_aggregate <- df1 %>%
  group_by(Date.Admitted.adjusted) %>%
  summarise(notifications_values = length(Date.Admitted.adjusted)) %>%
  tq_transmute(select     = notifications_values,
               mutate_fun = apply.weekly,
               FUN        = mean) %>%
  filter(Date.Admitted.adjusted <= as.Date(uploadDate, "%Y%m%d"))

notifications_aggregate$notifications_values <- round(notifications_aggregate$notifications_values)

# plot
notification <- ggplot(data=notifications_aggregate, aes(x=Date.Admitted.adjusted, y=notifications_values)) +
  geom_line()+
  geom_point() +
  theme_bw() +
  ylab("Notified cases") +
  xlab("Date")

ggsave("Figures/notifications_national.tiff")

# calibration times
notifications_aggregate$notifications_times <- difftime(notifications_aggregate$Date.Admitted.adjusted, startdate, units = "days")

# remove data before day 40 and remove last few time points due to lag in reporting
notifications_aggregate <- subset(notifications_aggregate, notifications_times > 40 & 
                                    notifications_times < 
                                    notifications_aggregate$notifications_times[(nrow(notifications_aggregate))])

# save for powerbi
write.csv(notifications_aggregate[,c("notifications_times", "notifications_values")], "Output/calibration_notifications_philippines_powerbi.csv")

# add commas and brackets for python formatting
notifications_times <- paste(notifications_aggregate$notifications_times, collapse=", ")
notifications_values2 <- paste("[", notifications_aggregate$notifications_values, "]", collapse=",")
notifications_values2 <- gsub(" ", "", notifications_values2)

notifications_aggregate$notifications_times <- paste0("     ", format(unlist(notifications_aggregate$notifications_times)),",")
notifications_aggregate$notifications_times <- gsub(" days", "", notifications_aggregate$notifications_times)
notifications_aggregate$notifications_values <- paste0("     ", format(unlist(notifications_aggregate$notifications_values)),",")

notifications_aggregate <- notifications_aggregate[,c("notifications_times", "notifications_values")]
notifications_aggregate <- notifications_aggregate[complete.cases(notifications_aggregate),]
notifications_aggregate <- cbind(notifications_aggregate, notifications_times, notifications_values2)

# save
write.csv(notifications_aggregate, "Output/calibration_notifications.csv", row.names=F)

# regional --------------------------------------------------------------------------------------
df2 <- data %>%
  group_by(Region) %>%
  summarise(region_days_adm_to_report = round(mean(person_days_adm_to_report, na.rm=T))) %>%
  left_join(data, by = "Region") %>%
  filter(Region == "NCR" |
           Region == "4A" | # Calabarzon
           Region == "07") # Central Visayas

df2$Date.Admitted.adjusted <- if_else(is.na(df2$Date.Admitted), (df2$Report.Date - df2$region_days_adm_to_report), df2$Date.Admitted)

# rename regions
df2$Region[df2$Region == "4A"] <- "Region_IV_A_Calabarzon"
df2$Region[df2$Region == "07"] <- "Region_VII_Central_Visayas"
regions <- unique(df2$Region)

notifications_aggregate <- df2 %>%
  group_by(Region, Date.Admitted.adjusted) %>%
  summarise(notifications_values = length(Date.Admitted.adjusted)) %>%
  tq_transmute(select     = notifications_values,
               mutate_fun = apply.weekly,
               FUN        = mean) %>%
  filter(Date.Admitted.adjusted <= as.Date(uploadDate, "%Y%m%d")) %>%
  filter(Date.Admitted.adjusted < as.Date("2020-07-17", "%Y-%m-%d"))

notifications_aggregate$notifications_values <- round(notifications_aggregate$notifications_values)

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
    # if (i == "NCR"){
    # tmpdf <- subset(notifications_aggregate, Region == i & notifications_times > 40 &
    #                   notifications_times <
    #                   notifications_aggregate$notifications_times[(nrow(notifications_aggregate))])
  # } else {
  #   tmpdf <- subset(notifications_aggregate, Region == i & notifications_times > 40 & 
  #                     notifications_times < 
  #                     notifications_aggregate$notifications_times[(nrow(notifications_aggregate)-1)])
  # }
  tmpdf <- tmpdf[,c("notifications_times", "notifications_values")]
  tmpdf <- tmpdf[complete.cases(tmpdf),]
  write.csv(tmpdf, paste0("Output/", i, "_calibration_notifications_powerbi.csv"), row.names=F)
  tmpdf$notifications_times2 <- paste(tmpdf$notifications_times, collapse=", ")
  tmpdf$notifications_times <- paste0(tmpdf$notifications_times, ",")
  notifications_values2 <- paste("[", tmpdf$notifications_values, "]", collapse=",")
  notifications_values2 <- gsub(" ", "", notifications_values2)
  tmpdf$notifications_values2 <- notifications_values2
  tmpdf$notifications_values <- paste0(tmpdf$notifications_values, ",")
  filename <- paste0("Output/", i, "_calibration_notifications.csv")
  write.csv(tmpdf, filename, row.names=F)
}

