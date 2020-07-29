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
  left_join(data, by = "Region") 

data$Date.Admitted.adjusted <- if_else(is.na(data$Date.Admitted), (data$Report.Date - data$region_days_adm_to_report), data$Date.Admitted)

# deaths --------------------------------------------------------------------------
# aggregate daily death data
deaths_aggregate <- data %>%
  group_by(Date.Died) %>%
  summarise(deaths_values = length(Date.Died)) %>%
  filter(!is.na(Date.Died))

# plot
deaths <- ggplot(data=deaths_aggregate, aes(x=Date.Died, y=deaths_values)) +
  geom_line()+
  geom_point() +
  theme_bw() +
  ylab("Deaths") +
  xlab("Date")

ggsave("Figures/deaths_national.tiff")

# calibration times
deaths_aggregate$deaths_times <- difftime(deaths_aggregate$Date.Died, startdate, units = "days")

# remove data before day 40
deaths_aggregate <- subset(deaths_aggregate, deaths_times > 40)

# add commas and brackets for python formatting
deaths_times <- paste(deaths_aggregate$deaths_times, collapse=", ")
deaths_values2 <- paste("[", deaths_aggregate$deaths_values, "]", collapse=",")
deaths_values2 <- gsub(" ", "", deaths_values2)

deaths_aggregate$deaths_times <- paste0("     ", format(unlist(deaths_aggregate$deaths_times)),",")
deaths_aggregate$deaths_times <- gsub(" days", "", deaths_aggregate$deaths_times)
deaths_aggregate$deaths_values <- paste0("     ", format(unlist(deaths_aggregate$deaths_values)),",")

deaths_aggregate <- deaths_aggregate[,c("deaths_times", "deaths_values")]
deaths_aggregate <- deaths_aggregate[complete.cases(deaths_aggregate),]
deaths_aggregate <- cbind(deaths_aggregate, deaths_times, deaths_values2)

# save
write.csv(deaths_aggregate, "Output/calibration_deaths.csv", row.names=F)

# notifications ------------------------------------------------------------------
# aggregate daily notifications
notifications_aggregate <- data %>%
  group_by(Date.Admitted.adjusted) %>%
  summarise(notifications_values = length(Date.Admitted.adjusted)) 

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

# remove data before day 40
notifications_aggregate <- subset(notifications_aggregate, notifications_times > 40)

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

# ICU beds ----------------------------------------------------------------------
# important dates
uploadDate <- "20200628"

# aggregate daily icu beds
icufileName <- paste0("DOH COVID Data Drop_", uploadDate, "/DOH COVID Data Drop_ ", uploadDate, " - 05 DOH Data Collect - Daily Report.csv")
icuInfo <- read.csv(icufileName, head = T, stringsAsFactors = F)

icuInfo$reportdate <- as.Date(icuInfo$reportdate, "%Y-%m-%d")

icu_aggregate <- icuInfo %>%
  group_by(reportdate) %>%
  summarise(icu_values = sum(icu_o)) %>%
  filter(!is.na(reportdate))

# plot
icu_plot <- ggplot(data=icu_aggregate, aes(x=reportdate, y=icu_values)) +
  geom_line()+
  geom_point() +
  theme_bw() +
  ylab("ICU beds") +
  xlab("Date")

ggsave("Figures/ICU_beds_national.tiff")

# calibration times
icu_aggregate$icu_times <- difftime(icu_aggregate$reportdate,  startdate, units = "days")

# remove data before day 40
icu_aggregate <- subset(icu_aggregate, icu_times > 40)

# add commas and brackets for python formatting
icu_times <- paste(icu_aggregate$icu_times, collapse=", ")
icu_values2 <- paste("[", icu_aggregate$icu_values, "]", collapse=",")
icu_values2 <- gsub(" ", "", icu_values2)

icu_aggregate$icu_times <- paste0("     ", format(unlist(icu_aggregate$icu_times)),",")
icu_aggregate$icu_times <- gsub(" days", "", icu_aggregate$icu_times)
icu_aggregate$icu_values <- paste0("     ", format(unlist(icu_aggregate$icu_values)),",")

icu_aggregate <- icu_aggregate[,c("icu_times", "icu_values")]
icu_aggregate <- icu_aggregate[complete.cases(icu_aggregate),]
icu_aggregate <- cbind(icu_aggregate, icu_times, icu_values2)

# save 
write.csv(icu_aggregate, "Output/calibration_icu.csv", row.names=F)
