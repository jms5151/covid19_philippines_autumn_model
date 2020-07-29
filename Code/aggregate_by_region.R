# load library
library(tidyverse)
library(ggplot2)

# important dates
uploadDate <- "20200624"

startdate <- as.Date("2019-12-31", "%Y-%m-%d")

# ICU beds -------------------------------------------------------
# aggregate daily icu beds by region
icufileName <- paste0("DOH COVID Data Drop_", uploadDate, "/DOH COVID Data Drop_ ", uploadDate, " - 05 DOH Data Collect - Daily Report.csv")
icuInfo <- read.csv(icufileName, head = T, stringsAsFactors = F)

icuInfo$reportdate <- as.Date(icuInfo$reportdate, "%Y-%m-%d")

icu_aggregate <- icuInfo %>%
  group_by(region, reportdate) %>%
  summarise(icu_values = sum(icu_o)) %>%
  filter(region == "NATIONAL CAPITAL REGION (NCR)" |
           region == "REGION IV-A (CALABAR ZON)" |
           region == "REGION V (BICOL REGION)" |
           region == "REGION VII (CENTRAL VISAYAS)") %>%
  filter(!is.na(reportdate))

# plot
icu_plot <- ggplot(data=icu_aggregate, aes(x=reportdate, y=icu_values, group=region)) +
  geom_line()+
  geom_point() +
  facet_wrap(~region, scales="free") +
  theme_bw() +
  ylab("ICU beds") +
  xlab("Date")

ggsave("Figures/ICU_beds_by_region.tiff")

# rename regions
icu_aggregate$region[icu_aggregate$region == "NATIONAL CAPITAL REGION (NCR)"] <- "NCR"
icu_aggregate$region[icu_aggregate$region == "REGION IV-A (CALABAR ZON)"] <- "Region_IV_A_Calabarzon"
icu_aggregate$region[icu_aggregate$region == "REGION V (BICOL REGION)"] <- "Region_V_Bicol"
icu_aggregate$region[icu_aggregate$region == "REGION VII (CENTRAL VISAYAS)"] <- "Region_VII_Central_Visayas"

# calibration times
icu_aggregate$icu_times <- difftime(icu_aggregate$reportdate,  startdate, units = "days")

# add commas and brackets for python formatting
icu_aggregate$icu_times <- paste0("    ", format(unlist(icu_aggregate$icu_times)),",")
icu_aggregate$icu_times <- gsub(" days", "", icu_aggregate$icu_times)
icu_aggregate$icu_values2 <- paste0("     [", format(unlist(icu_aggregate$icu_values)),"],") 
icu_aggregate$icu_values <- paste0("     ", format(unlist(icu_aggregate$icu_values)),",") 

# save by region
regions <- unique(icu_aggregate$region)

for(i in regions){
  tmpdf <- subset(icu_aggregate, region == i)
  tmpdf <- tmpdf[,c("icu_times", "icu_values", "icu_values2")]
  tmpdf <- tmpdf[complete.cases(tmpdf),]
  filename <- paste0("Output/", i, "_calibration_icu.csv")
  write.csv(tmpdf, filename, row.names=F)
}











# # aggregate with data drop data
# # deaths --------------------------------------------------------
# casesfileName <- paste0("DOH COVID Data Drop_", uploadDate, "/DOH COVID Data Drop_ ", uploadDate, " - 04 Case Information.csv")
# casesInfo <- read.csv(casesfileName, head = T, stringsAsFactors = F) 
# 
# casesInfo$DateDied <- as.Date(casesInfo$DateDied, "%Y-%m-%d")
# 
# deaths_aggregate <- casesInfo %>%
#   group_by(RegionRes, DateDied) %>%
#   summarise(deaths_values = length(DateDied)) %>%
#   filter(RegionRes == "NCR" |
#          RegionRes == "Region IV-A: CALABARZON" |
#          RegionRes == "Region V: Bicol Region" |
#          RegionRes == "Region VII: Central Visayas") %>%
#   filter(!is.na(DateDied))
# 
# ggplot(data=deaths_aggregate, aes(x=DateDied, y=deaths_values, group=RegionRes)) +
#   geom_line()+
#   geom_point() +
#   facet_wrap(~RegionRes, scales="free") +
#   theme_bw() +
#   ylab("Deaths") +
#   xlab("Date")
# 
# # rename regions
# deaths_aggregate$RegionRes[deaths_aggregate$RegionRes == "Region IV-A: CALABARZON"] <- "Region_IV_A_Calabarzon"
# deaths_aggregate$RegionRes[deaths_aggregate$RegionRes == "Region V: Bicol Region"] <- "Region_V_Bicol"
# deaths_aggregate$RegionRes[deaths_aggregate$RegionRes == "Region VII: Central Visayas"] <- "Region_VII_Central_Visayas"
# 
# # calibration times
# deaths_aggregate$deaths_times <- difftime(deaths_aggregate$DateDied,  startdate, units = "days")
# 
# # save by region
# regions <- unique(deaths_aggregate$RegionRes)
# 
# for(i in regions){
#   tmpdf <- subset(deaths_aggregate, RegionRes == i)
#   tmpdf <- tmpdf[,c("deaths_times", "deaths_values")]
#   tmpdf <- tmpdf[complete.cases(tmpdf),]
#   deaths_times <- paste(tmpdf$deaths_times, collapse=", ")
#   deaths_values <- paste(tmpdf$deaths_values, collapse=", ")
#   deaths_values2 <- paste("[", tmpdf$deaths_values, "]", collapse=",")
#   deaths_values2 <- gsub(" ", "", deaths_values2)
#   tmpdf2 <- data.frame(deaths_times, deaths_values, deaths_values2)
#   filename <- paste0("Output/", i, "_calibration_deaths.csv")
#   write.csv(tmpdf2, filename, row.names=F)
# }
# 
# # cases based on date symptom onset -----------------------------
# casesInfo$DateOnset <- as.Date(casesInfo$DateOnset, "%Y-%m-%d")
# 
# notifications_aggregate <- casesInfo %>%
#   group_by(RegionRes, DateOnset) %>%
#   summarise(notifications_values = length(DateOnset)) %>%
#   filter(RegionRes == "NCR" |
#            RegionRes == "Region IV-A: CALABARZON" |
#            RegionRes == "Region V: Bicol Region" |
#            RegionRes == "Region VII: Central Visayas") %>%
#   filter(!is.na(DateOnset))
# 
# ggplot(data=notifications_aggregate, aes(x=DateOnset, y=notifications_values, group=RegionRes)) +
#   geom_line()+
#   geom_point() +
#   facet_wrap(~RegionRes, scales="free") +
#   theme_bw() +
#   ylab("Notified cases") +
#   xlab("Date")
# 
# # rename regions
# notifications_aggregate$RegionRes[notifications_aggregate$RegionRes == "Region IV-A: CALABARZON"] <- "Region_IV_A_Calabarzon"
# notifications_aggregate$RegionRes[notifications_aggregate$RegionRes == "Region V: Bicol Region"] <- "Region_V_Bicol"
# notifications_aggregate$RegionRes[notifications_aggregate$RegionRes == "Region VII: Central Visayas"] <- "Region_VII_Central_Visayas"
# 
# # calibration times
# notifications_aggregate$notifications_times <- difftime(notifications_aggregate$DateOnset,  startdate, units = "days")
# 
# # save by region
# regions <- unique(notifications_aggregate$RegionRes)
# 
# for(i in regions){
#   tmpdf <- subset(notifications_aggregate, RegionRes == i)
#   tmpdf <- tmpdf[,c("notifications_times", "notifications_values")]
#   tmpdf <- tmpdf[complete.cases(tmpdf),]
#   notifications_times <- paste(tmpdf$notifications_times, collapse=", ")
#   notifications_values <- paste(tmpdf$notifications_values, collapse=", ")
#   notifications_values2 <- paste("[", tmpdf$notifications_values, "]", collapse=",")
#   notifications_values2 <- gsub(" ", "", notifications_values2)
#   tmpdf2 <- data.frame(notifications_times, notifications_values, notifications_values2)
#   filename <- paste0("Output/", i, "_calibration_notifications.csv")
#   write.csv(tmpdf2, filename, row.names=F)
# }
# 