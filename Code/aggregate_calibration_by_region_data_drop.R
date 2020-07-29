rm(list=ls()) #remove previous variable assignments

# load library
library(tidyverse)
library(ggplot2)

# important dates
uploadDate <- "20200628"

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
icu_times <- paste(icu_aggregate$icu_times, collapse=", ")
icu_values2 <- paste("[", icu_aggregate$icu_values, "]", collapse=",")
icu_values2 <- gsub(" ", "", icu_values2)

icu_aggregate$icu_times <- paste0("     ", format(unlist(icu_aggregate$icu_times)),",")
icu_aggregate$icu_times <- gsub(" days", "", icu_aggregate$icu_times)
icu_aggregate$icu_values <- paste0("     ", format(unlist(icu_aggregate$icu_values)),",")

# save by region
regions <- unique(icu_aggregate$region)

for(i in regions){
  tmpdf <- subset(icu_aggregate, region == i)
  tmpdf <- tmpdf[,c("icu_times", "icu_values")]
  tmpdf <- tmpdf[complete.cases(tmpdf),]
  tmpdf <- cbind(tmpdf, icu_times, icu_values2)
  filename <- paste0("Output/", i, "_calibration_icu.csv")
  write.csv(tmpdf, filename, row.names=F)
}
