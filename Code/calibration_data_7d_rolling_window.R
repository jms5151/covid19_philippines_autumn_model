# Philippines weekly calibration data aggregation -------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)
library(zoo)
library(ggplot2)

# load data
uploadDate <- "2020-08-22"
fileName <- paste0("FASSSTER_Data/ConfirmedCases_Final_", uploadDate, ".csv")
fassster_data <- read.csv(fileName, head = T, stringsAsFactors = F) 

uploadDate2 <- "20200823"
fileName2 <- paste0("DOH_COVID_Data_Drop/DOH COVID Data Drop_ ", uploadDate2, " - 05 DOH Data Collect - Daily Report.csv")
doh_data <- read.csv(fileName2, head = T, stringsAsFactors = F) 

# format dates
fassster_data$imputed_Date_Admitted <- as.Date(fassster_data$imputed_Date_Admitted, "%Y-%m-%d")
fassster_data$Date_Died <- as.Date(fassster_data$Date_Died, "%Y-%m-%d")
doh_data$reportdate <- as.Date(doh_data$reportdate, "%Y-%m-%d")
startdate <- as.Date("2019-12-31", "%Y-%m-%d")

# format region names
fassster_data$Region[fassster_data$Region == "NCR"] <- "manila"
fassster_data$Region[fassster_data$Region == "4A"] <- "calabarzon"
fassster_data$Region[fassster_data$Region == "07"] <- "central-visayas"

doh_data$Region <- doh_data$region
doh_data$Region[doh_data$Region == "NATIONAL CAPITAL REGION (NCR)"] <- "manila"
doh_data$Region[doh_data$Region == "REGION IV-A (CALABAR ZON)"] <- "calabarzon"
doh_data$Region[doh_data$Region == "REGION VII (CENTRAL VISAYAS)"] <- "central-visayas"

# duplicate data and combine with original dataset to calculate national and regional estimates simultaneously
dup_fassster_data <- fassster_data
dup_fassster_data$Region <- "philippines"
fassster_data2 <- rbind(fassster_data, dup_fassster_data)

dup_doh_data <- doh_data
dup_doh_data$Region <- "philippines"
doh_data2 <- rbind(doh_data, dup_doh_data)

# aggregate notification data to weekly values 
notifications <- fassster_data2 %>%
  rename(Date = imputed_Date_Admitted) %>%
  filter(Date <= as.Date(uploadDate, "%Y-%m-%d")) %>%
  filter(Region == "philippines" | Region == "manila" | Region == "calabarzon" | Region == "central-visayas") %>% 
  group_by(Region, Date) %>%
  summarise(values = length(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
  mutate(values = round(rollapply(values, width = 7, FUN = mean, align = "left", fill = NA, na.rm = T)))

# aggregate death data to weekly values 
deaths <- fassster_data2 %>%
  rename(Date = Date_Died) %>%
  filter(Date <= as.Date(uploadDate, "%Y-%m-%d")) %>%
  filter(Region == "philippines" | Region == "manila" | Region == "calabarzon" | Region == "central-visayas") %>% 
  group_by(Region, Date) %>%
  summarise(values = length(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
  mutate(values = round(rollapply(values, width = 7, FUN = mean, align = "left", fill = NA, na.rm = T)))


# aggregate ICU data to weekly values 
icu <- doh_data2 %>%
  rename(Date = reportdate) %>%
  filter(Date <= as.Date(uploadDate, "%Y-%m-%d")) %>%
  filter(Region == "philippines" | Region == "manila" | Region == "calabarzon" | Region == "central-visayas") %>% 
  group_by(Region, Date) %>%
  summarise(values = sum(icu_o, na.rm = T)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
  mutate(values = round(rollapply(values, width = 7, FUN = mean, align = "left", fill = NA, na.rm = T)))

# format, plot, and save data
dfs <- list(notifications, deaths, icu)
names <- c("notifications", "deaths", "icu")
regions <- c("calabarzon", "central-visayas", "manila", "philippines")
deathtimes <- c(153, 92, 183, 153)

for(i in 1:length(dfs)){
  # subset by calibration target
  dfx <- dfs[[i]]
  dfx$values <- round(dfx$values)
  dfx$times <- as.numeric(difftime(dfx$Date, startdate, units = "days"))
  # plot
  fig <- ggplot(data=dfx, aes(x=Date, y=values, group=Region)) +
    geom_line()+
    geom_point() +
    facet_wrap(~Region, scales="free") +
    theme_bw() +
    ylab(paste0("Mean daily", names[i])) +
    xlab("Date")
  ggsave(paste0("Figures/", uploadDate, "_", names[i], "_by_region.tiff"))
  # subset by region
  for(j in 1:length(regions)){
    dfx2 <- subset(dfx, Region == regions[j])
    if(names[i] == "deaths"){
      dfx2 <- subset(dfx2, times > deathtimes[j] & times < max(times)-13)  
    } else {
      dfx2 <- subset(dfx2, times > 60 & times < max(times)-13)  
    }
    dfx2 <- dfx2[complete.cases(dfx2),]
    # format and save data for calibration targets
    dfx2$times <- paste0(dfx2$times, ",")
    dfx2$values <- paste0(dfx2$values, ",")
    write.csv(dfx2[,c("times", "values")], paste0("Output/", uploadDate, "_", regions[j], "_", names[i], "_long.csv"), row.names = F)
  }
}
