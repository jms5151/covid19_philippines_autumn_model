# Philippines weekly calibration data aggregation -------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)
library(tidyquant)
library(ggplot2)

# load data
uploadDate <- "2020-07-28"
fileName <- paste0("FASSSTER_Data/ConfirmedCases_Final_", uploadDate, ".csv")
fassster_data <- read.csv(fileName, head = T, stringsAsFactors = F) 

uploadDate2 <- "20200728"
fileName2 <- paste0("DOH_COVID_Data_Drop/DOH COVID Data Drop_ ", uploadDate2, " - 05 DOH Data Collect - Daily Report.csv")
doh_data <- read.csv(fileName2, head = T, stringsAsFactors = F) 
  
# format dates
fassster_data$imputed_Date_Admitted <- as.Date(fassster_data$imputed_Date_Admitted, "%Y-%m-%d")
fassster_data$Date_Died <- as.Date(fassster_data$Date_Died, "%Y-%m-%d")
doh_data$reportdate <- as.Date(doh_data$reportdate, "%Y-%m-%d")
startdate <- as.Date("2019-12-31", "%Y-%m-%d")

# format region names
fassster_data$Region[fassster_data$Region == "4A"] <- "CAL"
fassster_data$Region[fassster_data$Region == "07"] <- "CEN-VIS"

doh_data$Region <- doh_data$region
doh_data$Region[doh_data$Region == "NATIONAL CAPITAL REGION (NCR)"] <- "NCR"
doh_data$Region[doh_data$Region == "REGION IV-A (CALABAR ZON)"] <- "CAL"
doh_data$Region[doh_data$Region == "REGION VII (CENTRAL VISAYAS)"] <- "CEN-VIS"

# duplicate data and combine with original dataset to calculate national and regional estimates simultaneously
dup_fassster_data <- data
dup_fassster_data$Region <- "PHL"
fassster_data2 <- rbind(fassster_data, dup_fassster_data)

dup_doh_data <- doh_data
dup_doh_data$Region <- "PHL"
doh_data2 <- rbind(doh_data, dup_doh_data)

# aggregate notification data to weekly values 
notifications <- fassster_data2 %>%
  rename(Date = imputed_Date_Admitted) %>%
  filter(Date <= as.Date(uploadDate, "%Y-%m-%d")) %>%
  filter(Region == "PHL" | Region == "NCR" | Region == "CAL" | Region == "CEN-VIS") %>% 
  group_by(Region, Date) %>%
  summarise(values = length(Date)) %>%
  tq_transmute(select     = values,
               mutate_fun = apply.weekly,
               FUN        = mean) 

# aggregate death data to weekly values 
deaths <- fassster_data2 %>%
  rename(Date = Date_Died) %>%
  filter(Date <= as.Date(uploadDate, "%Y-%m-%d")) %>%
  filter(Region == "PHL" | Region == "NCR" | Region == "CAL" | Region == "CEN-VIS") %>% 
  group_by(Region, Date) %>%
  summarise(values = length(Date)) %>%
  tq_transmute(select     = values,
               mutate_fun = apply.weekly,
               FUN        = mean) 

# aggregate ICU data to weekly values 
icu <- doh_data2 %>%
  rename(Date = reportdate) %>%
  filter(Date <= as.Date(uploadDate, "%Y-%m-%d")) %>%
  filter(Region == "PHL" | Region == "NCR" | Region == "CAL" | Region == "CEN-VIS") %>% 
  group_by(Region, Date) %>%
  summarise(values = sum(icu_o)) %>%
  tq_transmute(select     = values,
               mutate_fun = apply.weekly,
               FUN        = mean) 

# format, plot, and save data
dfs <- list(notifications, deaths, icu)
names <- c("notifications", "deaths", "icu")
regions <- unique(notifications$Region)

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
    ylab(paste0("Mean ", names[i], " per week")) +
    xlab("Date")
  ggsave(paste0("Figures/", names[i], "_by_region_", uploadDate, ".tiff"))
  # subset by region
  for(j in regions){
    dfx2 <- subset(dfx, Region == j)
    dfx2 <- subset(dfx2, times > 40 & times < max(times))
    # format and save data for calibration plots
    times_wide <- paste(dfx2$times, collapse=", ")
    values_wide <- paste("[", dfx2$values, "]", collapse=",")
    values_wide <- gsub(" ", "", values_wide)
    plot_targets <- c(times_wide, values_wide)
    writeLines(plot_targets, paste0("Output/", j, "_", names[i], "_wide_", uploadDate, ".txt"))
    # format and save data for calibration targets
    dfx2$times <- paste0(dfx2$times, ",")
    dfx2$values <- paste0(dfx2$values, ",")
    write.csv(dfx2[,c("times", "values")], paste0("Output/", j, "_", names[i], "_long_", uploadDate, ".csv"), row.names = F)
  }
}