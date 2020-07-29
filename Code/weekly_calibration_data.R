# Philippines weekly calibration data aggregation -------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(tidyverse)
library(tidyquant)
library(ggplot2)

# load data
uploadDate <- "2020-07-28"
fileName <- paste0("FASSSTER_Data/ConfirmedCases_Final_", uploadDate, ".csv")
data <- read.csv(fileName, head = T, stringsAsFactors = F) 

# format dates
data$imputed_Date_Admitted <- as.Date(data$imputed_Date_Admitted, "%Y-%m-%d")
data$Date_Died <- as.Date(data$Date_Died, "%Y-%m-%d")
startdate <- as.Date("2019-12-31", "%Y-%m-%d")

# format region names
data$Region[data$Region == "4A"] <- "CAL"
data$Region[data$Region == "07"] <- "CEN-VIS"

# duplicate data and combine with original dataset to calculate national and regional estimates simultaneously
dup_data <- data
dup_data$Region <- "PHL"
data2 <- rbind(data, dup_data)

# aggregate notification data to weekly values 
notifications <- data2 %>%
  rename(Date = imputed_Date_Admitted) %>%
  filter(Date <= as.Date(uploadDate, "%Y-%m-%d")) %>%
  filter(Region == "PHL" | Region == "NCR" | Region == "CAL" | Region == "CEN-VIS") %>% 
  group_by(Region, Date) %>%
  summarise(values = length(Date)) %>%
  tq_transmute(select     = values,
               mutate_fun = apply.weekly,
               FUN        = mean) 

# aggregate death data to weekly values 
deaths <- data2 %>%
  rename(Date = Date_Died) %>%
  filter(Date <= as.Date(uploadDate, "%Y-%m-%d")) %>%
  filter(Region == "PHL" | Region == "NCR" | Region == "CAL" | Region == "CEN-VIS") %>% 
  group_by(Region, Date) %>%
  summarise(values = length(Date)) %>%
  tq_transmute(select     = values,
               mutate_fun = apply.weekly,
               FUN        = mean) 

# plot data
notification_fig <- ggplot(data=notifications, aes(x=Date, y=values, group=Region)) +
  geom_line()+
  geom_point() +
  facet_wrap(~Region, scales="free") +
  theme_bw() +
  ylab("Mean notifications per week") +
  xlab("Date")

ggsave(paste0("Figures/notifications_by_region_", uploadDate, ".tiff"))

death_fig <- ggplot(data=deaths, aes(x=Date, y=values, group=Region)) +
  geom_line()+
  geom_point() +
  facet_wrap(~Region, scales="free") +
  theme_bw() +
  ylab("Mean deaths per week") +
  xlab("Date")

ggsave(paste0("Figures/deaths_by_region_", uploadDate, ".tiff"))

# format and save data
dfs <- list(notifications, deaths)
names <- c("notifications", "deaths")
regions <- unique(notifications$Region)

for(i in 1:length(dfs)){
  dfx <- dfs[[i]]
  dfx$values <- round(dfx$values)
  dfx$times <- as.numeric(difftime(dfx$Date, startdate, units = "days"))
  for(j in regions){
    dfx2 <- subset(dfx, Region == j)
    dfx2 <- subset(dfx2, times > 40 & times < max(times))
    # format and save data for plotting
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