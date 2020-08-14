library("dplyr")
library("dbplyr")

# list most powerbi databases and order by date of download
dbfiles <- file.info(list.files("powerbi_derived_ouputs/", full.names = T))
dbfiles$filename <- rownames(dbfiles)
dbfiles <- dbfiles[order(dbfiles$mtime),]

# load powerbi files and extract derived outputs
src_dbi(tmp_powerbi)
most_recent_db_files_indexes <- seq(nrow(dbfiles)-3, nrow(dbfiles), 1)
powerbi_df <- data.frame()
  
for(i in most_recent_db_files_indexes){
  tmp_filename <- dbfiles$filename[i]
  # derived outputs
  tmp_powerbi <- DBI::dbConnect(RSQLite::SQLite(), tmp_filename)
  tmp_derived_outputs <- tbl(tmp_powerbi, "derived_outputs")
  tmp_derived_outputs <- tmp_derived_outputs %>%
    select(Scenario, times, notifications_at_sympt_onset, icu_occupancy, total_infection_deaths)
  tmp_derived_outputs <- data.frame(tmp_derived_outputs)
  # uncertainty
  tmp_uncertainty <- tbl(tmp_powerbi, "uncertainty")
  tmp_uncertainty <- data.frame(tmp_uncertainty)
  colnames(tmp_uncertainty)[3] <- "times" 
  tmp_uncertainty$level <- paste0(tmp_uncertainty$type, "_Q", tmp_uncertainty$quantile) 
  tmp_uncertainty <- spread(tmp_uncertainty[,c("Scenario", "times", "level", "value")], key = level, value = value)
  tmp_uncertainty <- data.frame(tmp_uncertainty)
  # add region name, combine, and create long dataframe
  tmp_df <- merge(tmp_derived_outputs, tmp_uncertainty, by = c("Scenario", "times"), all.x = T)
  tmp_df$Region <- substr(tmp_filename, 32, (nchar(tmp_filename)-22))
  powerbi_df <- rbind(powerbi_df, tmp_df)
}

# load calibration data
uploadDate <- "2020-08-08"
regions <- c("philippines", "manila", "calabarzon", "central-visayas")
local_data_types <- c("notifications", "icu", "deaths")
local_data_df <- data.frame()

for(j in 1:length(regions)){
  local_data_tmp_df <- data.frame(matrix(ncol=1, nrow=0))
  colnames(local_data_tmp_df) <- "times"
  for(k in 1:length(local_data_types)){
    tmp_local_data <- read.csv(paste0("Output/", uploadDate, "_", regions[j], "_", local_data_types[k], "_long.csv"), head = T)
    colnames(tmp_local_data)[2] <- paste0("local_data_", local_data_types[k])
    local_data_tmp_df <- merge(local_data_tmp_df, tmp_local_data, by = "times", all = T)
  }
  local_data_tmp_df <- lapply(local_data_tmp_df, gsub, pattern = ",", replacement = "")
  local_data_tmp_df <- lapply(local_data_tmp_df, as.numeric)
  local_data_tmp_df <- as.data.frame(local_data_tmp_df)
  local_data_tmp_df$Region <- regions[j]
  local_data_tmp_df$data_type <- "calibration" 
  local_data_df <- rbind(local_data_df, local_data_tmp_df)
}

# merge powerbi data with local calibration data
x <- merge(powerbi_df, local_data_df, by = c("Region", "times"), all.x = T)

# plot
# x$Date <- create dates from times? Then plot against dates?
stop_plot_date <- as.Date(uploadDate, "%Y-%m-%d") + 49 # 6 weeks after upload date
stop_plot_time <- as.numeric(difftime(stop_plot_date, as.Date("2019-12-31", "%Y-%m-%d")))

x2 <- subset(x, times <= stop_plot_time)
x3 <- subset(x2, Region == "calabarzon" & Scenario == "S_0")

# is milinda plotting notifications_at_sympt_onset or 50%ile for comparison with local data??
# plot(x3$times, x3$notifications_at_sympt_onset, type = 'l', ylim=c(0,5000))
# points(x3$times, x3$local_data_notifications, pch = 16, col = 'blue')
plot(x3$times, x3$notifications_Q0.5, type = 'l', ylim=c(0,5000))
lines(x3$times, x3$notifications_Q0.25, type = 'l', lty = 2)
lines(x3$times, x3$notifications_Q0.75, type = 'l', lty = 2)
points(x3$times, x3$local_data_notifications, pch = 16, col = 'blue')
summary(lm(x3$local_data_notifications~x3$notifications_Q0.5))

end_calibration <- which(x$times == max(!is.na(x$notifications_at_sympt_onset)))


ggplot(data = x2, aes(x = times, y = notifications_at_sympt_onset, group = Scenario, col = Scenario)) +
  geom_line() +
  geom_point(data = x2, aes(x = times, y = local_data_notifications, group = Scenario)) +
  facet_grid(~Region) +
  theme_bw() 

plot(x$times, x$notifications_at_sympt_onset, type = 'l')
points(x$times, x$local_data_deaths, col = 'red', pch = 16)

# these aren't really correct, because they are the newly 7 day average values
fileName_caldata <- paste0("powerbi_derived_ouputs/", runDate, "_", site, "_notifications.csv")
calValData <- read.csv(fileName_caldata, head = T, stringsAsFactors = F)  

x <- merge(doutputs, calValData, by = "times", all = T)
plot(x$values, x$notifications_at_sympt_onset, pch = 16, xlab = "Confirmed cases", ylab = "Predicted notifications")
abline(0,1)

plot(x$times, x$notifications_at_sympt_onset)
points(x$times, x$values, col = "red")

# overprediction
plot(x$times, (x$notifications_at_sympt_onset)-x$values, type = 'b', pch = 16)
