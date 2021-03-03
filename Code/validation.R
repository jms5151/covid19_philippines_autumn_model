library("dplyr")
library("dbplyr")
library("tidyverse")

# Current model output -------------------------------------------------------------------------------
# list most powerbi databases and order by date of download
dbfiles <- file.info(list.files("powerbi_derived_ouputs/", full.names = T))
dbfiles$filename <- rownames(dbfiles)
dbfiles <- dbfiles[order(dbfiles$mtime),]

# load powerbi files and extract derived outputs
# src_dbi(tmp_powerbi)
most_recent_db_files_indexes <- seq(nrow(dbfiles)-3, nrow(dbfiles), 1)
powerbi_df <- data.frame()
  
for(i in most_recent_db_files_indexes){
  tmp_filename <- dbfiles$filename[i]
  tmp_powerbi <- DBI::dbConnect(RSQLite::SQLite(), tmp_filename)
  # uncertainty outputs
  tmp_uncertainty <- tbl(tmp_powerbi, "uncertainty")
  tmp_uncertainty <- data.frame(tmp_uncertainty)
  colnames(tmp_uncertainty)[3] <- "times" 
  # filter and format outputs
  tmp_uncertainty <- subset(tmp_uncertainty, quantile == 0.25 | quantile == 0.5 | quantile == 0.75)
  tmp_uncertainty <- subset(tmp_uncertainty, type != "incidence")
  tmp_uncertainty$level <- paste0(tmp_uncertainty$type, "_Q", tmp_uncertainty$quantile) 
  tmp_uncertainty <- spread(tmp_uncertainty[,c("Scenario", "times", "level", "value")], key = level, value = value)
  tmp_uncertainty <- data.frame(tmp_uncertainty)
  # add region name and create long data frame
  tmp_uncertainty$Region <- substr(tmp_filename, 32, (nchar(tmp_filename)-22))
  powerbi_df <- rbind(powerbi_df, tmp_uncertainty)
}

# load calibration data
uploadDate <- "2020-08-22"
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

# merge powerbi data with local calibration data, format, and save
x <- merge(powerbi_df, local_data_df, by = c("Region", "times"), all.x = T)
x$Model_run_date <- uploadDate
x$Data_aquired_date <- uploadDate
x$Date <- as.Date(x$times, origin = '2019-12-31') #create dates from times
write.csv(x, paste0("validation/", uploadDate, "_model_run.csv"), row.names = F)

# plot current and all former model runs -----------------------------------------------------
# read in all prior model runs
model_runs <- list.files("validation/")
model_run_dates <- c()
model_run_dfs <- list()

for(k in 1:length(model_runs)){
  x <- read.csv(paste0("validation/", model_runs[k]), head = T, stringsAsFactors = F)
  model_run_name <- substr(model_runs[k], 1, 20)
  model_run_dates <- c(model_run_dates, as.character(substr(model_runs[k], 1, 10)))
  x$Date < as.Date(x$Date, "%Y-%m-%d")
  model_run_dfs[[k]] <- x
  assign(model_run_name, x)
}  

scenariox <- "S_0"
yvars_local <- c("notifications", "icu", "deaths")
yvars_derived <- c("notifications", "prevXlate_activeXclinical_icuXamong", "infection_deathsXall")
yvars_NiceNames <- c("Notifications", "ICU occupancy", "Deaths")
regions <- c("philippines", "manila", "calabarzon", "central-visayas")
regionNiceNames <- c("Philippines", "Metro Manila", "Calabarzon", "Central Visayas")

# par(mfrow = c(4, 4), mar = c(2, 2, 2, 0.5), oma=c(0, 4, 8, 2))
# par(mfrow = c(3, 4), mar = c(2, 2, 2, 0.5), oma=c(2, 4, 8, 2))

for(l in 1:length(model_run_dates)){
  x <- model_run_dfs[[l]]
  if(uploadDate != model_run_dates[l]){
    new_data <- model_run_dfs[[which((model_run_dates) == max(model_run_dates))]]
    new_data2 <- new_data[,c("Region", "Date", "Scenario", "local_data_notifications", "local_data_icu", "local_data_deaths")]
    colnames(new_data2) <- c("Region", "Date", "Scenario", "new_notifications", "new_icu", "new_deaths")
    x$Data_aquired_date[x$Date >= x$Date[max(which(!is.na(x$local_data_notifications)))]] <- uploadDate
    x <- merge(x, new_data2, by = c("Region", "Date", "Scenario"), all = T)
    x$local_data_notifications <- ifelse(is.na(x$local_data_notifications) & x$Data_aquired_date == uploadDate, x$new_notifications, x$local_data_notifications)
    x$local_data_icu <- ifelse(is.na(x$local_data_icu) & x$Data_aquired_date == uploadDate, x$new_icu, x$local_data_icu)
    x$local_data_deaths <- ifelse(is.na(x$local_data_deaths) & x$Data_aquired_date == uploadDate, x$new_deaths, x$local_data_deaths)
  }
  x$Date <- as.Date(x$Date, "%Y-%m-%d")
  x$Model_run_date <- as.Date(x$Model_run_date, "%Y-%m-%d")
  x <- subset(x, Scenario == scenariox)
  pdf(paste0("Figures/validation_plot_", model_run_dates[l], ".pdf"))
  par(mfrow = c(3, 4), mar = c(2, 2, 2, 0.5), oma=c(2, 4, 8, 2))
  for(y in 1:length(yvars_local)){
    for(rg in 1:length(regions)){
      # identify column names
      localData <- paste0("local_data_", yvars_local[y])
      Q25 <- paste0(yvars_derived[y], "_Q0.25")
      Q50 <- paste0(yvars_derived[y], "_Q0.5")
      Q75 <- paste0(yvars_derived[y], "_Q0.75")
      # subset data appropriately
      stop_plot_date <- unique(x$Model_run_date) + 49 # 6 weeks after upload date
      x2 <- subset(x, Date <= stop_plot_date)
      x3 <- subset(x2, Region == regions[rg])
      if(uploadDate == model_run_dates[l]){
        end_calibration <- x3$Date[max(which(!is.na(x3[,localData])))]
        x3$model <- ifelse(x3$Date <= end_calibration, "calibrated", "uncalibrated")
      } else {
        x3$model <- ifelse(x3$Model_run_date == x3$Data_aquired_date, "calibrated", "uncalibrated")
      } 
      # set plot limits
      ymax <- max(x3[Q75]) + 100
      xmax <- max(x3$Date)
      xmin <- as.Date("2020-01-01", "%Y-%m-%d")
      # plot
      plot(x3$Date[x3$model == "calibrated"], x3[x3$model == "calibrated", Q50], type = 'l', xlim = c(xmin, xmax), ylim = c(0, ymax), ylab = "", xlab = "")
      lines(x3$Date[x3$model == "calibrated"], x3[x3$model == "calibrated", Q25], type = 'l', lty = 2, xlim = c(xmin, xmax), ylim = c(0, ymax))
      lines(x3$Date[x3$model == "calibrated"], x3[x3$model == "calibrated", Q75], type = 'l', lty = 2, xlim = c(xmin, xmax), ylim = c(0, ymax))
      points(x3$Date[x3$model == "calibrated"], x3[x3$model == "calibrated", localData], pch = 16, xlim = c(xmin, xmax), ylim = c(0, ymax))
      lines(x3$Date[x3$model == "uncalibrated"], x3[x3$model == "uncalibrated", Q50], type = 'l', xlim = c(xmin, xmax), ylim = c(0, ymax), col = "blue")
      lines(x3$Date[x3$model == "uncalibrated"], x3[x3$model == "uncalibrated", Q25], type = 'l', lty = 2, xlim = c(xmin, xmax), ylim = c(0, ymax), col = "blue")
      lines(x3$Date[x3$model == "uncalibrated"], x3[x3$model == "uncalibrated", Q75], type = 'l', lty = 2, xlim = c(xmin, xmax), ylim = c(0, ymax), col = "blue")
      points(x3$Date[x3$model == "uncalibrated"], x3[x3$model == "uncalibrated", localData], pch = 16, xlim = c(xmin, xmax), ylim = c(0, ymax), col = "blue")
      polygon(x = c(x3$Date[x3$model == "calibrated"], rev(x3$Date[x3$model == "calibrated"])),
              y = c(x3[x3$model == "calibrated", Q25], rev(x3[x3$model == "calibrated", Q75])),
              col =  adjustcolor("steelblue", alpha.f = 0.10), border = NA)
      polygon(x = c(x3$Date[x3$model == "uncalibrated"], rev(x3$Date[x3$model == "uncalibrated"])),
              y = c(x3[x3$model == "uncalibrated", Q25], rev(x3[x3$model == "uncalibrated", Q75])),
              col =  adjustcolor("dodgerblue", alpha.f = 0.10), border = NA)
      r2 <- summary(lm(x3[,localData]~x3[,Q50]))$adj.r.squared
      legend("topleft", legend =  bquote(italic(R)^2 == .(format(r2, digits = 2))), bty = 'n', cex= 1.2)
      if(yvars_local[y] == "notifications"){
        title(regionNiceNames[rg], line = 1)
      }
      if(regions[rg] == "philippines"){
        mtext(yvars_NiceNames[y], side = 2, line = 3)
      }
    }
  }
  # add major text
  mtext("AuTuMN Covid-19 model", side = 3, line = 5, adj = 0, outer = T, cex = 1.5)
  mtext(paste0("Run date: ", model_run_dates[l]), side = 3, line = 3, adj = 0, outer = T, cex = 1.1)
  dev.off()
}

# # calculate R2 by week
# test <- x
# test$week <- as.numeric(round(difftime(test$Date, test$Model_run_date, units = "weeks")))
# test$week[test$week <= 0] <- 0
# test <- subset(test, Region == "philippines")
# test2 <- sapply(split(test[,c("local_data_notifications", "notifications_Q0.5")], test$week), function(x) summary(lm(x))$r.sq)
# 
# # radar plot
# library(fmsb)
# 
# for(rg in regions){
#   x3 <- subset(x2, Region == rg & Scenario == scenariox)
#   data <- data.frame("Notifications" = round(summary(lm(x3$local_data_notifications~x3[,Q50]))$adj.r.squared, 2),
#                      "ICU" = round(summary(lm(x3$local_data_icu~x3$prevXlate_activeXclinical_icuXamong_Q0.5))$adj.r.squared, 2),
#                      "Deaths" = round(summary(lm(x3$local_data_deaths~x3$infection_deathsXall_Q0.5))$adj.r.squared, 2))
#   data <- rbind(data.frame("Notifications" = c(1, 0), "ICU" = c(1, 0), "Deaths" = c(1, 0)), data)
#   radarchart(data,  cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8)
# }