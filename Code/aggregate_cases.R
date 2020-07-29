# aggregate case data from Philippines DoH -----------------------

# load library
library(tidyverse)

# important dates
uploadDate <- "20200616"

startdate <- as.Date("2020-01-01", "%Y-%m-%d")

# new cases ------------------------------------------------------
casesfileName <- paste0("DOH COVID Data Drop_", uploadDate, "/DOH COVID Data Drop_ ", uploadDate, " - 04 Case Information.csv")
casesInfo <- read.csv(casesfileName, head = T) 

casesInfo$DateRepConf <- as.Date(casesInfo$DateRepConf, "%Y-%m-%d")

# caseInfo_aggregate <- casesInfo %>%
#   group_by(DateRepConf) %>%
#   summarise(total_cases = length(DateRepConf))
# 
# caseInfo_aggregate$cases_times <- difftime(caseInfo_aggregate$DateRepConf,  startdate, units = "days")
# calibration_cases <- data.frame("cases_times" = caseInfo_aggregate$cases_times, "cases_values" = caseInfo_aggregate$total_cases)
# calibration_cases <- calibration_cases[complete.cases(calibration_cases),]
# write.csv(calibration_cases, "Output/calibration_cases.csv", row.names=F)

# cases based on date symptom onset -----------------------------
casesInfo$DateOnset <- as.Date(casesInfo$DateOnset, "%Y-%m-%d")

notifications_aggregate <- casesInfo %>%
  group_by(DateOnset) %>%
  summarise(notifications_values = length(DateOnset))

notifications_aggregate$cases_times <- difftime(notifications_aggregate$DateOnset,  startdate, units = "days")
notifications_cases <- data.frame("notifications_times" = notifications_aggregate$cases_times, "notifications_values" = notifications_aggregate$notifications_values)
notifications_cases <- notifications_cases[complete.cases(notifications_cases),]
notifications_cases <- subset(notifications_cases, notifications_times > 40)
notifications_times <- paste(notifications_cases$notifications_times, collapse=", ")
notifications_values <- paste(notifications_cases$notifications_values, collapse=", ")
notifications_values2 <- paste("[", notifications_cases$notifications_values, "]", collapse=",")
notifications_values2 <- gsub(" ", "", notifications_values2)
notifications <- data.frame(notifications_times, notifications_values, notifications_values2)
write.csv(notifications, "Output/calibration_cases_by_symptom_onset.csv", row.names=F)

# deaths --------------------------------------------------------
casesInfo$DateDied <- as.Date(casesInfo$DateDied, "%Y-%m-%d")

deaths_aggregate <- casesInfo %>%
  group_by(DateDied) %>%
  summarise(total_deaths = length(DateDied)) %>%
  filter(!is.na(DateDied))

deaths_aggregate$deaths_times <- difftime(deaths_aggregate$DateDied,  startdate, units = "days")
calibration_deaths <- data.frame("deaths_times" = deaths_aggregate$deaths_times, "deaths_values" = deaths_aggregate$total_deaths)
calibration_deaths <- calibration_deaths[complete.cases(calibration_deaths),]
calibration_deaths <- subset(calibration_deaths, deaths_times > 40)
deaths_times <- paste(calibration_deaths$deaths_times, collapse=", ")
deaths_values <- paste(calibration_deaths$deaths_values, collapse=", ")
deaths_values2 <- paste("[", calibration_deaths$deaths_values, "]", collapse=",")
deaths_values2 <- gsub(" ", "", deaths_values2)
deaths <- data.frame(deaths_times, deaths_values, deaths_values2)
write.csv(deaths, "Output/calibration_deaths.csv", row.names=F)

# testing --------------------------------------------------------
# testingfileName <- paste0("DOH COVID Data Drop_", uploadDate, "/DOH COVID Data Drop_ ", uploadDate, " - 07 Testing Aggregates.csv")
# testingInfo <- read.csv(testingfileName, head = T, stringsAsFactors = F)
# 
# testingInfo$Date <- as.Date(testingInfo$Date, "%B %d, %Y")
# 
# testingInfo$UNIQUE.INDIVIDUALS.TESTED <- as.numeric(gsub(",", "", testingInfo$UNIQUE.INDIVIDUALS.TESTED))
# testingInfo$POSITIVE.INDIVIDUALS <- as.numeric(gsub(",", "", testingInfo$POSITIVE.INDIVIDUALS))
# testingInfo$NEGATIVE.INDIVIDUALS <- as.numeric(gsub(",", "", testingInfo$NEGATIVE.INDIVIDUALS))
# 
# testingInfo_aggregate <- testingInfo %>%
#   group_by(Date) %>%
#   summarise(total_tested = sum(UNIQUE.INDIVIDUALS.TESTED),
#             total_positive = sum(POSITIVE.INDIVIDUALS),
#             total_negative = sum(NEGATIVE.INDIVIDUALS),
#             percent_positive = round(total_positive/total_tested, 2),
#             percent_negative = round(total_negative/total_tested, 2))
# 
# plot(testingInfo_aggregate$Date, testingInfo_aggregate$total_tested, pch=16, xlab = "Date", ylab = "Tests (daily)", type='b', ylim=c(0,190000))
# par(new=T)
# plot(testingInfo_aggregate$Date, testingInfo_aggregate$total_positive, pch=16, axes=F, xlab="", ylab="", col='blue', type='b', ylim=c(0,190000))
# legend("topleft", bty = 'n', legend=c("Total tested", "Total tested positive"), pch=c(16,16), text.col=c("black", "blue"), col=c("black", "blue"))
# 
# plot(testingInfo_aggregate$Date, testingInfo_aggregate$percent_positive, pch=16, xlab = "Date", ylab = "Percent positive tests (daily)", type='b')
# 
# x <- merge(caseInfo_aggregate, testingInfo_aggregate, by.x="DateRepConf", by.y="Date")
# x$percPopTest <- x$total_tested/106700000
# x$propSymp <- x$total_cases*x$percPopTest
# plot(x$DateRepConf, x$propSymp, type='b')

# ICU use -------------------------------------------------------
icufileName <- paste0("DOH COVID Data Drop_", uploadDate, "/DOH COVID Data Drop_ ", uploadDate, " - 05 DOH Data Collect - Daily Report.csv")
icuInfo <- read.csv(icufileName, head = T, stringsAsFactors = F)

icuInfo$reportdate <- as.Date(icuInfo$reportdate, "%Y-%m-%d")
icuInfo$icu_times <- difftime(icuInfo$reportdate,  startdate, units = "days")

icuInfo_aggregate <- icuInfo %>%
  group_by(icu_times) %>%
  summarise(icu_values = sum(icu_o))

icu_times <- paste(icuInfo_aggregate$icu_times, collapse=", ")
icu_values <- paste(icuInfo_aggregate$icu_values, collapse=", ")
icu_values2 <- paste("[", icuInfo_aggregate$icu_values, "]", collapse=",")
icu_values2 <- gsub(" ", "", icu_values2)
icu <- data.frame(icu_times, icu_values, icu_values2)
write.csv(icu, "Output/calibration_ICU_beds.csv", row.names=F)

# visualize results ---------------------------------------------
par(mfrow = c(3, 2))

plot(caseInfo_aggregate$DateRepConf, caseInfo_aggregate$total_cases, pch=16, xlab = "Report date", ylab = "New cases (daily total)", type='b', main="Cases")

plot(deaths_aggregate$DateDied, deaths_aggregate$total_deaths, pch=16, xlab = "Report date", ylab = "Total deaths (daily total)", type='b', main="Deaths")

plot(testingInfo_aggregate$Date, testingInfo_aggregate$total_tested, pch=16, xlab = "Date", ylab = "Tests (daily)", type='b', ylim=c(0,190000), main="Tested")
par(new=T)
plot(testingInfo_aggregate$Date, testingInfo_aggregate$total_positive, pch=16, axes=F, xlab="", ylab="", col='blue', type='b', ylim=c(0,190000))
legend("topleft", bty = 'n', legend=c("Total tested", "Total tested positive"), pch=c(16,16), text.col=c("black", "blue"), col=c("black", "blue"))

plot(testingInfo_aggregate$Date, testingInfo_aggregate$percent_positive, pch=16, xlab = "Date", ylab = "Percent positive tests (daily)", type='b', main="Percent positive tests")

plot(icuInfo_aggregate$reportdate, icuInfo_aggregate$icu_beds_occupied, pch=16, type='b', xlab = "Report date", ylab = "ICU beds occupied", main="ICU beds used")

plot(icuInfo_aggregate$reportdate, icuInfo_aggregate$percent_icu_beds_occupied, pch=16, xlab = "Date", ylab = "Percent ICU beds occupied", type='b', main="Percent of total ICU beds used")

plot(symptomOnset_aggregate$cases_times, symptomOnset_aggregate$total_cases, type='b', ylim=c(0,250), pch=16, xlab='Days since Jan 1 to sympotom onset', ylab='total cases')

