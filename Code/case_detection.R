# testing --------------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data
uploadDate <- "20200628"
testingfileName <- paste0("DOH COVID Data Drop_", uploadDate, "/DOH COVID Data Drop_ ", uploadDate, " - 07 Testing Aggregates.csv")
testing <- read.csv(testingfileName, head = T, stringsAsFactors = F)

testing$report_date <- as.Date(testing$report_date, "%Y-%m-%d")

# testing_regions <- testing %>% filter(facility_name == )

ggplot(data=testing, aes(x=report_date, y=pct_positive_cumulative, group=facility_name)) +
  geom_line()+
  geom_point() +
  # facet_wrap(~facility_name, scales="free") +
  theme_bw() +
  ylab("Percent positive") +
  xlab("Date")


plot(testingInfo_aggregate$Date, testingInfo_aggregate$total_tested, pch=16, xlab = "Date", ylab = "Tests (daily)", type='b', ylim=c(0,190000))
par(new=T)
plot(testingInfo_aggregate$Date, testingInfo_aggregate$total_positive, pch=16, axes=F, xlab="", ylab="", col='blue', type='b', ylim=c(0,190000))
legend("topleft", bty = 'n', legend=c("Total tested", "Total tested positive"), pch=c(16,16), text.col=c("black", "blue"), col=c("black", "blue"))

plot(testingInfo_aggregate$Date, testingInfo_aggregate$percent_positive, pch=16, xlab = "Date", ylab = "Percent positive tests (daily)", type='b')

x <- merge(caseInfo_aggregate, testingInfo_aggregate, by.x="DateRepConf", by.y="Date")
x$percPopTest <- x$total_tested/106700000
x$propSymp <- x$total_cases*x$percPopTest
plot(x$DateRepConf, x$propSymp, type='b')
