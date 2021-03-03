# create figures
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(gridExtra)
library(dbplyr)
library(truncnorm)
library(zoo)

autumn_base_dir <- "C:/Users/Jamie/code/autumn/data/inputs/"
figures_dir <- "C:/Users/Jamie/Box/Projects/2021_Caldwell_etal_WHO_Bulletin/WHO_Bulletin/Figures/"

# Fig 1C -------------------------------------------------------------------------------------
agestr <- read_excel(paste0(autumn_base_dir, "world-population/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx"), 
                     sheet = "ESTIMATES",
                     skip = 16)

agestr[,c(9:29)] <- lapply(agestr[,c(9:29)], as.numeric)

pop_str_plot_fun <- function(df, labelsOn, titleName, textSize){
  df['75+'] = rowSums(df[, c("75-79", "80-84", "85-89", "90-94", "95-99", "100+")]) 
  df <- df[,c("0-4", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+")]
  df <- gather(df, key = "Age_group", value = "PopSize")
  p <- ggplot(data=df, aes(x=PopSize/1000, y=Age_group)) +
    geom_bar(stat="identity") +
    theme_classic() +
    theme(text = element_text(size = textSize)) +
    ylab("") +
    xlab("Population size (millions)") +
    ggtitle(titleName)
  if(labelsOn == TRUE){
    p <- p + ylab("Age group\n")
  }
  p
}

pop_phl <- subset(agestr, agestr[,'Region, subregion, country or area *'] == "Philippines" & agestr['Reference date (as of 1 July)'] == 2020)
natPopPlot <- pop_str_plot_fun(pop_phl, labelsOn = TRUE, titleName = "", textSize = 8)
ggsave(file = paste0(figures_dir, 'pop_str_national.pdf'), natPopPlot, width = 4.79, height = 3.55)

# subregions
agestr_subregions <- read.csv(paste0(autumn_base_dir, "world-population/subregions.csv"))
colnames(agestr_subregions) <- gsub("X", "", colnames(agestr_subregions))
colnames(agestr_subregions) <- gsub("\\.", "-", colnames(agestr_subregions))
colnames(agestr_subregions)[25] <- "100+"

pop_ncr <- subset(agestr_subregions, region == "Metro Manila")
ncrPopPlot <- pop_str_plot_fun(pop_ncr, labelsOn = TRUE, titleName = "National Capital Region", textSize = 8)

pop_cal <- subset(agestr_subregions, region == "Calabarzon")
calPopPlot <- pop_str_plot_fun(pop_cal, labelsOn = FALSE, titleName = "Calabarzon", textSize = 8)

pop_vis <- subset(agestr_subregions, region == "Central Visayas")
visPopPlot <- pop_str_plot_fun(pop_vis, labelsOn = FALSE, titleName = "Central Visayas", textSize = 8)

# Fig 1E -------------------------------------------------------------------------------------
mobility <- read.csv(paste0(autumn_base_dir, 'mobility/Google_Mobility_Report.csv'))
mobility$date <- as.Date(mobility$date, "%Y-%m-%d")
mobility$Other <- rowMeans(mobility[,c("retail_and_recreation_percent_change_from_baseline",
                                      "grocery_and_pharmacy_percent_change_from_baseline",
                                      "parks_percent_change_from_baseline",
                                      "transit_stations_percent_change_from_baseline")])

mobility_plot_fun <- function(df, labelsOn, legendOn, textSize){
  x <- gather(df, key = Location, value = values, workplaces_percent_change_from_baseline:Other)
  p <- ggplot(data = x, aes(x=date, y = (values+100)/100, col = Location)) +
    geom_line() +
    scale_color_manual(values=c("black", "#E69F00", "#56B4E9"),
                     breaks=c("residential_percent_change_from_baseline", "workplaces_percent_change_from_baseline", "Other"),
                     labels=c("Home", "Work", "Other locations")) +
    theme_classic() +
    scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
    xlab('') +
    ylab('') +
    theme(legend.position = 'none',
          text = element_text(size = textSize)
          ) +
    ggtitle("")
  if(labelsOn == TRUE){
    p <- p + ylab('Change in mobility from baseline')
  } 
  if(legendOn == TRUE){
    p <- p +
      theme(
        legend.position = c(.95, .02),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        legend.background = element_blank()
      )
  }
  p
}
  
# mobility plots
nat_mobility <- subset(mobility, country_region == 'Philippines' & metro_area == "" & sub_region_1 == "")
natMobMap <- mobility_plot_fun(nat_mobility,  labelsOn = TRUE, legendOn = TRUE, textSize = 8)
ggsave(file = paste0(figures_dir, 'mobility_national.pdf'), natMobMap, width = 4.79, height = 3.55)

cal_mobility <- subset(mobility, country_region == 'Philippines' & sub_region_1 == "Calabarzon")
calMobMap <- mobility_plot_fun(cal_mobility, labelsOn = TRUE, legendOn = TRUE, textSize = 8)

vis_mobility <- subset(mobility, country_region == 'Philippines' & sub_region_1 == "Central Visayas")
visMobMap <- mobility_plot_fun(vis_mobility, labelsOn = FALSE, legendOn = FALSE, textSize = 8)

manila_mobility <- subset(mobility, country_region == 'Philippines' & sub_region_1 == "National Capital Region")
ncrMobMap <- mobility_plot_fun(manila_mobility, labelsOn = FALSE, legendOn = FALSE, textSize = 8)

# Fig. S1
FigS1 <- grid.arrange(calPopPlot, visPopPlot, ncrPopPlot, calMobMap, visMobMap, ncrMobMap, ncol = 3, nrow = 2)
ggsave(file = paste0(figures_dir, 'Fig_S1.pdf'), FigS1, width = 7.85, height = 4.91)
# ggsave(file = paste0(figures_dir, 'Fig_S1.jpg'), FigS1, width = 7.85, height = 4.91)

# Figures with powerbi --------------------------------------------------------------------------
# list most powerbi databases and order by date of download
dbfiles <- file.info(list.files("powerbi_derived_ouputs/", full.names = T))
dbfiles$filename <- rownames(dbfiles)
dbfiles <- dbfiles[order(dbfiles$mtime),]

# load powerbi files and extract derived outputs
# src_dbi(tmp_powerbi)
most_recent_db_files_indexes <- seq(nrow(dbfiles)-3, nrow(dbfiles), 1)
powerbi_uncertainty <- data.frame()
powerbi_derived_outputs <- data.frame()
powerbi_mcmc <- data.frame()

for(i in most_recent_db_files_indexes){
  tmp_filename <- dbfiles$filename[i]
  tmp_powerbi <- DBI::dbConnect(RSQLite::SQLite(), tmp_filename)
  # pre-calculated uncertainty outputs
  tmp_uncertainty <- tbl(tmp_powerbi, "uncertainty")
  tmp_uncertainty <- data.frame(tmp_uncertainty)
  tmp_uncertainty$Region <- substr(tmp_filename, 32, (nchar(tmp_filename)-22))
  # derived outputs
  tmp_derived_outputs <- tbl(tmp_powerbi, "derived_outputs")
  tmp_derived_outputs <- data.frame(tmp_derived_outputs)
  tmp_derived_outputs$Region <- substr(tmp_filename, 32, (nchar(tmp_filename)-22))
  # mcmc results
  tmp_mcmc <- tbl(tmp_powerbi, "mcmc_params")
  tmp_mcmc <- data.frame(tmp_mcmc)
  tmp_mcmc$Region <- substr(tmp_filename, 32, (nchar(tmp_filename)-22))
  # bind data frame
  powerbi_uncertainty <- rbind(powerbi_uncertainty, tmp_uncertainty)
  powerbi_derived_outputs <- rbind(powerbi_derived_outputs, tmp_derived_outputs)
  powerbi_mcmc <- rbind(powerbi_mcmc, tmp_mcmc)
  DBI::dbDisconnect(tmp_powerbi)
}

powerbi_uncertainty$Region[powerbi_uncertainty$Region == "covid_19-philippines"] <- "Philippines"
powerbi_uncertainty$Region[powerbi_uncertainty$Region == "covid_19-calabarzon"] <- "Calabarzon"
powerbi_uncertainty$Region[powerbi_uncertainty$Region == "covid_19-central-visayas"] <- "Central Visayas"
powerbi_uncertainty$Region[powerbi_uncertainty$Region == "covid_19-manila"] <- "National Capital Region"
powerbi_uncertainty$Date <- as.Date("2019-12-31", "%Y-%m-%d") + powerbi_uncertainty$time
colnames(powerbi_uncertainty)[3] <- "times"

powerbi_mcmc$Region[powerbi_mcmc$Region == "covid_19-philippines"] <- "Philippines"
powerbi_mcmc$Region[powerbi_mcmc$Region == "covid_19-calabarzon"] <- "Calabarzon"
powerbi_mcmc$Region[powerbi_mcmc$Region == "covid_19-central-visayas"] <- "Central Visayas"
powerbi_mcmc$Region[powerbi_mcmc$Region == "covid_19-manila"] <- "National Capital Region"

# powerbi_derived_outputs$Region[powerbi_derived_outputs$Region == "covid_19-philippines"] <- "Philippines"
# powerbi_derived_outputs$Region[powerbi_derived_outputs$Region == "covid_19-calabarzon"] <- "Calabarzon"
# powerbi_derived_outputs$Region[powerbi_derived_outputs$Region == "covid_19-central-visayas"] <- "Central Visayas"
# powerbi_derived_outputs$Region[powerbi_derived_outputs$Region == "covid_19-manila"] <- "National Capital Region"

# Figure 2 ------------------------------------------------------------------------
# need to save local data from processing in python
confcases <- read.csv("data/PHL_notifications_processed.csv", stringsAsFactors = F) 
confcases$Region[confcases$Region == "philippines"] <- "Philippines"
confcases$Region[confcases$Region == "calabarzon"] <- "Calabarzon"
confcases$Region[confcases$Region == "central_visayas"] <- "Central Visayas"
confcases$Region[confcases$Region == "manila"] <- "National Capital Region"

# mhs function
mhs <- read.csv("data/MD_function_values.csv")
mhs$date <- as.Date("2019-12-31", "%Y-%m-%d") + mhs$times
mhs <- subset(mhs, times <= 314)
mhs2 <- data.frame("inverse_mhs_values" = mhs$inverse_mhs_values[nrow(mhs)], 
                   "date" = seq.Date(max(mhs$date)+1, max(powerbi_uncertainty$Date), by = "1 day"))

counterfactual_comparison <- function(region, filename){
  confcases_phl <- subset(confcases, Region == region)
  # df with MHS
  notificationsPHL_withMHS <- subset(powerbi_uncertainty, Region == region & scenario == 0 & type == "notifications")
  notificationsPHL_withMHS <- spread(notificationsPHL_withMHS, key = quantile, value = value)
  colnames(notificationsPHL_withMHS)[6:10] <- paste0("X", colnames(notificationsPHL_withMHS)[6:10])
  notificationsPHL_withMHS <- merge(notificationsPHL_withMHS, confcases_phl, by = c("Region", "times"), all.x = T)
  # df without MHS
  notificationsPHL_withoutMHS <- subset(powerbi_uncertainty, Region == region & scenario == 1 & type == "notifications")
  notificationsPHL_withoutMHS <- spread(notificationsPHL_withoutMHS, key = quantile, value = value)
  colnames(notificationsPHL_withoutMHS)[6:10] <- paste0("X", colnames(notificationsPHL_withoutMHS)[6:10])
  notificationsPHL_withoutMHS <- merge(notificationsPHL_withoutMHS, confcases_phl, by = c("Region", "times"), all.x = T)
  
  maxY_withMHS <- max(notificationsPHL_withMHS$X0.975)*2

  # plot
  plotwithMHS <- ggplot(notificationsPHL_withMHS, aes(x = Date, y = X0.5)) +
    geom_ribbon(aes(ymin = X0.025, ymax = X0.975, x = Date), alpha = 0.3, fill="#3366FF") +
    geom_ribbon(aes(ymin = X0.25, ymax = X0.75, x = Date), alpha = 0.5, fill="#3366FF") +
    geom_line(size = 1, col = 'darkblue') +
    geom_line(data = mhs, aes(x = date, y = (1-inverse_mhs_values)*maxY_withMHS), size = 1, col = "darkred") +
    geom_line(data = mhs2, aes(x = date, y = (1-inverse_mhs_values)*maxY_withMHS), linetype = "dashed", size = 1, col = "darkred") +
    scale_y_continuous(sec.axis = sec_axis(~ . /maxY_withMHS, name = "MHS scaled function\n")) +
    geom_point(aes(x = Date, y = mean_daily_notifications), fill = "black", size = 1.1) +
    ylab("Cases\n") +
    xlab("") +
    scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
    theme_bw() +
    theme(
      strip.background = element_rect(
        color="white", fill="white"
      ),
      plot.title = element_text(hjust = 0.5)
    ) +
    ggtitle("MHS incorporated") +
    theme(text = element_text(size = 8))
  
  plotwithoutMHS <- ggplot(notificationsPHL_withoutMHS, aes(x = Date, y = X0.5)) +
    geom_ribbon(aes(ymin = X0.025, ymax = X0.975, x = Date), alpha = 0.3, fill="#3366FF") +
    geom_ribbon(aes(ymin = X0.25, ymax = X0.75, x = Date), alpha = 0.5, fill="#3366FF") +
    geom_point(aes(x = Date, y = mean_daily_notifications), fill = "black", size = 1.1) +
    geom_line(size = 1, col = 'darkblue') +
    ylab("Cases\n") +
    xlab("") +
    scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
    theme_bw() +
    theme(
      strip.background = element_rect(
        color="white", fill="white"
      ),
      plot.title = element_text(hjust = 0.5)
    ) +
    ggtitle("MHS not incorporated") + 
    theme(text = element_text(size = 8))

  fig2 <- grid.arrange(plotwithMHS, plotwithoutMHS, ncol = 2, widths=c(1.1,1))
  ggsave(file = paste0(figures_dir, "/", filename, '.pdf'), fig2, width = 7.85, height = 3.49)
  ggsave(file = paste0(figures_dir, "/", filename, '.jpg'), fig2, width = 7.85, height = 3.49)
}

counterfactual_comparison("Philippines", "Fig_2_Philippines")
counterfactual_comparison("Calabarzon", "Fig_S2_calabarzon")
counterfactual_comparison("Central Visayas", "Fig_S3_central_visayas")
counterfactual_comparison("National Capital Region", "Fig_S4_ncr")

# Figure 3 ------------------------------------------------------------------------
confcases$Region[confcases$Region == "calabarzon"] <- "Calabarzon"
confcases$Region[confcases$Region == "central_visayas"] <- "Central Visayas"
confcases$Region[confcases$Region == "manila"] <- "National Capital Region"

notifications <- subset(powerbi_uncertainty, Region != "Philippines" & scenario == 0 & type == "notifications")
notifications <- spread(notifications, key = quantile, value = value)
colnames(notifications)[6:10] <- paste0("X", colnames(notifications)[6:10])

notifications_merged <- merge(notifications, confcases, by = c("Region", "times"), all.x = T)

fig3 <- ggplot(notifications_merged, aes(x = Date, y = X0.5)) +
  facet_wrap(~Region, scales="free_y", ncol = 1) +
  geom_ribbon(aes(ymin = X0.025, ymax = X0.975, x = Date), alpha = 0.3, fill="#3366FF") +
  geom_ribbon(aes(ymin = X0.25, ymax = X0.75, x = Date), alpha = 0.5, fill="#3366FF") +
  geom_line(size = 1, col = 'darkblue') +
  geom_point(aes(x = Date, y = mean_daily_notifications), fill = "black", size = 1.1) +
  ylab("Cases") +
  xlab("") +
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
  theme_bw() +
  theme(
    strip.background = element_rect(
      color="white", fill="white"
    )
  ) + 
  theme(text = element_text(size = 8))

ggsave(file = paste0(figures_dir, 'Fig_3.pdf'), fig3, width = 4.75, height = 5.98)
ggsave(file = paste0(figures_dir, 'Fig_3.jpg'), fig3, width = 4.75, height = 5.98)

# Figure 4 ---------------------------------------------------------
deaths <- read.csv("data/PHL_deaths_processed.csv", stringsAsFactors = F) 
deaths$type <- "accum_deaths"
colnames(deaths)[4] <- "Reported_data"
icu <- read.csv("data/PHL_icu_processed.csv", stringsAsFactors = F) 
icu$type <- "icu_occupancy"
colnames(icu)[2] <- "Region"
colnames(icu)[4] <- "Reported_data"

reportedData <- rbind(deaths, icu)
reportedData$Region[reportedData$Region == "calabarzon"] <- "Calabarzon"
reportedData$Region[reportedData$Region == "central_visayas"] <- "Central Visayas"
reportedData$Region[reportedData$Region == "manila"] <- "National Capital Region"
reportedData$Region[reportedData$Region == "philippines"] <- "Philippines"

powersub <- subset(powerbi_uncertainty, scenario == 0)
powersub <- subset(powersub, type == "accum_deaths" | type == "icu_occupancy" | type == "incidence" | type == "proportion_seropositive")
powersub_merged <- merge(powersub, reportedData, by = c("Region", "times", "type"), all = T)

powersub_merged <- spread(powersub_merged, key = quantile, value = value)
colnames(powersub_merged)[8:12] <- paste0("X", colnames(powersub_merged)[8:12])

powersub_merged$type[powersub_merged$type == "accum_deaths"] <- "Cumulative deaths"
powersub_merged$type[powersub_merged$type == "icu_occupancy"] <- "ICU occupancy"
powersub_merged$type[powersub_merged$type == "incidence"] <- "Incidence"
powersub_merged$type[powersub_merged$type == "proportion_seropositive"] <- "Percentage recovered"

derived_outputs_fun <- function(df, region, titleName, fileName){
  df <- subset(df, Region == region)
  ggplot(df, aes(x = Date, y = X0.5)) +
    facet_wrap(~type, scales="free_y", ncol = 2) +
    geom_ribbon(aes(ymin = X0.025, ymax = X0.975, x = Date), alpha = 0.3, fill="#3366FF") +
    geom_ribbon(aes(ymin = X0.25, ymax = X0.75, x = Date), alpha = 0.5, fill="#3366FF") +
    geom_line(size = 1, col = 'darkblue') +
    geom_point(aes(x = Date, y = Reported_data), fill = "black", size = 1.1) +
    ylab("") +
    xlab("") +
    ggtitle(titleName) +
    scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
    theme_bw() +
    theme(
      strip.background = element_rect(
        color="white", fill="white"
      )
    ) + 
    theme(text = element_text(size = 8))
  ggsave(paste0(figures_dir, "/", fileName, ".pdf"), width = 8.16, height = 5.98)
  ggsave(paste0(figures_dir, "/", fileName, ".jpeg"), width = 8.16, height = 5.98)
  
}

derived_outputs_fun(powersub_merged, "Philippines", "", "Fig_4")
derived_outputs_fun(powersub_merged, "Calabarzon", "Calabarzon", "FigS5_Calabarzon")
derived_outputs_fun(powersub_merged, "National Capital Region", "Central Visayas", "FigS6_Central_Visayas")
derived_outputs_fun(powersub_merged, "National Capital Region", "National Capital Region", "FigS7_NCR")

# Fig. 5 ----------------------------------------------------------------
scenario_plot_fun <- function(df, region, fileName){
  x <- subset(df, Region == region & type == "notifications" & times > 425 & scenario != 1) # may have to reduce scenarios next time
  x <- spread(x, key = quantile, value = value)
  colnames(x)[6:10] <- paste0("X", colnames(x)[6:10])

  p1 <- ggplot(x, aes(x = Date, y = X0.5, group = as.factor(scenario), col = as.factor(scenario))) +
    geom_line(size = 1) +
    scale_color_manual(values=c("black", "darkgreen", "darkmagenta", "#E69F00", "#56B4E9", "darkred"),
                       breaks=c(0,2,6,4,3,5),
                       labels=c("Baseline (current\nconditions sustained)", 
                                "50% home workers\nreturn onsite",
                                "Re-open schools", 
                                "50% reduction MHS", 
                                "70% reduction MHS", 
                                "End MHS"
                                )) +
    ylab("Detected cases (modeled)") +
    xlab("") +
    scale_x_date(date_breaks = "2 months", date_labels =  "%b %Y") +
    theme_bw() +
    theme(
      strip.background = element_rect(
        color="white", fill="white"
      )
    ) +
    theme(
      legend.position = c(0.6, 0.95),
      legend.justification = c("left", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6),
      legend.title = element_blank(),
      legend.background = element_blank()
    ) + 
    theme(text = element_text(size = 8))
  if(region != "Philippines"){
    p1 <- p1 + ggtitle(region)
  } else {
    p1 <- p1 + ggtitle("")
  }
  
  x2 <- subset(x, scenario == 0 | scenario == 5)
  x2 <- subset(x2, times > 424)
  
  p2 <- ggplot(x2, aes(x = Date, y = X0.5, group = as.factor(scenario), col = as.factor(scenario))) +
    geom_ribbon(aes(ymin = X0.025, ymax = X0.975, x = Date, fill = as.factor(scenario)), alpha = 0.3, linetype = 0) +
    geom_ribbon(aes(ymin = X0.25, ymax = X0.75, x = Date, fill = as.factor(scenario)), alpha = 0.5, linetype = 0) +
    geom_line(size = 1) +
    scale_fill_manual(values=c("black", "darkred"))+
    scale_color_manual(values=c("black", "darkred"),
                       breaks=c(0, 3)) +
    ylab("") +
    xlab("") +
    scale_x_date(date_breaks = "2 months", date_labels =  "%b %Y") +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("") + 
    theme(text = element_text(size = 8))
  
  fig7 <- grid.arrange(p1, p2, ncol = 2)
  ggsave(file = paste0(figures_dir, "/", fileName, '.pdf'), fig7, width = 7.46, height = 3.22)
  ggsave(file = paste0(figures_dir, "/", fileName, '.jpg'), fig7, width = 7.46, height = 3.22)
}

scenario_plot_fun(powerbi_uncertainty, "Philippines", "Fig_5_philippines")
scenario_plot_fun(powerbi_uncertainty, "Calabarzon", 'Fig_S10_calabarzon')
scenario_plot_fun(powerbi_uncertainty, "Central Visayas", "Fig_S11_central_visayas")
scenario_plot_fun(powerbi_uncertainty, "National Capital Region", "Fig_S12_NCR")

# Fig. S8 ----------------------------------------------------------------
names <- c("sojourn.compartment_periods_calculated.exposed.total_period",
           "sojourn.compartment_periods_calculated.active.total_period",
           "contact_rate",
           "testing_to_detection.assumed_cdr_parameter",
           "mobility.microdistancing.behaviour.parameters.max_effect",
           "infectious_seed",
           "clinical_stratification.props.symptomatic.multiplier",
           "clinical_stratification.props.hospital.multiplier",
           "infection_fatality.multiplier")

mcmc <- powerbi_mcmc[powerbi_mcmc$name %in% names,]
mcmc <- subset(mcmc, Region == "Philippines")
mcmc$type <- "Posterior"

# create priors
niter <- nrow(mcmc[mcmc$name == "contact_rate",])
priors <- data.frame(name = "sojourn.compartment_periods_calculated.exposed.total_period", value = rtruncnorm(niter, a = 1, mean = 5.5, sd = 0.97))
priors <- rbind(priors, data.frame(name = "sojourn.compartment_periods_calculated.active.total_period", value = rtruncnorm(niter, a = 4, mean = 6.5, sd = 0.77)))
priors <- rbind(priors, data.frame(name = "contact_rate", value = runif(niter, 0.02, 0.04)))
priors <- rbind(priors, data.frame(name = "testing_to_detection.assumed_cdr_parameter", value = runif(niter, 0.02, 0.11)))
priors <- rbind(priors, data.frame(name = "mobility.microdistancing.behaviour.parameters.max_effect", value = runif(niter, 0.1, 0.6)))
priors <- rbind(priors, data.frame(name = "infectious_seed", value = runif(niter, 10, 100)))
priors <- rbind(priors, data.frame(name = "clinical_stratification.props.symptomatic.multiplier", value = rtruncnorm(niter, a = 0.5, mean = 1, sd = 0.2)))
priors <- rbind(priors, data.frame(name = "clinical_stratification.props.hospital.multiplier", value = rtruncnorm(niter, a = 0.5, mean = 1, sd = 0.2)))
priors <- rbind(priors, data.frame(name = "infection_fatality.multiplier", value = rtruncnorm(niter, a = 0.5, mean = 1, sd = 0.2)))
priors$type <- "Prior"

mcmc_full <- rbind(mcmc[, c("name", "value", "type")], priors)
mcmc_full$type <- factor(mcmc_full$type, levels = c("Prior", "Posterior"))

paramLabels <- c("Incubation period",
                 "Duration actively infectious",
                 "Infection risk per contact",
                 "Case detection rate\nat base testing rate",
                 "Max MHS effect",
                 "Initial infectious individuals",
                 "Symptomatic proportion\nadjuster for LMIC",
                 "Hospitalisation \nadjuster for LMIC",
                 "Infection fatality rate\nadjuster for LMIC")

pLabs <- data.frame("name" = names, "paramLabels" = paramLabels, stringsAsFactors=FALSE)
mcmc_full <- merge(mcmc_full, pLabs, by = "name", all.x = T)
mcmc_full$paramLabels <- factor(mcmc_full$paramLabels, levels = c(paramLabels))

mcmc_fig <- ggplot() + 
  geom_density(data = mcmc_full, aes(x = value, fill = type, col = type, group = type), alpha = 0.4) +
  scale_color_manual(values=c("#999999", "#E69F00")) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  facet_wrap(~paramLabels, ncol = 3, scales = "free") +
  theme_bw() +
  ylab("Density") +
  xlab("Parameter value") +
  theme(
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_blank(),
    legend.background = element_blank()
  ) + 
  geom_text(size=8) +
  theme(text = element_text(size = 8))

ggsave(file = paste0(figures_dir, "/Fig_S8.pdf"), mcmc_fig, width = 7, height = 5)
ggsave(file = paste0(figures_dir, "/Fig_S8.jpg"), mcmc_fig, width = 7, height = 5)

# Fig. S9 ----------------------------------------------------------------
testData <- read.csv(paste0(autumn_base_dir, "covid_phl/COVID_Phl_testing.csv"), stringsAsFactors = F)
facilities <- read.csv("data/PHL_testing_facilities.csv", stringsAsFactors = F)

testData <- merge(testData, facilities, by = "facility_name")

phl_tests <- testData %>%
  group_by(report_date) %>%
  summarise(tests = sum(daily_output_samples_tested, na.rm = T))
phl_tests$Region <- "Philippines"

regional_tests <- testData %>%
  group_by(Region, report_date) %>%
  summarise(tests = sum(daily_output_samples_tested, na.rm = T))
regional_tests <- as.data.frame(regional_tests)  

testDF <- rbind(regional_tests, phl_tests[,c("Region", "report_date", "tests")])
testDF$report_date <- as.Date(testDF$report_date, "%Y-%m-%d")
testDF$per_capita_tests <- NA
testDF$per_capita_tests <- ifelse(testDF$Region == "Philippines", (testDF$tests/(sum(pop_phl[,9:29])*1000))*10000, testDF$per_capita_tests)
testDF$per_capita_tests <- ifelse(testDF$Region == "Calabarzon", (testDF$tests/(sum(pop_cal[,5:24])*1000))*10000, testDF$per_capita_tests)
testDF$per_capita_tests <- ifelse(testDF$Region == "Central Visayas", (testDF$tests/(sum(pop_vis[,5:24])*1000))*10000, testDF$per_capita_tests)
testDF$per_capita_tests <- ifelse(testDF$Region == "National Capital Region", (testDF$tests/(sum(pop_ncr[,5:24])*1000))*10000, testDF$per_capita_tests)

testDF <- testDF %>%
  group_by(Region) %>%
  mutate(per_capita_tests_smoothed = rollapply(per_capita_tests, width = 20, FUN = mean, fill = NA, na.rm = T))

cdr <- powerbi_mcmc %>% 
  filter(name == "testing_to_detection.assumed_cdr_parameter") %>%
  group_by(Region) %>%
  sample_n(500) %>% 
  group_by(Region) %>%
  mutate(id = row_number())

testDF2 <- merge(cdr, testDF, by = "Region")
testDF2$CDR <- 1 - exp(-testDF2$value * testDF2$per_capita_tests_smoothed)
testDF2$Region <- factor(testDF2$Region, levels = c("Philippines", "Calabarzon", "Central Visayas", "National Capital Region"))

# remove after re-running!
testDF2$CDR <- ifelse(testDF2$Region == "National Capital Region", testDF2$CDR * 0.5, testDF2$CDR)

testDF3 <- testDF2 %>%
  group_by(Region, report_date) %>%
  summarise(X0.025 = quantile(CDR, 0.025, na.rm = T),
            X0.25 = quantile(CDR, 0.25, na.rm = T),
            X0.5 = quantile(CDR, 0.50, na.rm = T),
            X0.75 = quantile(CDR, 0.75, na.rm = T),
            X0.975 = quantile(CDR, 0.975, na.rm = T))

ggplot(testDF3, aes(x = report_date, y = X0.5)) +
  facet_wrap(~Region, ncol = 2) +
  geom_ribbon(aes(ymin = X0.025, ymax = X0.975, x = report_date), alpha = 0.3, fill="#3366FF") +
  geom_ribbon(aes(ymin = X0.25, ymax = X0.75, x = report_date), alpha = 0.5, fill="#3366FF") +
  geom_line(size = 1, col = 'darkblue') +
  ylab("") +
  xlab("") +
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
  theme_bw() +
  theme(
    strip.background = element_rect(
      color="white", fill="white"
    )
  ) +
  theme(text = element_text(size = 8))

ggsave(paste0(figures_dir, "Fig_S9.pdf"), width = 7.53, height = 5.5)
ggsave(paste0(figures_dir, "Fig_S9.jpeg"), width = 7.53, height = 5.5)

# cumulative notifications by age ----------------------------------------------------------------
casesByAge <- read.csv("data/ConfirmedCases_Final_2021-02-17.csv", stringsAsFactors = F)

# format
casesByAge$Region[casesByAge$Region == "central_visayas"] <-  "Central Visayas"
casesByAge$Region[casesByAge$Region == "manila"] <-  "National Capital Region"
casesByAge$Region[casesByAge$Region == "calabarzon"] <-  "Calabarzon"

casesByAge$AgeGroup[casesByAge$Age <= 4] <- "0-4"
casesByAge$AgeGroup[casesByAge$Age >=5 & casesByAge$Age <= 9] <- "5-9"
casesByAge$AgeGroup[casesByAge$Age >=10 & casesByAge$Age <= 14] <- "10-14"
casesByAge$AgeGroup[casesByAge$Age >=15 & casesByAge$Age <= 19] <- "15-19"
casesByAge$AgeGroup[casesByAge$Age >=20 & casesByAge$Age <= 24] <- "20-24"
casesByAge$AgeGroup[casesByAge$Age >=25 & casesByAge$Age <= 29] <- "25-29"
casesByAge$AgeGroup[casesByAge$Age >=30 & casesByAge$Age <= 34] <- "30-34"
casesByAge$AgeGroup[casesByAge$Age >=35 & casesByAge$Age <= 39] <- "35-39"
casesByAge$AgeGroup[casesByAge$Age >=40 & casesByAge$Age <= 44] <- "40-44"
casesByAge$AgeGroup[casesByAge$Age >=45 & casesByAge$Age <= 49] <- "45-49"
casesByAge$AgeGroup[casesByAge$Age >=50 & casesByAge$Age <= 54] <- "50-54"
casesByAge$AgeGroup[casesByAge$Age >=55 & casesByAge$Age <= 59] <- "55-59"
casesByAge$AgeGroup[casesByAge$Age >=60 & casesByAge$Age <= 64] <- "60-64"
casesByAge$AgeGroup[casesByAge$Age >=65 & casesByAge$Age <= 69] <- "65-69"
casesByAge$AgeGroup[casesByAge$Age >=70 & casesByAge$Age <= 74] <- "70-74"
casesByAge$AgeGroup[casesByAge$Age >=75] <- "75+"

# sum(is.na(casesByAge$AgeGroup)) #2174 not identified to age

casesByAge2 <- casesByAge
casesByAge2$Region <- "Philippines"
casesByAge3 <- rbind(casesByAge, casesByAge2)
# casesByAge_cum <- casesByAge3 %>%
#   filter(Region == "Philippines" | Region == "Central Visayas" | Region == "National Capital Region" | Region == "Calabarzon") %>%
#   filter(!is.na(AgeGroup)) %>%
#   group_by(Region, AgeGroup) %>%
#   summarise(Cumulative_cases = sum(!is.na(Report_Date)))

casesByAge_cumAndProp <- casesByAge3 %>%
  filter(Region == "Philippines" | Region == "Central Visayas" | Region == "National Capital Region" | Region == "Calabarzon") %>%
  filter(!is.na(AgeGroup)) %>%
  group_by(Region) %>%
  summarise(total_cases = sum(!is.na(Report_Date))) %>%
  right_join(casesByAge_cum, by = "Region") %>%
  mutate(Cumulative_cases_prop = Cumulative_cases/total_cases,
         type = "Reported")

# need to subset out modeled data, combine by 10 yr age bins, and then merge with case data
# modeled_cases <- grep("^notificationsXagegroup", unique(powerbi_uncertainty$type), value = TRUE)
# casesByAge_modeled <- powerbi_uncertainty[powerbi_uncertainty$type %in% modeled_cases, ]
# casesByAge_modeled <- casesByAge_modeled[casesByAge_modeled$scenario == 0, ]
# casesByAge_modeled$AgeGroup <- gsub("notificationsXagegroup_|Xclinical_icu|Xclinical_hospital_non_icu|Xclinical_sympt_isolate", "", casesByAge_modeled$type)
# 
# # format
# casesByAge_modeled$AgeGroup[casesByAge_modeled$AgeGroup == "0"] <- "0-4"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "5"] <- "5-9"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "10"] <- "10-14"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "15"] <- "15-19"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "20"] <- "20-24"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "25"] <- "25-29"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age =="30"] <- "30-34"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "35"] <- "35-39"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "40"] <- "40-44"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "45"] <- "45-49"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "50"] <- "50-54"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "55"] <- "55-59"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "60"] <- "60-64"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "65"] <- "65-69"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "70"] <- "70-74"
# casesByAge_modeled$AgeGroup[casesByAge_modeled$Age == "75"] <- "75+"
# 
# casesByAge_modeled_vs_data <- casesByAge_modeled %>%
#   group_by(Region, AgeGroup, quantile) %>%
#   summarise(value = sum(value)) %>%
#   left_join(casesByAge_cum, by = c("Region", "AgeGroup"))

x <- subset(powerbi_derived_outputs, scenario == 0)
notsByAge <- powerbi_derived_outputs[,grep(".*notificationsXagegroup_.*", colnames(powerbi_derived_outputs))]
notsByAge$Region <- powerbi_derived_outputs$Region

# notsByAge2 <- notsByAge %>%
#   gather(key = AgeGroup, value = value, notificationsXagegroup_75:notificationsXagegroup_0) %>%
#   group_by(Region, AgeGroup) %>%
#   summarise(cum_vals = sum(value))
# 
# notsByAge2$AgeGroup <- gsub("notificationsXagegroup_", "", notsByAge2$AgeGroup)
# casesByAge_modeled <- notsByAge2
# casesByAge_modeled2 <- casesByAge_modeled %>%
#   group_by(Region) %>%
#   summarise(cum_total = sum(cum_vals)) %>%
#   right_join(casesByAge_modeled) %>%
#   mutate(cum_prop = cum_vals/cum_total)
# 
# y <- casesByAge_modeled2 %>% left_join(casesByAge_cumAndProp)
# z <- subset(y, Region == "Philippines")
# plot(z$cum_vals, z$Cumulative_cases, col = as.factor(z$AgeGroup), pch = 16, cex = 1.5, ylab = c("Cumulative confirmed cases"), xlab = c("Cumulative modeled cases"))
# plot(z$cum_prop, z$Cumulative_cases_prop, col = as.factor(z$AgeGroup), pch = 16, cex = 1.5, ylab = c("Cumulative confirmed cases"), xlab = c("Cumulative modeled cases"))

notsByAge2 <- notsByAge %>%
  gather(key = AgeGroup, value = value, notificationsXagegroup_75:notificationsXagegroup_0) %>%
  group_by(Region, AgeGroup) %>%
  summarise(Cumulative_cases = sum(value))

notsByAge2$AgeGroup <- gsub("notificationsXagegroup_", "", notsByAge2$AgeGroup)

notsByAge2$AgeGroup[notsByAge2$AgeGroup == "0"] <- "0-4"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "5"] <- "5-9"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "10"] <- "10-14"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "15"] <- "15-19"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "20"] <- "20-24"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "25"] <- "25-29"
notsByAge2$AgeGroup[notsByAge2$AgeGroup =="30"] <- "30-34"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "35"] <- "35-39"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "40"] <- "40-44"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "45"] <- "45-49"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "50"] <- "50-54"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "55"] <- "55-59"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "60"] <- "60-64"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "65"] <- "65-69"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "70"] <- "70-74"
notsByAge2$AgeGroup[notsByAge2$AgeGroup == "75"] <- "75+"

casesByAge_modeled2 <- notsByAge2 %>%
  group_by(Region) %>%
  summarise(total_cases = sum(Cumulative_cases)) %>%
  right_join(notsByAge2) %>%
  mutate(Cumulative_cases_prop = Cumulative_cases/total_cases,
         type = "Modeled")

q <- rbind(casesByAge_modeled2, casesByAge_cumAndProp)


# qplot <- 
ggplot(q, aes(fill=type, y=Cumulative_cases_prop, x=AgeGroup)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  scale_x_discrete(limits=ageLevels) +
  xlab("") +
  ylab("Cumulative cases") +
  facet_wrap(~Region, ncol = 2) +
  scale_fill_manual("legend", values = c("Modeled" = "#3366FF", "Reported" = "black")) +
  theme(legend.position = c(0.9, 0.9),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

ggsave(qplot, file = paste0(figures_dir, "/Fig_X_barplot.pdf"), width = 11.19, height = 7.03)
# proportion
# casesByAge_modeled_vs_data <- casesByAge_modeled %>%
#   group_by(Region, AgeGroup, quantile) %>%
#   summarise(value = sum(value)) %>%
#   left_join(casesByAge_cumAndProp, by = c("Region", "AgeGroup"))
# 
# casesByAge_modeled_vs_data2 <- casesByAge_modeled %>%
#   group_by(Region, quantile) %>%
#   summarise(total_modeled_cases = sum(value)) %>%
#   right_join(casesByAge_modeled_vs_data, by = c("Region", "quantile")) %>%
#   mutate(Modeled_cases_prop = value/total_modeled_cases)

ageLevels <- c("0-4",
                       "5-9",
                       "10-14",
                       "15-19",
                       "20-24",
                       "25-29",
                       "30-34",
                       "35-39",
                       "40-44",
                       "45-49",
                       "50-54",
                       "55-59",
                       "60-64",
                       "65-69",
                       "70-74",
                       "75+")

# multiplot
p1 <- ggplot(casesByAge_modeled_vs_data, aes(x=AgeGroup, y=value)) + 
  geom_boxplot(coef=1e30, alpha = 0.7, fill="#3366FF") +
  geom_point(casesByAge_modeled_vs_data, mapping = aes(x=AgeGroup, y=Cumulative_cases, fill = "Confirmed cases"), show.legend = TRUE) +
  facet_wrap(~Region, scales="free_y", ncol = 2) +
  theme_bw() +
  scale_x_discrete(limits=ageLevels) +
  ylab("Cumulative cases") +
  xlab("Age group (yrs)") +
  theme(legend.position = c(0.9, 0.9),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

ggsave(p1, file = paste0(figures_dir, "/Fig_X.pdf"), width = 11.19, height = 7.03)

# multiplot proportions
ggplot(y, aes(x=AgeGroup, y=cum_vals)) +
  geom_boxplot(coef=1e30, alpha = 0.7, fill="#3366FF") +
  geom_point(y, mapping = aes(x=AgeGroup, y=Cumulative_cases_prop, fill = "Confirmed cases"), show.legend = TRUE) +
  facet_wrap(~Region, scales="free_y", ncol = 2) +
  theme_bw() +
  scale_x_discrete(limits=ageLevels) +
  ylab("Cumulative cases") +
  xlab("Age group (yrs)") +
  theme(legend.position = c(0.9, 0.9),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

# individual plots by region
# plotcasesByAge <- function(region, fileName){
#   x <- subset(casesByAge_modeled_vs_data, Region == region)
#   
#   p <- ggplot(x, aes(x=AgeGroup, y=value)) + 
#     geom_boxplot(coef=1e30, alpha = 0.7, fill="#3366FF") +
#     geom_point(x, mapping = aes(x=AgeGroup, y=Cumulative_cases, fill = "Confirmed cases"), show.legend = TRUE) +
#     theme_bw() +
#     scale_x_discrete(limits=ageLevels) +
#     ylab("Cumulative cases") +
#     xlab("Age group (yrs)") +
#     theme(legend.position = c(0.8, 0.9),
#           legend.title = element_blank())
#   
#   ggsave(file = paste0(figures_dir, "/", fileName, '.pdf'), p, width = 8.67, height = 5.44)
#   ggsave(file = paste0(figures_dir, "/", fileName, '.jpg'), p, width = 8.67, height = 5.44)
# }
# 
# 
# plotcasesByAge("Philippines", "Fig_X_Philippines")
# plotcasesByAge("National Capital Region", "Fig_X_NCR")
# plotcasesByAge("Calabarzon", "Fig_X_Calabarzon")
# plotcasesByAge("Central Visayas", "Fig_X_Central_Visayas")

# percent recovered
recov <- subset(powerbi_uncertainty, scenario == 0 & Region == "Philippines" & type == "proportion_seropositive")
recov2 <- recov %>%
  spread(key = quantile, value = "value") %>%
  filter(Date < "2021-03-01")
colnames(recov2)[6:10] <- paste0("X", colnames(recov2)[6:10])
  
ggplot(recov2, aes(x = Date, y = X0.5)) +
  geom_ribbon(aes(ymin = X0.025, ymax = X0.975, x = Date), alpha = 0.3, fill="#3366FF") +
  geom_ribbon(aes(ymin = X0.25, ymax = X0.75, x = Date), alpha = 0.5, fill="#3366FF") +
  geom_line(size = 1, col = 'darkblue') +
  ylab("") +
  xlab("") +
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
  theme_bw() +
  theme(
    strip.background = element_rect(
      color="white", fill="white"
    )
  ) +
  theme(text = element_text(size = 8))

ggsave(paste0(figures_dir, "seropositive.pdf"), width = 7.53, height = 5.5)

# Table 1
# max effect for text
x <- subset(powerbi_mcmc, Region == "Philippines")

table1 <- x %>%
  group_by(name) %>%
  summarise(X0.025 = quantile(value, 0.025, na.rm = T),
            X0.5 = quantile(value, 0.50, na.rm = T),
            X0.975 = quantile(value, 0.975, na.rm = T)
            )

table1

# incidence peak
y <- subset(powerbi_uncertainty, Region == "Philippines" & type == "incidence" & Date <= "2021-02-17")

peakInc <- y %>%
  group_by(Date) %>%
  summarise(X0.5 = quantile(value, 0.50, na.rm = T)) %>%
  filter(X0.5 == max(X0.5))  

y2 <- subset(y, Date == peakInc$Date)
  
quantile(y2$value, 0.025)
quantile(y2$value, 0.5)
quantile(y2$value, 0.975)

# attack rate 
z <- subset(powersub_merged, Region == "Philippines" & type == "Percentage recovered" & Date == "2021-02-17")
z$X0.025
z$X0.5
z$X0.975

# CDR, although Table 1 above is sufficient
testDF3$Region <- as.character(testDF3$Region)
q <- subset(testDF3, Region == "Philippines" & report_date == "2021-02-10")
q$X0.025
q$X0.5
q$X0.975 
