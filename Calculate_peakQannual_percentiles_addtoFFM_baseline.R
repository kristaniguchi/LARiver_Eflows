#Flow Recommendation Report - Peak Q metrics calculated for baseline
  #Annual peak Q from hourly timeseries


#other packages
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")
library(ffcAPIClient);library(ggplot2);library(lubridate);library(dplyr);library(dataRetrieval);library(stringr);library(tidyr);library(purrr);library(Cairo)
detach(package:plyr)


#directory with flow output
wd <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/"
#list files
list.files <- list.files(wd, full.names = TRUE)
hrly.files <- grep("Nodes/hourly", list.files)
flow.files <- list.files[hrly.files]
#set output directory
output.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/"

#read in percentiles and annual results FFM output table



#COMID for each reporting node
comid.node <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/reportingnodes_COMID.csv")

#Reporting node description file
reporting.node.names <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- rename(reporting.node.names, ReportingNode = SWMM.Node )

#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric


#loop through the output files and calc FFMs for each reporting node

#create empty percentiles df with all summary values
percentiles.all <- data.frame(matrix(NA,1,10))
names(percentiles.all) <- c("p10","p25","p50","p75","p90","metric","comid","result_type", "source2","ReportingNode")

#annual results
annual.all <- data.frame(matrix(NA,1,3))
names(annual.all) <- c("Year", "ReportingNode", "PeakQ_cfs")

#test out GLEN
#i=8

for(i in 1:length(flow.files)){
  #read in flow file
  data <- read.csv(flow.files[i])
  #reporting node
  node <- gsub(".csv", "", flow.files[i])
  node <- gsub("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/hourly_flow_", "", node)
  #COMID
  comid.i <- comid.node$COMID[comid.node$Name == node]
  
  #format date
  #if 11101250, format date differently
  if(node == "11101250"){
    data$datetime <- as.POSIXct(data$datetime, format="%m/%d/%Y %H:%M")
    data$date <- format(data$datetime, format="%m/%d/%Y")
    data$year <- format(data$datetime, format="%Y")
    data$month <- format(data$datetime, format="%m")
    unique.dates <- unique(data$date)
    
  }else{
    data$datetime <- as.POSIXct(data$datetime, format="%Y-%m-%d %H:%M:%S")
    data$date <- format(data$datetime, format="%m/%d/%Y")
    data$year <- format(data$datetime, format="%Y")
    data$month <- format(data$datetime, format="%m")
    unique.dates <- unique(data$date)
  }
  ################
  
  #Add water year column
  dat<-data %>% 
    mutate(Water.year=ifelse(month > 9, year+1,year))
  
  #Specify water years that have more than 360 discharge values
  keep <- levels(as.factor(dat$Water.year))[table(as.factor(dat$Water.year)) >= 358]
  dat <- dat[as.factor(dat$Water.year) %in% keep, ]
  
  
  # Create summary data frame (dat3) for each water year, find max hourly Q
  dat3<-dat%>%
    group_by(Water.year)%>% 
    summarise(PeakQ=max(flow))
  
  #find the 10th percentile and 90th percentiles of peak Q
  percentiles <- quantile(dat3$PeakQ, c(0.1, 0.25, 0.5, 0.75, 0.9))
  
  #data with percentiles "comid","result_type", "source2","ReportingNode"
  row.data <- c(percentiles, "PeakQ.cfs", comid.i[1], "baseline", "SWMM/n", node)
  #save into overall dfs
  percentiles.all<- rbind(percentiles.all, row.data)
  
  #data with annual results
  annual.data <- data.frame(cbind(dat3$Water.year, node, dat3$PeakQ))
  names(annual.data) <- c("Year", "ReportingNode", "PeakQ_cfs")
  #save into overall dfs
  annual.all <- rbind(annual.all, annual.data)
  
  
}
  

#remove first NA row
percentiles.all2 <- percentiles.all[2:length(percentiles.all$p10),]
annual.all2 <- annual.all[2:length(annual.all$Year),]
annual.all2$Year <- as.numeric(annual.all2$Year)

#write.csv peak annual and percentiles
outfile.name <- paste0(output.dir, "AnnualPeakQ_baseline_percentiles_04142021.csv")
write.csv(percentiles.all2, file=outfile.name, row.names=TRUE)
#save csv 
outfile.name2 <- paste0(output.dir, "AnnualPeakQ_baseline_annual_results_04142021.csv")
write.csv(annual.all2, file=outfile.name2, row.names=TRUE)

#read in ffm annual and percentile results baseline and add in peak flow data to the spreadsheets
ffm.annual <- read.csv(paste0(output.dir, "FFM_annual_results_reportingnodes_all_baseline_02172021.csv"))
ffm.percentiles <- read.csv(paste0(output.dir, "FFM_percentiles_reportingnodes_all_02172021.csv"))

#join by year and node for annual
annual.join <- left_join(ffm.annual, annual.all2, by=c("Year" = "Year", "ReportingNode"="ReportingNode"))

#join by node for percentiles
percentiles.join <- rbind(ffm.percentiles, percentiles.all2)

#write the joined and save it as previous name
write.csv(annual.join, paste0(output.dir, "FFM_annual_results_reportingnodes_all_baseline_02172021.csv"), row.names=FALSE)
write.csv(percentiles.join, paste0(output.dir, "FFM_percentiles_reportingnodes_all_02172021.csv"), row.names=FALSE)



#####UPDATE the wet-season peak flow values from the output table from Katie
flowrange.table.all <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/FlowRanges_Species_RecUses_Allnodes_04132021.csv")

#for all current flow range for Winter Peak Flows, replace upper and lower limit with p10 and p90 of PeakQ_cfs, change metric name
#find index of rows to be updated (current flow, peak)
species.seasonal.component <- paste0(flowrange.table.all$Species, "_", flowrange.table.all$Seasonal.Component)
ind.current.peaks <- grep("Current Flow_Winter Peak Flows", species.seasonal.component)

for(j in ind.current.peaks){
  #subset to row j, current peak flow row
  row.j <- flowrange.table.all[j,]
  #find node
  node <- row.j$Node
  
  #if node is F37B High or Low, use F37B
  ind.check <- grep("F37B", node)
  if(length(ind.check) > 0){
    node <- "F37B"
  }
  
  #subset percentiles
  percentiles.sub <- percentiles.join %>% 
    filter(ReportingNode == node) %>% 
    filter(metric == "PeakQ.cfs")
  
  #update Lower_Limit with p10, Upper_Limit with p90
  row.j$Lower_Limit <- percentiles.sub$p10
  row.j$Upper_Limit <- percentiles.sub$p90
  #update metric to PeakQ.cfs
  row.j$metric <- "PeakQ.cfs"
  
  #save updated row back into ffm df
  flowrange.table.all[j,] <- row.j
  
}


#replace the old file with the updated one
write.csv(flowrange.table.all, file="C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/FlowRanges_Species_RecUses_Allnodes_04132021.csv", row.names=FALSE)

