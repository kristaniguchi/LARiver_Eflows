#### Stormwater Capture Scenarios - Calc FFMs 

#load libraries
#for functional flow calculator:
#install.packages("devtools")
library("devtools")
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library("ffcAPIClient")

#other packages
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")

#my token for FFC API Client
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"
set_token(mytoken) 

#directory where stormwater scenario outputs are saved
#data.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/Results_WRP_UBF/"
data.dir <- "C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/Results_WRP_UBF/"


#scenarios to loop through (12, exclude baseline)
path.all <- list.files(data.dir, full.names = TRUE)
scenario.names <- list.files(data.dir)
path.scenarios <- path.all

#IF EXCLUDING BASELINE RUNS, find index of baseline scenarios, exclude
#ind.baseline <- grep("Baseline", path.all)
#path.scenarios <- path.all[-ind.baseline]
#scenario.names <- list.files(data.dir)[-ind.baseline]
#to run baseline only runs
#ind.baseline <- grep("Baseline", path.all)
#path.scenarios <- path.all[ind.baseline]
#scenario.names <- list.files(data.dir)[ind.baseline]

#temp: only run AggressiveInfiltration_WRP100_UBF100 and AggressiveInfiltration_WRP100_UBF0 
#ind.run1 <- grep("AggressiveInfiltration_WRP100_UBF100", path.all)
#ind.run2 <- grep("AggressiveInfiltration_WRP100_UBF0", path.all)
#path.scenarios <- path.all[c(ind.run2,ind.run1)]
#scenario.names <- scenario.names[c(ind.run2,ind.run1)]



#lookup table that relates SWMM node with SUSTAIN junctions
#junctions <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/Lookup_SWMM_ReportingNode_SUSTAIN_Junctions.csv") %>% 
junctions <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/Lookup_SWMM_ReportingNode_SUSTAIN_Junctions.csv") %>% 
    rename(ReportingNode = Reporting.Node)
#unique junctions to loop through
unique.junctions <- unique(junctions$Junction.Number)

#COMID for each reporting node
#comid.node <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/reportingnodes_COMID.csv")%>% 
comid.node <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/reportingnodes_COMID.csv")%>% 
  rename(ReportingNode = Name)

#Reporting node description file
#reporting.node.names <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- rename(reporting.node.names, ReportingNode = SWMM.Node )

#merge junctions lookup, COMID, reporting node name files
lookup_junctions <- inner_join(junctions, comid.node, by = "ReportingNode") %>% 
  inner_join(reporting.node.names, by = "ReportingNode")

#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric



#####Loop through each scenario and model node (junction) and calc FFMs

#create empty percentiles df with all summary values
percentiles.all <- data.frame(matrix(NA,1,11))
names(percentiles.all) <- c("p10","p25","p50","p75","p90","metric","comid","result_type", "source2","ReportingNode", "Scenario")

#create empty df with all annual results from each scenario
results.all <- data.frame(matrix(NA, 1, 30))
names(results.all) <- c("Year","DS_Dur_WS","DS_Tim","DS_Mag_50","DS_Mag_90","FA_Dur","FA_Mag","FA_Tim","SP_ROC","SP_Dur","SP_Mag","SP_Tim","Wet_BFL_Dur",
                        "Wet_BFL_Mag_10","Wet_BFL_Mag_50","Wet_Tim","Peak_Tim_10","Peak_Tim_2","Peak_Tim_5","Peak_Dur_10","Peak_Dur_2","Peak_Dur_5","Peak_10","Peak_2",       
                        "Peak_5","Peak_Fre_10","Peak_Fre_2","Peak_Fre_5","ReportingNode","Scenario")




#empty vector of error nodes if errors will save here
ffc.errors <- NA

for(i in 1:length(scenario.names)){
  #scenario i name
  scenario.name <- scenario.names[i]
  
  #if Baseline scenario, file will be PostDev_Junction_
  ind.baseline <- grep("Baseline", scenario.name)
  if(length(ind.baseline) > 0){
    #list junction numbers in directory
    junc.files <- list.files(path.scenarios[i], pattern = "PostDev_Junction_")
    #junction full paths
    junc.full.path <- list.files(path.scenarios[i], pattern = "PostDev_Junction_", full.names = TRUE)
  }else{
    #else if not baseline use Init_Junction_ naming convention
    #list junction numbers in directory
    junc.files <- list.files(path.scenarios[i], pattern = "Init_Junction_")
    #junction full paths
    junc.full.path <- list.files(path.scenarios[i], pattern = "Init_Junction_", full.names = TRUE)
  }
  
  
  #create new directory for scenario
  dir.create(paste0(data.dir,scenario.names[i],"/daily/"))
  dir.create(paste0(data.dir,scenario.names[i],"/daily/FFMs/"))

  #loop through each junction associated with SWMM node
  for(j in 1:length(unique.junctions)){
    #get node data for junction j
    sub <- lookup_junctions[lookup_junctions$Junction.Number == unique.junctions[j],] 
    #node name
    node <- as.character(sub$ReportingNode)[1]
    COMID <- sub$COMID[1]
    
    #find junction j index for output file
    ind.junc <- grep(paste0("_", unique.junctions[j],".out"), junc.files)
    #read in junction out
    data <- read.table(junc.full.path[ind.junc], skip = 32)
    names(data) <- c("Junction.Number", "year", "month", "day", "hour", "min", "Volume", "Stage", "Inflow_t", "Outflow_w","Outflow_o","Outflow_ud","Outflow_ut","flow.cfs","Infiltration","Perc","AET","Seepage")
    #format date
    #add leading zero to hour
    MONTH <- sprintf("%02d",data$month)
    DAY <- sprintf("%02d",data$day)
    date <- paste(MONTH, DAY, data$year, sep="/")
    data$date <- as.POSIXct(date, format = "%m/%d/%Y")
    unique.dates <- unique(date)
    #format q to be numeric
    data$flow.cfs <- as.numeric(as.character(data$flow.cfs))
    ################
    
    #calc mean daily flow for dataent predicted data
    mean.daily.data <- data %>% 
      group_by(date) %>% 
      summarize(flow = mean(flow.cfs, ra.rm = TRUE)) %>% 
      ungroup()
    
    #create new data frame with date and mean daily flow to go into FFC
    daily.data <- data.frame(cbind(unique.dates, mean.daily.data$flow))
    names(daily.data) <- c("date", "flow")
    daily.data$flow <- as.numeric(daily.data$flow )
    #write daily output file
    fname <- paste0(data.dir,scenario.names[i],"/daily/", node, "_junction_", unique.junctions[j], "_data_daily.csv")

    write.csv(daily.data, fname, row.names = FALSE)
    ################
    
    #calc FFMs and alteration for current data
    #create new directory to save ffm outputs

    #Try catch errors in evaluate alteration, skip iteration
    tryCatch({
      #Run daily data through FFC online
      #run timeseries data from scenario j through FFC
      #new FFC api set up
      ffc <- FFCProcessor$new()  # make a new object we can use to run the commands
      #allow ffc to run with min of 1 years
      ffc$fail_years_data <- 1
      #setup
      ffc$set_up(timeseries=daily.data,
                 token=mytoken,
                 comid = COMID[1])
      #then run
      ffc$run()
      
      # then pull metrics out as dataframes
      #predicted results, SWMM scenario j
      scenario.percentiles.all <- ffc$ffc_percentiles
      model <- paste0("SUSTAIN_Junction_", unique.junctions[j])
      scenario.percentiles.all$source2 <- rep(model, length(scenario.percentiles.all$p10))
      scenario.percentiles.all$ReportingNode <- rep(node, length(scenario.percentiles.all$p10))
      scenario.percentiles.all$Scenario <- rep(scenario.name, length(scenario.percentiles.all$p10))

      #save scenario percentiles into percentiles.all df
      percentiles.all <- data.frame(rbind(percentiles.all, scenario.percentiles.all))
      #save as backup
      write.csv(percentiles.all, paste0(data.dir,scenario.names[i],"/daily/FFMs/percentiles.all.csv"))
      
      #pull out the annual results
      #annual results
      scenario.results.ffm.all <- ffc$ffc_results
      scenario.results.ffm.all$ReportingNode <- rep(node, length(scenario.results.ffm.all$Year))
      scenario.results.ffm.all$Scenario <- rep(scenario.name, length(scenario.results.ffm.all$Year))
      
      #save annual results into results.all df
      results.all <- data.frame(rbind(results.all, scenario.results.ffm.all))
      #Write annual results all for backup
      write.csv(results.all, file = paste0(data.dir,scenario.names[i], "/daily/FFMs/FFM_annual_results_allnodes_scenarios.csv"))
      
    }, error = function(e) {
      print(paste0(i, " FFC Error"))
      ffc.errors <- c(ffc.errors, node)
      #}, warning = function(w) {
      #print(warnings()) #could delete this if not necessary
    })
  }
}


#remove first NA row percentiles.all
percentiles.all2 <- percentiles.all[2:length(percentiles.all$p10),]
#save reporting node column as class
percentiles.all2$ReportingNode <- as.character(percentiles.all2$ReportingNode)
#write percentiles.all
#write.csv(percentiles.all2, file = "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenariosUrbn.csv", row.names=FALSE)
write.csv(percentiles.all2, file = "C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenariosUrbn.csv", row.names=FALSE)

#remove first NA row results.all
results.all2 <- results.all[2:length(results.all$Year),]
#save reporting node column as class
results.all2$ReportingNode <- as.character(results.all2$ReportingNode)
#write results.all2
#write.csv(results.all2, file = "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_results_SUSTAIN_Junctions_StormwaterScenariosUrbn.csv", row.names=FALSE)
write.csv(results.all2, file = "C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_results_SUSTAIN_Junctions_StormwaterScenariosUrbn.csv", row.names=FALSE)





#Write baseline only runs!
#write.csv(percentiles.all2, file = "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_percentiles_SUSTAIN_BaselineScenarios_urbn_only.csv")

#Temp: read in the percentiles file all bmps and append the 2 scenarios onto that one, save as new csv
#write.csv(percentiles.all2, file = "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenariosUrbn2.csv")
#temp2 <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenariosUrbn2.csv")
#temp.all <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenariosUrbn.csv")
#test <- temp.all %>% 
  #bind_rows(temp2)
#write.csv(test, row.names = FALSE, file="C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenarios_BMPUrbn.csv")
