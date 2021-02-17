#LA River Water Reclamation Plant (WRP) Scenarios: Functional Flow Metrics
#By Kris Taniguchi-Quan

#load libraries
#for functional flow calculator:
#install.packages("devtools")
library("devtools")
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library("ffcAPIClient")

#to uninstall package and reinstall (if updates to R package were made)
#remove.packages("ffcAPIClient") #uninstall then restart R session
#library("devtools")
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
#install.packages("ffcAPIClient")
#library("ffcAPIClient")


#other packages
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")

#my token for FFC API Client
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"
#set token
set_token(mytoken)

#directory with flow output from 500 scenarios
#laptop or desktop need to update:
#wd <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results/"
wd <- "C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results/"

#list files
flow.files <- list.files(wd, full.names = TRUE)
flow.file.name <- list.files(wd)

#set output directory - change depending on laptop or desktop
#output.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/"
output.dir <- "C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/"


#COMID for each reporting node
#comid.node <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/reportingnodes_COMID.csv")%>% 
comid.node <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/reportingnodes_COMID.csv")%>% 
  rename(ReportingNode = Name)

#Reporting node description file
#reporting.node.names <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- read.csv("C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- rename(reporting.node.names, ReportingNode = SWMM.Node )


#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric



#loop through the output files and calc FFMs for each scenario (500) and each reporting node (16)

#create empty percentiles df with all summary values
percentiles.all <- data.frame(matrix(NA,1,11))
names(percentiles.all) <- c("p10","p25","p50","p75","p90","metric","comid","result_type", "source2","ReportingNode", "Scenario")
#create empty df with all annual results from each scenario
results.all <- data.frame(matrix(NA, 1, 30))
names(results.all) <- c("Year","DS_Dur_WS","DS_Tim","DS_Mag_50","DS_Mag_90","FA_Dur","FA_Mag","FA_Tim","SP_ROC","SP_Dur","SP_Mag","SP_Tim","Wet_BFL_Dur",
                        "Wet_BFL_Mag_10","Wet_BFL_Mag_50","Wet_Tim","Peak_Tim_10","Peak_Tim_2","Peak_Tim_5","Peak_Dur_10","Peak_Dur_2","Peak_Dur_5","Peak_10","Peak_2",       
                        "Peak_5","Peak_Fre_10","Peak_Fre_2","Peak_Fre_5","ReportingNode","Scenario")


#test out certain nodes
i = 7 # test GLEN
#i =  2 #F319 wardlow

for(i in 1:length(flow.files)){
#for(i in 11:length(flow.files)){
    
  #read in flow file
  data <- read.csv(flow.files[i])
  #reporting node
  node <- gsub("_all_Q.csv", "", flow.file.name[i])

  #loop through each of the 500 scenarios (columns, first col is date) and run FFC
  for(j in 2:501){
  #for(j in 402:501){
    #create new data frame with date and mean daily flow to go into FFC
    data.daily <- data.frame(cbind(data$V1, as.numeric(data[,j])))
    names(data.daily) <- c("date", "flow")
    
    ####FFM 
    #calc FFMs and alteration for future data
    #find COMID for reporting node i
    COMID <- comid.node$COMID[comid.node$ReportingNode == node]
    
    #run timeseries data from scenario j through FFC
    #new FFC api set up
    ffc <- FFCProcessor$new()  # make a new object we can use to run the commands
    #allow ffc to run with min of 1 years
    ffc$fail_years_data <- 1
    #setup
    ffc$set_up(timeseries=data.daily,
               token=mytoken,
               comid = COMID[1])
    #then run
    ffc$run()
    
    # then pull metrics out as dataframes
    #predicted results, SWMM scenario j
    scenario.percentiles.all <- ffc$ffc_percentiles
    model <- paste0("SWMM\n")
    scenario.percentiles.all$source2 <- rep(model, length(scenario.percentiles.all$p10))
    scenario.percentiles.all$ReportingNode <- rep(node, length(scenario.percentiles.all$p10))
    scenario.percentiles.all$Scenario <- rep(j-1, length(scenario.percentiles.all$p10))
    
    #save scenario percentiles into percentiles.all df
    percentiles.all <- data.frame(rbind(percentiles.all, scenario.percentiles.all))
    
    #pull out the wet, moderate, and dry year annual results
    #annual results
    scenario.results.ffm.all <- ffc$ffc_results
    scenario.results.ffm.all$ReportingNode <- rep(node, length(scenario.results.ffm.all$Year))
    scenario.results.ffm.all$Scenario <- rep(j-1, length(scenario.results.ffm.all$Year))
    
    #save annual results into results.all df
    results.all <- data.frame(rbind(results.all, scenario.results.ffm.all))
    

    #if first scenario, pull ref predictions and save
    if(j == 2){
      #get predicted percentiles
      ref.percentiles <- ffc$predicted_percentiles
      ref.percentiles$source2 <- rep("Statewide\nReference", length(ref.percentiles$p10))
      #write reference outputs
      #create new directory to save ffm outputs
      dir.new <- paste0(output.dir,"reference")
      dir.create(dir.new)
      write.csv(ref.percentiles, file=paste0(dir.new,"/", node, ".ref.percentiles.statewide.csv"), row.names=FALSE)
    }
  }
  #Write percentiles all for backup
  write.csv(percentiles.all, file = paste0(output.dir, "/FFM_percentiles_allnodes_scenarios.csv"))
  #Write annual results all for backup
  write.csv(results.all, file = paste0(output.dir, "/FFM_annual_results_allnodes_scenarios.csv"))
  
}

#remove first NA row
percentiles.all2 <- percentiles.all[2:length(percentiles.all$p10),]
#save reporting node column as class
percentiles.all2$ReportingNode <- as.character(percentiles.all2$ReportingNode)
#write percentiles.all
write.csv(percentiles.all2, file = paste0(output.dir, "/FFM_percentiles_allnodes_scenarios_02172021.csv"))




#####Format all percentiles for all nodes ###########
#### select columns and join #### 
#note you need to have percentiles already calculated and formatted similar to percentiles.all3
percentiles.all3 <- select(percentiles.all2, "ReportingNode", "p10","p25","p50","p75","p90","metric", "Scenario") 

#join with reporting node names: to get the order of the nodes (upstream to downtream) and the reach order3
percentiles.all.join <- merge(percentiles.all3, reporting.node.names, by= "ReportingNode") %>% 
  select("X.","ReportingNode", "Description","p10","p25","p50","p75","p90","metric", "Scenario", "Reach", "order","order3","Reporting_Reach")

#sort based on metric and X.
percentiles.all.sort <- percentiles.all.join[order(percentiles.all.join$metric, percentiles.all.join$X.),]

#round values
percentiles.all.sort.rnd <- percentiles.all.sort %>% mutate_if(is.numeric, ~round(., 2))
#write.csv(percentiles.all.sort.rnd, file= "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/FFM_percentiles_reportingnodes_all_formattedround.csv")



#### Boxplot comparisons: baseline, reference, and reference
#loop through each site and each metric, create scenario curves

#read in scenario information
scenarios.WRP <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/iterations_labeled.csv")

#unique flow metrics 
unique.metrics <- unique(percentiles.all.sort$metric)
#unique sites
unique.sites <- unique(percentiles.all.sort$ReportingNode)

for(l in 1:length(unique.sites)){
  #subset to scenario percentiles for unique site j
  percentiles.sub <- filter(percentiles.all.sort, ReportingNode == unique.sites[l])
  
  #find baseline percentiles for node
  baseline.dir <- paste0("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/", unique.sites[l],"/baseline.percentiles.all.", unique.sites[l],".csv")
  baseline.percentiles <- read.csv(baseline.dir)
  baseline.percentiles$type <- rep("Baseline", length(baseline.percentiles$p10))
  
  #ref percentiles for node from statewide model
  ref.dir <- paste0(dir.new,"/", unique.sites[l], ".ref.percentiles.statewide.csv")
  ref.percentiles <- read.csv(ref.dir)
  ref.percentiles$type <- rep("Reference", length(ref.percentiles$p10))
  
  #loop through each metric
  for(m in 1:(length(unique.metrics)-1)){
    #subset scenario percentiles to metric m
    metric.sub <- filter(percentiles.sub, metric == unique.metrics[m])
    #merge with scenario info
    metrics.sub.scenarios <- merge(metric.sub, scenarios.WRP, by= "Scenario")
    
    #get metric info
    #find metric name infor
    metric.info <- ffm.labels[ffm.labels$metric == unique.metrics[m],]
    
    #plot sensitivity curve for metric and reporting node
    p <- ggplot(metrics.sub.scenarios, aes(x = Avg_Q_cfs, y = p50)) + 
      geom_point() +
      labs(title = metric.info$title_component, subtitle=metric.info$title_ffm) + 
      ylab(metric.info$title_ffm) +
      theme(legend.position="bottom")
    
    #boxplots comparing baseline, scenarios, reference
    #subset baseline percentiles to metric m
    baseline.sub <- filter(baseline.percentiles, metric == unique.metrics[m])
    #subset ref percentiles to metric m
    ref.sub <- filter(ref.percentiles, metric == unique.metrics[m])
    
    #combine baseline, ref percentiles
    baseline.ref.merge <- bind_rows(ref.sub, baseline.sub) 
    baseline.ref.merge$color <- baseline.ref.merge$type
    
    #find percentiles of average WRP discharge (we plot 5 scenarios on boxplots)
    scenario.quantiles.avWRP <- quantile(metrics.sub.scenarios$Avg_Q_cfs, c(0.1,  0.5,  0.9))
    #find closest Q WRP to these break points
    split.num <- length(scenario.quantiles.avWRP)
    #find actual station value closes to split.stations.all
    av.WRP.actual <- NA
    
    for(split in 1:split.num){
      #find index of av WRP Q closest to the quantiles
      ind.WRP <- which(abs(metrics.sub.scenarios$Avg_Q_cfs - scenario.quantiles.avWRP[split])==min(abs(metrics.sub.scenarios$Avg_Q_cfs - scenario.quantiles.avWRP[split])))
      #the actual av WRP Q to use for boxplots
      av.WRP.actual[split] <- metrics.sub.scenarios$Avg_Q_cfs[ind.WRP] 
    }
    
    #add in minimum WRP
    av.WRP.actual <- c(min(metrics.sub.scenarios$Avg_Q_cfs), av.WRP.actual)
    
    #subset scenarios to the 5 chosen above
    scenario.sub <- metrics.sub.scenarios[metrics.sub.scenarios$Avg_Q_cfs %in% av.WRP.actual,]
    #sort based on Av Q WRP
    scenario.sub <- scenario.sub[order(scenario.sub$Avg_Q_cfs),]
    #save the type as the Av Q cfs WRP
    scenario.sub$type <- paste0(round(scenario.sub$Avg_Q_cfs, 0), " cfs\nAvg Q")
    scenario.sub$color <- rep("WRP Scenario", length(scenario.sub$Avg_Q_cfs))
    
    #merge with current, baseline
    percentiles.all.sub <- bind_rows(baseline.ref.merge, scenario.sub) 
    
    #set levels for the scenario boxplots
    levels <- sort(percentiles.all.sub$type, decreasing=TRUE)
    levels <- c(levels[2:length(levels)], levels[1])
    percentiles.all.sub$type <- factor(percentiles.all.sub$type, levels)
    #set levels for colors
    percentiles.all.sub$color <- factor(percentiles.all.sub$color, levels= c("Baseline", "WRP Scenario", "Reference"))
    
    boxplots <- ggplot(percentiles.all.sub, aes(x=type, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=color)) +
      geom_boxplot(stat="identity")  + 
      labs(title = metric.info$title_component, subtitle=metric.info$title_ffm) + 
      ylab(metric.info$title_ffm) + xlab("") +
      theme(legend.position="bottom", legend.title = element_blank())
    
    #print boxplot plots
    print(boxplots)
    
  }
}




##### Visualizing flow metrics ###########

#loop to go through each metric, make metric boxplots for each reporting node from downstream to upstream

#unique flow metrics 
unique.metrics <- unique(percentiles.all.sort$metric)

#for the plots, we want to show upstream to downstream, and rio hondo and compton creek where they input
#sort based on metric and order
#percentiles.all.sort2 <- percentiles.all.join[order(percentiles.all.join$metric, percentiles.all.join$order),]
#use order3 to have rio hondo and compton creek plotted at the end
percentiles.all.sort2 <- percentiles.all.join[order(percentiles.all.join$metric, percentiles.all.join$order3),]


for(k in 1:(length(unique.metrics)-1)){
  #### filter  based on metric k#### 
  sub <- filter(percentiles.all.sort2, metric == unique.metrics[k])
  #save reportingnode as vector with order
  sub$ReportingNode <- factor(sub$ReportingNode, levels = unique(sub$ReportingNode))
  #find metric name infor
  metric.info <- ffm.labels[ffm.labels$metric == unique.metrics[k],]
  #make reach a factor
  sub$Reach <- factor(sub$Reach, levels=c("Mainstem", "Rio Hondo", "Compton Creek"))
  #max metric value
  max <- max(sub$p50)
  min <- min(sub$p50)
  range <- max - ((max - min)/10)
  
  #create plot of median values --> don't use for report
  p <- ggplot(sub, aes(x = ReportingNode, y = p50, group = Reach)) + geom_point(aes(shape=Reach, color=Reach), size=2) +
    labs(title = metric.info$title_component, subtitle=metric.info$title_ffm) + ylab(metric.info$title_ffm) +
    theme(legend.position="bottom") +
    geom_vline(aes(xintercept = 5.5),linetype="dotted") + annotate(geom = "text", x = 5.2, y = range, label = "Glendale", angle = 90) +
    geom_vline(aes(xintercept = 3.5),linetype="dotted") + annotate(geom = "text", x = 3.2, y = range, label = "Burbank", angle = 90) +
    geom_vline(aes(xintercept = 2.5),linetype="dotted") + annotate(geom = "text", x = 2.2, y = range, label = "Tillman", angle = 90) 
  #print(p)
  
  
  ####create boxplots####
  #remove duplicate compton creek values
  ind.cc <- grep("Upper Compton", sub$Description)
  sub2 <- sub[-ind.cc,]
  
  b<- ggplot(sub2, aes(x=ReportingNode, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90, fill=Reach, color=Reach)) +
    geom_boxplot(stat="identity")  + 
    labs(title = metric.info$title_component, subtitle=metric.info$title_ffm) + ylab(metric.info$title_ffm) +
    theme(legend.position="bottom") +
    scale_color_manual(name = "Reach", labels = , values = c("#006d2c", "#b30000", "#54278f")) +
    scale_fill_manual(name = "Reach", values = c("#66c2a5", "#fc8d62", "#8da0cb")) +
    geom_vline(aes(xintercept = 5.5),linetype="dotted") +
    geom_vline(aes(xintercept = 3.5),linetype="dotted") +
    geom_vline(aes(xintercept = 2.5),linetype="dotted") 
  
  #add in the WRP annotation: will need to change the y location for each depending on your y axis
  if(unique.metrics[k] == "DS_Mag_50"){
    b <- b +  annotate(geom = "text", x = 5.2, y = 132, label = "Glendale", angle = 90) +
      annotate(geom = "text", x = 3.2, y = 132, label = "Burbank", angle = 90) +
      annotate(geom = "text", x = 2.2, y = 132, label = "Tillman", angle = 90) 
  }
  if(unique.metrics[k] == "Wet_BFL_Mag_50"){
    b <- b + 
      annotate(geom = "text", x = 5.2, y = 180, label = "Glendale", angle = 90) +
      annotate(geom = "text", x = 3.2, y = 180, label = "Burbank", angle = 90) +
      annotate(geom = "text", x = 2.2, y = 180, label = "Tillman", angle = 90) 
  }
  print(b)
  file.name <- paste0("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/", metric.info$metric, "_boxplot_reportingnodes.jpg")
  ggsave(b, filename=file.name, dpi=300, height=5, width=9.5)
  
  
}




