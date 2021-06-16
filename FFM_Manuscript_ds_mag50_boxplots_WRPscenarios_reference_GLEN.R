#####FFM Manuscript Figure X.
#### Boxplot comparisons: baseline, reference, and reference
#loop through each site and each metric, create scenario curves

#read in scenario information
scenarios.WRP <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/iterations_labeled.csv")
#change all the units of cfs to cms in ffm.labels df
names(scenarios.WRP) <-gsub("cfs", "cms", names(scenarios.WRP))
#convert all to cms
for(i in 2:length(names(scenarios.WRP))){
  #for col i convert to cms
  sub <- as.numeric(scenarios.WRP[,i])*0.028316846592
  #save back into scenario.WRP
  scenarios.WRP[,i] <- sub
}


#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric
#change all the units of cfs to cms in ffm.labels df
ffm.labels[] <- lapply(ffm.labels, function(x) gsub("cfs", "cms", x))

#create list of magnitude metrics that need to be converted
mag.metrics <- ffm.labels$flow_metric[grep("cms",ffm.labels$flow_characteristic)]

#read in WRP scenario percentiles
percentiles.all <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/FFM_percentiles_metricsub_allnodes_WRPscenarios_baseline_03242021.CSV")
#make copy to update from cfs to cms for mag metrics
percentiles.all2 <- percentiles.all

#convert all percentiles of magnitude metrics to cms
for(i in 1:length(percentiles.all$p10)){
  sub <- percentiles.all[i,]
  if(length(which(sub$metric == mag.metrics)) > 0){
    #if mag metric, convert percentiles to cms
    sub$p10 <- sub$p10*0.028316846592
    sub$p25 <- sub$p25*0.028316846592
    sub$p50 <- sub$p50*0.028316846592
    sub$p75 <- sub$p75*0.028316846592
    sub$p90 <- sub$p90*0.028316846592
    
    percentiles.all2[i,] <- sub
  }
}




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

#unique flow metrics 
unique.metrics <- unique(percentiles.all.sort$metric)
#only plot ds baseflow mag
unique.metrics <- c("DS_Mag_50")


output.dir <- "C:/Users/KristineT.SCCWRP2K/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/"
#directory of statewide ref FFM predictions
dir.new<- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/reference"

#unique sites
unique.sites <- unique(percentiles.all.sort$ReportingNode)
#only do this for GLEN
unique.sites <- c("GLEN")

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
  for(m in 1:(length(unique.metrics))){
    #subset scenario percentiles to metric m
    metric.sub <- filter(percentiles.sub, metric == unique.metrics[m])
    #merge with scenario info
    metrics.sub.scenarios <- merge(metric.sub, scenarios.WRP, by= "Scenario")
    
    #get metric info
    #find metric name infor
    metric.info <- ffm.labels[ffm.labels$metric == unique.metrics[m],]
    

    #boxplots comparing baseline, scenarios, reference
    #subset baseline percentiles to metric m
    baseline.sub <- filter(baseline.percentiles, metric == unique.metrics[m])
    #convert cfs to cms
    baseline.sub$p10 <- baseline.sub$p10*0.028316846592
    baseline.sub$p25 <- baseline.sub$p25*0.028316846592
    baseline.sub$p50 <- baseline.sub$p50*0.028316846592
    baseline.sub$p75 <- baseline.sub$p75*0.028316846592
    baseline.sub$p90 <- baseline.sub$p90*0.028316846592
    
    #subset ref percentiles to metric m
    ref.sub <- filter(ref.percentiles, metric == unique.metrics[m])
    #convert cfs to cms
    ref.sub$p10 <- ref.sub$p10*0.028316846592
    ref.sub$p25 <- ref.sub$p25*0.028316846592
    ref.sub$p50 <- ref.sub$p50*0.028316846592
    ref.sub$p75 <- ref.sub$p75*0.028316846592
    ref.sub$p90 <- ref.sub$p90*0.028316846592
    
    #combine baseline, ref percentiles
    baseline.ref.merge <- bind_rows(ref.sub, baseline.sub) 
    baseline.ref.merge$color <- baseline.ref.merge$type
    
    #find percentiles of average WRP discharge (we plot 5 scenarios on boxplots)
    scenario.quantiles.avWRP <- quantile(metrics.sub.scenarios$Avg_Q_cms, c(0.1,  0.5,  0.9))
    #find closest Q WRP to these break points
    split.num <- length(scenario.quantiles.avWRP)
    #find actual station value closes to split.stations.all
    av.WRP.actual <- NA
    
    for(split in 1:split.num){
      #find index of av WRP Q closest to the quantiles
      ind.WRP <- which(abs(metrics.sub.scenarios$Avg_Q_cms - scenario.quantiles.avWRP[split])==min(abs(metrics.sub.scenarios$Avg_Q_cms - scenario.quantiles.avWRP[split])))
      #the actual av WRP Q to use for boxplots
      av.WRP.actual[split] <- metrics.sub.scenarios$Avg_Q_cms[ind.WRP] 
    }
    
    #add in minimum WRP
    av.WRP.actual <- c(min(metrics.sub.scenarios$Avg_Q_cms), av.WRP.actual)
    
    #subset scenarios to the 5 chosen above
    scenario.sub <- metrics.sub.scenarios[metrics.sub.scenarios$Avg_Q_cms %in% av.WRP.actual,]
    #sort based on Av Q WRP
    scenario.sub <- scenario.sub[order(scenario.sub$Avg_Q_cms),]
    #save the type as the Av Q cms WRP
    scenario.sub$type <- paste0(round(scenario.sub$Avg_Q_cms, 2), " cms\nAvg WRP Q")
    scenario.sub$color <- rep("WRP Scenario", length(scenario.sub$Avg_Q_cms))
    
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
      #labs(title = metric.info$title_component, subtitle=metric.info$title_ffm) + 
      ylab(paste0(metric.info$title_component2, metric.info$title_ffm2)) + xlab("") +
      scale_fill_manual(name = "", values = c("grey36", "grey80", "white")) +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      theme_bw() + 
      theme(legend.position="bottom", legend.title = element_blank()) 

    
    #add in line for recreational use
    #kayaking flow limit for GLEN
    kayak.flow.limit <- 64.9*0.028316846592
    fishing.flow.limit <- 96.02036384*0.028316846592
    
    boxplots.rec <- boxplots +
      geom_hline(yintercept=kayak.flow.limit,linetype="dotted", size=.75) +
      annotate(geom = "text", x = 5.6, y = kayak.flow.limit+.1, label = "Kayaking Lower Limit") 
      #geom_hline(yintercept=fishing.flow.limit,linetype="dotted", size=.75) +
      #annotate(geom = "text", x = 5.6, y = fishing.flow.limit+.1, label = "Fishing Lower Limit")
      
      
    
    #print boxplot plots
    print(boxplots.rec)
    
    #filename
    file.name <- paste0("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/manuscripts/sensitivitycurves_FFMs/02_figures/fig7_", metric.info$metric, "_boxplot_WRP_Ref_recreationlimits_GLEN.jpg")
    ggsave(boxplots.rec, filename=file.name, dpi=300, height=5, width=8)
    
  }
}
