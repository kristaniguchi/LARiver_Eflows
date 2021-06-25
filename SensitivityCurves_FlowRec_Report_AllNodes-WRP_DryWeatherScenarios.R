#Sensitivity Curves for flow recommendations report appendix
#WRP Scenarios Sensitivity Curves for FFMs - all reporting nodes
#loops through all nodes and generates flow-based sensitivity curves
#curves for WRP and stormwater/stormdrain reduction scenarios
#uses seasonal average WRP for each reporting node and scenario

#other packages
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")


#Read in FFM percentiles from scenarios WRP - all nodes
#ffm.all <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/FFM_percentiles_allnodes_scenarios_02172021.csv")
ffm.all <- read.csv("C:/Users/KristineT/Documents/Git/LARiver_Eflows/FFM_percentiles_allnodes_scenarios_02172021.csv")

#add in baseline FFM percentiles and combine with ffm.all - saved in directory - all nodes
#baseline.ffm <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/FFM_percentiles_reportingnodes_all_02172021.csv")
baseline.ffm <- read.csv("C:/Users/KristineT/Documents/Git/LARiver_Eflows/FFM_percentiles_reportingnodes_all_02172021.csv")

#save baseline scenario as 0 - will add to ffm.all
baseline.ffm$Scenario <- 0
#add rows to ffm.all
ffm.all <- add_row(ffm.all, baseline.ffm)

#read in WRP scenario labels with seasonal WRP  and various values for each reporting node based on which WRP discharges to it
iterations <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/manuscripts/sensitivitycurves_FFMs/summary_seasonalWRP_node.csv") %>% 
  rename(dry_season=dry, wet_season=wet, spring=spr)



#read in FFM percentiles from SUSTAIN stormwater scenarios and stormdrain reduction scenarios [in same dataframe], includes baseline
#ffm.bmp.urbn <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenariosUrbn.csv")
ffm.bmp.urbn <- read.csv("C:/Users/KristineT/Documents/Git/LARiver_Eflows/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenariosUrbn.csv")

#SUSTAIN scenario labels
sustain.scenarios <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Sustain_ScenarioNumbers.csv")

#output directory where sensitivity curves to be saved
#out.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/scenario_curves_FFM_all/seasonal/"
#save in report folder
out.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/sensitivity_curves_all/SeasonalWRP_FlowBased/"
dir.create(out.dir)

#output directory for BMP curves
out.dir.bmp <- paste0(out.dir, "Dryweather_Flowbased/")
dir.create(out.dir.bmp)

#Reporting node description file
reporting.node.names <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- rename(reporting.node.names, ReportingNode = SWMM.Node )

#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric
ffm.labels$metric <- ffm.labels$flow_metric
#unique flow metrics 
unique.metrics <- unique(ffm.labels$metric)
#subset to only mag and wet and dry 
metrics.to.plot <- c("Wet_BFL_Mag_10", "DS_Mag_50") #, "Wet_BFL_Mag_50", "DS_Mag_90")
#metrics.to.plot <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "DS_Mag_50", "DS_Mag_90", "Peak_2")


#join WRP with reporting node names and FFM labels: to get the order of the nodes (upstream to downtream) and the reach order3
#WRP percentiles join
ffm.all.join <- merge(ffm.all, reporting.node.names, by= "ReportingNode") %>% 
  select("X.","ReportingNode", "Description","p10","p25","p50","p75","p90","metric", "Scenario", "Reach", "order","order3","Reporting_Reach") %>% 
  merge(ffm.labels, by = "metric") %>% 
  #merge(iterations, by = "Scenario") %>% 
  mutate(ScenarioType = "WRP") %>% 
  mutate(ScenarioType2 = "WRP")  
  


#join Stormwater urbn with reporting node names and FFM labels: to get the order of the nodes (upstream to downtream) and the reach order3
ffm.all.join.bmp.urbn <- merge(ffm.bmp.urbn, reporting.node.names, by= "ReportingNode") %>% 
  select("X.","ReportingNode", "Description","p10","p25","p50","p75","p90","metric", "Scenario", "Reach", "order","order3","Reporting_Reach") %>% 
  merge(ffm.labels, by = "metric") %>% 
  merge(sustain.scenarios, by = "Scenario") %>% 
  mutate(ScenarioType = "Stormwater, Stormdrain Diversions") %>% 
  mutate(ScenarioType2 = Scenario)


#loop through the scenarios to plot percentiles scenarios curve
#change to unique nodes from bmp urban scenarios
unique.nodes <- unique(ffm.all.join.bmp.urbn$ReportingNode)
#unique.nodes <- unique(ffm.all.join$ReportingNode)
unique.urban.nodes <- unique(ffm.all.join.bmp.urbn$ReportingNode)
unique.wrp.nodes <- unique(ffm.all.join$ReportingNode)
#find missing nodes not in urban
unique.wrp.nodes[!(unique.wrp.nodes %in% unique.urban.nodes)]

#only run for GLEN for TAC presentation
#i <- grep("GLEN", unique.nodes)
#do not make sensitivity curves for LA1 and LA2 [tidal influence] and CC F45B and Upstream Tillman LA20
unique.nodes <- unique.nodes[unique.nodes != "LA1" & unique.nodes != "LA2" & unique.nodes != "F45B" & unique.nodes != "LA20"]

#export csv for Daniel to fit curves for p10, p50, p90 to determine flow range and see if flow range falls within optimal ranges
ffm.all.join.export.metrics <- ffm.all.join %>% 
  filter(metric == "Wet_BFL_Mag_10" | metric == "DS_Mag_50") 
ffm.all.join.export.metrics <- ffm.all.join.export.metrics[ffm.all.join.export.metrics$ReportingNode %in% unique.nodes,]
unique(ffm.all.join.export.metrics$ReportingNode)
#add in season column
ffm.all.join.export.metrics$Season <- NA
ffm.all.join.export.metrics$Season[ffm.all.join.export.metrics$metric == "DS_Mag_50"] <- "Dry"
ffm.all.join.export.metrics$Season[ffm.all.join.export.metrics$metric == "Wet_BFL_Mag_10"] <- "Wet"
#write csv
write.csv(ffm.all.join.export.metrics, file = "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/FFM_percentiles_metricsub_10nodes_WRPscenarios_baseline_03242021.csv", row.names=FALSE)



for(i in 1:length(unique.nodes)){

  #subset percentiles
  #subset WRP to node i
  ffm.sub <- ffm.all.join[ffm.all.join$ReportingNode == unique.nodes[i],]
  #subset SUSTAIN scenarios to node i
  ffm.sub.urban <- ffm.all.join.bmp.urbn[ffm.all.join.bmp.urbn$ReportingNode == unique.nodes[i],]
  
  #add in scenario WRP Q data for node i
  #subset WRP iterations for node i
  iterations.node <- iterations[iterations$ReportingNode == unique.nodes[i],] %>% 
    select(Scenario, dry_season, wet_season, spring)
  #WRP iterations merge with ffm.sub
  ffm.sub <- ffm.sub %>% 
    merge(iterations.node, by="Scenario")
  
  #add in scenario WRPQ data for node i - urban scenarios
  #find ind WRP50 and WRP100 scenarios and put i appropriate values
  ind.WRP50 <- grep("WRP50", ffm.sub.urban$Scenario)
  #WRP 100
  ind.WRP100 <- grep("WRP100", ffm.sub.urban$Scenario)
  #find baseline dry, wet, spring for that node
  baselines <- iterations.node[iterations.node$Scenario == 0,]
  
  #Add in seasonal WRP Q, start with all 0 for WRP0 and will replace with values
  ffm.sub.urban$dry_season <- 0
  ffm.sub.urban$wet_season <- 0
  ffm.sub.urban$spring <- 0
  #replace all WRP50 with seasonal wrp/2
  ffm.sub.urban$dry_season[ind.WRP50] <- baselines$dry_season/2
  ffm.sub.urban$wet_season[ind.WRP50] <- baselines$wet_season/2
  ffm.sub.urban$spring[ind.WRP50] <- baselines$spring/2
  #replace all ind.WRP100 with seasonal wrp
  ffm.sub.urban$dry_season[ind.WRP100] <- baselines$dry_season
  ffm.sub.urban$wet_season[ind.WRP100] <- baselines$wet_season
  ffm.sub.urban$spring[ind.WRP100] <- baselines$spring
  
  
  #loop to create sensitivity curves for each metric
  for(j in 1:length(metrics.to.plot)){
    
    #subset percentiles
    #subset WRP to metric j
    ffm.sub.metric.j <-  ffm.sub[ffm.sub$metric == metrics.to.plot[j],]
    #subset SUSTAIN scenarios to metric j
    ffm.sub.metric.j.urban <-  ffm.sub.urban[ffm.sub.urban$metric == metrics.to.plot[j],]
    

    ####################################
    ####POINT sensitivity Curves, plotting raw flow metric data for p10-p90
    #set colors for point curve
    colors <- c("#0571b0", "#fdae61",  "#ca0020")
    Percentile <- c("p90", "p50", "p10")
    labels1 <- factor(c("90th Percentile", "50th Percentile", "10th Percentile"), levels = c("90th Percentile", "50th Percentile", "10th Percentile"))
    labels <- factor(c("Wet", "Moderate", "Dry"), levels = c("Wet", "Moderate", "Dry"))
    labels2 <- factor(c("Wet Year (2017)", "Moderate Year (2015)", "Dry Year (2016)"), levels = c("Wet Year (2017)", "Moderate Year (2015)", "Dry Year (2016)"))
    lookup <- data.frame(cbind(colors, Percentile, labels, labels2,labels1))
    lookup$Percentile <- factor(Percentile, levels = Percentile)
    
    #get metric info
    metric.info <- ffm.labels[ffm.labels$flow_metric == metrics.to.plot[j],]
    metric.title <- paste0(metric.info$title_component, metric.info$title_ffm)
    
    #pivot longer WRP to format correctly
    data.plot <- pivot_longer(ffm.sub.metric.j, cols = c("p90", "p50", "p10"), names_to="Percentile", values_to="Value")
    data.plot$Percentile <- factor(data.plot$Percentile, levels = Percentile)
    
    #pivot longer .urban to format correctly
    data.plot.urban <- pivot_longer(ffm.sub.metric.j.urban, cols = c("p90", "p50", "p10"), names_to="Percentile", values_to="Value")
    data.plot.urban$Percentile <- factor(data.plot.urban$Percentile, levels = Percentile)
    #merge data with WRP data to plot in one graph with shape as ScenarioType
    #make Scenario from WRP
    data.plot2 <- data.plot %>% 
      mutate(Scenario = as.character(data.plot$Scenario))
    #merge sustain scenarios with WRP plot data, all SUSTAIN scenarios
    data.plot.urban.merge <- data.plot.urban %>% 
      bind_rows(data.plot2)
    #merge with sustain scenario labels (already merged outside loop)
    data.plot.urban.merge2 <- data.plot.urban.merge 
    #replace NA for WRP scenarios with WRP
    data.plot.urban.merge2$BMP_WRP[which(is.na(data.plot.urban.merge2$BMP_WRP))] <- "WRP"
    #replace NA for WRP scenarios with WRP
    data.plot.urban.merge2$UrbanBaseflow_name[which(is.na(data.plot.urban.merge2$UrbanBaseflow_name))] <- "WRP"
    
    
    #subset to BMP WRP only - subset urban removal only
    #subset to all scenarios except urban50 and urban0 (baseflow removal scenarios)
    ind.urb50 <- grep("UBF50", data.plot.urban.merge2$Scenario)
    ind.urb0 <- grep("UBF0", data.plot.urban.merge2$Scenario)
    data.plot.urban.merge2.bmponly <- data.frame(data.plot.urban.merge2[-c(ind.urb50,ind.urb0),])
    #subset to baseflow scenarios only
    data.plot.urban.merge2.urbnonly <- data.plot.urban.merge2 %>% 
      filter(BMP_WRP == "No BMPs") %>% 
      filter(UrbanBaseflow_pctQ != "UBF100") %>% 
      data.frame()
    
    #if dry season metric, set x to dry season wrp
    dry.ind <- grep("Dry", metric.info$title_name)
    if(length(dry.ind) > 0){
      #rename dry season WRP to generic x name
      data.plot <- data.plot %>% 
        rename(seasonal.wrp.Q = dry_season)
      ffm.sub.metric.j <- ffm.sub.metric.j %>% 
        rename(seasonal.wrp.Q = dry_season)
      ffm.sub.metric.j.urban <- ffm.sub.metric.j.urban %>% 
        rename(seasonal.wrp.Q = dry_season)
      data.plot.urban.merge2.urbnonly <- data.plot.urban.merge2.urbnonly %>% 
        rename(seasonal.wrp.Q = dry_season)
      
      
      #x axis label for seasonal wrp
      x.axis <- "Average Annual Dry-Season WRP Discharge (cfs)"
      baseline.metric <- baselines$dry_season
      #y max needs to be max of wetseason baseflow (same scale)
      #subset to 
      #subset WRP to Wet_BFL_Mag_10
      ffm.sub.wet <-  ffm.sub[ffm.sub$metric == "Wet_BFL_Mag_10",]
      #subset SUSTAIN scenarios to Wet_BFL_Mag_10
      ffm.sub.wet.urban <-  ffm.sub.urban[ffm.sub.urban$metric == "Wet_BFL_Mag_10",]
      #find max from wet season baseflow to use same scale on both plots
      y.max.WRP <- max(ffm.sub.wet$p90)

    }else{
      #rename wet season WRP to generic x name 
      #wrp data
      data.plot <- data.plot %>% 
        rename(seasonal.wrp.Q = wet_season)
      ffm.sub.metric.j <- ffm.sub.metric.j %>% 
        rename(seasonal.wrp.Q = wet_season)
      ffm.sub.metric.j.urban <- ffm.sub.metric.j.urban %>% 
        rename(seasonal.wrp.Q = wet_season)
      data.plot.urban.merge2.urbnonly <- data.plot.urban.merge2.urbnonly %>% 
        rename(seasonal.wrp.Q = wet_season)
      
      
      #x axis label for seasonal wrp
      x.axis <- "Average Annual Wet-Season WRP Discharge (cfs)"
      baseline.metric <- baselines$wet_season
      #find max from wet season baseflow to use same y scale on both plots
      y.max.WRP <- max(ffm.sub.metric.j$p90)
    }
    
    ####################################
    #####WRP Flow SENSITIVITY CURVES:
    #Plot as a smooth curve with shaded band (linear reg for p50 with the upper bound being p90 and lower p10) 
    
    #upper bound (p90)
    #fit p90
    fit.p90 <- lm(p90 ~ seasonal.wrp.Q, data = ffm.sub.metric.j)
    summary(fit.p90)
    #predict values 
    upper.bound <- predict(fit.p90, newdata = ffm.sub.metric.j)
    #lower bound (p10)
    #fit p10
    fit.p10 <- lm(p10 ~ seasonal.wrp.Q, data = ffm.sub.metric.j)
    summary(fit.p10)
    #predict values 
    lower.bound <- predict(fit.p10, newdata = ffm.sub.metric.j)
    
    #plot with stat smooth WRP curve
    p2 <- ggplot(ffm.sub.metric.j, aes(x=seasonal.wrp.Q, y = p50)) +
      #geom_point() +
      geom_smooth(method = "lm", color="black") +
      geom_ribbon(aes(ymin=lower.bound, ymax=upper.bound), alpha=0.2) +
      labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
           color = "Legend") + ylab(metric.title) +
      geom_vline(xintercept=baseline.metric, linetype="dashed", color = "black") +
      geom_text(aes(x= baseline.metric-1, y = min(data.plot$Value)+10, label = "Baseline WRP", angle=90), color = "black")+
      xlab(x.axis) + 
      theme_bw() + 
      ylim(c(NA, y.max.WRP*1.1)) +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    #save
    file.name2 <- paste0(out.dir, unique.nodes[i], "_", metric.info$metric, "_WRP_Flow_Sensitivity_Curve.jpg")
    ggsave(p2, filename=file.name2, dpi=300, height=5, width=5)
    
    #baseline WRP p50
    baselinep50 <- max(ffm.sub.metric.j$p50)
    WRP0_p50 <- min(ffm.sub.metric.j$p50)
    change.baseline.0 <- baselinep50 - WRP0_p50
    
    # ###########add in optimal flow range lower limit to WYT plot
    # #overlay optimal flow range on dry year for GLEN only
    # if(unique.nodes[i] == "GLEN"){
    #   #smooth curve
    #   p.flowrange2 <- p2 + 
    #     geom_hline(yintercept=77, color = "grey") 
    #   #save
    #   file.name <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_optimal.lowerlimit.jpg")
    #   #ggsave(p.flowrange2, filename=file.name, dpi=300, height=5, width=5)
    #   
    #   #points plot
    #   p.flowrange <- p + 
    #     geom_hline(yintercept=77, color = "grey") 
    #   #save
    #   file.name <- paste0(out.dir, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_optimal.lowerlimit.jpg")
    #   #ggsave(p.flowrange, filename=file.name, dpi=300, height=5, width=7)
    # }
    
    
    ###
    ####Create smooth sensitivity curve with urban baseflow removal scenarios only
    #Subset 50% reduction urban and pivot wider
    urban50 <- data.plot.urban.merge2.urbnonly %>% 
      filter(UrbanBaseflow_pctQ == "UBF50")  %>% 
      pivot_wider(values_from = Value, names_from = Percentile) %>% 
      data.frame()
    #50% reduction urban upper and lower bounds based on p90 and p10
    upper.bound50 <- urban50$p90
    #predict values 
    lower.bound50 <- urban50$p10
    
    #Subset 100% reduction urban and pivot wider
    urban0 <- data.plot.urban.merge2.urbnonly %>% 
      filter(UrbanBaseflow_pctQ == "UBF0") %>% 
      pivot_wider(values_from = Value, names_from = Percentile) %>% 
      data.frame()
    #100% reduction urban 
    #upper bound (p90)
    upper.bound0 <- urban0$p90
    #lower bound (p10)
    lower.bound0 <- urban0$p10
    
    
    #100% reduction plot with smooth curve
    urban0.p <- p2 +
      geom_ribbon(data = urban0, aes(ymin=lower.bound0, ymax=upper.bound0), alpha=0.2, fill = "red") +
      geom_line(data = urban0, mapping = aes(x=seasonal.wrp.Q, y = p50),color="#d73027", linetype="twodash", lwd=1) 

    #save
    file.name2 <- paste0(out.dir.bmp, unique.nodes[i], "_", metric.info$metric, "_WRP_Flow_Sensitivity_Curve_0Dryweather.jpg")
    #ggsave(urban0.p, filename=file.name2, dpi=300, height=5, width=5)
    
    
    #50% reduction plot with smooth curve
    urban50.p <- urban0.p +
      geom_ribbon(data = urban50, aes(ymin=lower.bound50, ymax=upper.bound50), alpha=0.5, fill = "#fee090") +
      geom_line(data = urban50, mapping = aes(x=seasonal.wrp.Q, y = p50),color="#fc8d59", linetype="twodash", lwd=1) +
      labs(title = paste0("Dry Weather Stormdrain Reduction: ", unique.nodes[i]), subtitle=metric.info$title_name,
           color = "Legend") 
    
    
    #save
    file.name2 <- paste0(out.dir.bmp, unique.nodes[i], "_", metric.info$metric, "_DryweatherStormdrain_Flow_Sensitivity_Curve.jpg")
    ggsave(urban50.p, filename=file.name2, dpi=300, height=5, width=5)
    
    #if GLEN add in lower limit line for optimal flow range used in report figures
    if(unique.nodes[i] == "GLEN"){
      #add lower limit to stormdrain reduction plot
      urban50.p.limit <- urban50.p +
        geom_hline(yintercept=77, linetype="dashed", color = "grey") +
        geom_hline(yintercept=23, linetype="dashed", color = "grey")
      #add lower limit to WRP curve basic
      p2.limit <- p2 +
        geom_hline(yintercept=77, linetype="dashed", color = "grey") +
        geom_hline(yintercept=23, linetype="dashed", color = "grey")
  
      #if dry season, add upper limit of 166
      if(length(dry.ind) > 0){
        #add upper limit to stormdrain reduction plot
        urban50.p.limit <- urban50.p.limit +
          geom_hline(yintercept=166, linetype="dashed", color = "grey")
        #add upper limit to WRP curve basic
        p2.limit <- p2.limit +
          geom_hline(yintercept=166, linetype="dashed", color = "grey")
      }
      #save stormdrain reduction with lower limits
      file.name2 <- paste0(out.dir.bmp, unique.nodes[i], "_", metric.info$metric, "_DryweatherStormdrain_Flow_Sensitivity_Curve_limits.jpg")
      ggsave(urban50.p.limit, filename=file.name2, dpi=300, height=5, width=5)
      #save WRP curve with limits
      file.name3 <- paste0(out.dir, unique.nodes[i], "_", metric.info$metric, "_WRP_Flow_Sensitivity_Curve_limits.jpg")
      ggsave(p2.limit, filename=file.name3, dpi=300, height=5, width=5)
    }
    
    #Change in baseline and urban 50 and 100 scenarios p50
    baselinep50 - WRP0_p50
    #urban 50 and 0 with baseline WRP
    baseline_urban50_p50 <- max(urban50$p50)
    baseline_urban0_p50 <- max(urban0$p50)
    #urban 50 and 0 with 0 WRP
    WRP0_urban50_p50 <- min(urban50$p50)
    WRP0_urban0_p50 <- min(urban0$p50)
    #change in baseline to baseline urban50
    change.baseline.urban50 <- baselinep50 - baseline_urban50_p50
    change.baseline.urban0 <- baselinep50 - baseline_urban0_p50
    ((WRP0_urban0_p50-baselinep50)/baselinep50)*100

  }
}




##################
###Sensitivity Curves for stormwater capture, urban drool removal, and combined - loop through all metrics and evaluate peak flows
#NOTE: this code has not been updated - need to update this for peak flow metrics. Can ignore for now

#loop through the scenarios to plot percentiles scenarios curve
unique.nodes <- unique(ffm.all.join$ReportingNode)

for(i in 1:length(unique.nodes)){
  #subset WRP to node i
  ffm.sub <- ffm.all.join[ffm.all.join$ReportingNode == unique.nodes[i],]
  
  #subset bmps scenarios to node i
  ffm.sub.bmp <- ffm.all.join.bmp[ffm.all.join.bmp$ReportingNode == unique.nodes[i],]
  
  #loop to create sensitivity curves for each metric
  for(j in 1:length(unique.metrics)){
    #subset WRP to metric j
    ffm.sub.metric.j <-  ffm.sub[ffm.sub$metric == unique.metrics[j],]
    
    #subset bmps to metric j
    ffm.sub.metric.j.bmp <-  ffm.sub.bmp[ffm.sub.bmp$metric == unique.metrics[j],]
    
    ####################################
    ####POINT sensitivity Curves, plotting raw flow metric data for p10-p90
    #set colors for point curve
    colors <- c("#0571b0", "#fdae61",  "#ca0020")
    Percentile <- c("p90", "p50", "p10")
    labels <- factor(c("Wet", "Moderate", "Dry"), levels = c("Wet", "Moderate", "Dry"))
    lookup <- data.frame(cbind(colors, Percentile, labels))
    lookup$Percentile <- factor(Percentile, levels = Percentile)
    
    #get metric info
    metric.info <- ffm.labels[ffm.labels$flow_metric == unique.metrics[j],]
    metric.title <- paste0(metric.info$title_component, metric.info$title_ffm)
    
    #pivot longer WRP to format correctly
    data.plot <- pivot_longer(ffm.sub.metric.j, cols = c("p90", "p50", "p10"), names_to="Percentile", values_to="Value")
    data.plot$Percentile <- factor(data.plot$Percentile, levels = Percentile)
    
    #pivot longer .bmp to format correctly
    data.plot.bmp <- pivot_longer(ffm.sub.metric.j.bmp, cols = c("p90", "p50", "p10"), names_to="Percentile", values_to="Value")
    data.plot.bmp$Percentile <- factor(data.plot.bmp$Percentile, levels = Percentile)
    #merge data with WRP data to plot in one graph with shape as ScenarioType
    #make Scenario from WRP
    data.plot2 <- data.plot %>% 
      mutate(Scenario = as.character(data.plot$Scenario))
    data.plot.bmp.merge <- data.plot.bmp %>% 
      bind_rows(data.plot2)
    
    ####Create point sensitivity curve with WRP and stormwater combined
    
    #if peak flow, then don't do color since on value for POR, no WYT
    if(metric.info$title_component == "Peak Flow Magnitude"){
      #create new dataframe to plot with scenario colors for BMP
      data.plot.bmp.merge2 <- data.plot.bmp.merge %>% 
        mutate(ScenarioBMP = Scenario)
      #remove WRP scenario at end of scenario BMP
      data.plot.bmp.merge2$ScenarioBMP <- gsub("_WRP", "__WRP", data.plot.bmp.merge2$ScenarioBMP)
      data.plot.bmp.merge2$ScenarioBMP <- sapply( strsplit(data.plot.bmp.merge2$ScenarioBMP, split="__"), `[`, 1)
      unique(data.plot.bmp.merge2$ScenarioBMP) 
      #rename BMP scenarios
      data.plot.bmp.merge2$ScenarioBMP <- gsub("AggressiveInfiltration", "Aggressive Infiltration", data.plot.bmp.merge2$ScenarioBMP)
      data.plot.bmp.merge2$ScenarioBMP <- gsub("Conservative_non_Infiltration", "Conservative No Infiltration", data.plot.bmp.merge2$ScenarioBMP)
      data.plot.bmp.merge2$ScenarioBMP <- gsub("Aggressive_non_Infiltration", "Aggresive No Infiltration", data.plot.bmp.merge2$ScenarioBMP)
      data.plot.bmp.merge2$ScenarioBMP <- gsub("ConservativeInfiltration", "Conservative Infiltration", data.plot.bmp.merge2$ScenarioBMP)
      #replace scenario number from WRP with No BMP
      data.plot.bmp.merge2$ScenarioBMP[data.plot.bmp.merge2$ScenarioType == "WRP"] <- "No BMPs"
      unique(data.plot.bmp.merge2$ScenarioBMP)
      #save as factor
      #if baseline included add it into the levels, if not exclude it
      data.plot.bmp.merge2$ScenarioBMP <- factor(data.plot.bmp.merge2$ScenarioBMP, levels = c("No BMPs", "Conservative No Infiltration", "Aggresive No Infiltration", "Conservative Infiltration", "Aggressive Infiltration"))
      metric.title <- paste0(metric.info$flow_component, metric.info$title_ffm)
      
      p.bmp <- ggplot(data.plot.bmp.merge2, aes(x=Avg_Q_cfs, y = Value, color=ScenarioBMP)) +
        geom_point(mapping = aes(shape = ScenarioType, size = ScenarioType)) +
        labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
             color = "Legend") + ylab(metric.title) +
        xlab("Average Annual WRP Discharge (cfs)") +
        #scale_color_manual(values = colors, labels = labels, name="Water Year Type") +
        scale_shape_manual(values = c("Stormwater + WRP"=2, "WRP"=20), name="Scenario Type") +
        scale_size_manual(values = c("Stormwater + WRP"=4, "WRP"=2), name="Scenario Type") +
        theme_bw() +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
      #print plot
      print(p.bmp)
      #save
      file.name <- paste0(out.dir.bmp, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_BMP_WRP.jpg")
      ggsave(p.bmp, filename=file.name, dpi=300, height=5, width=7)
      
    }else{
      p.bmp <- ggplot(data.plot.bmp.merge, aes(x=Avg_Q_cfs, y = Value, color = Percentile)) +
        geom_point(mapping = aes(shape = ScenarioType, size = ScenarioType)) +
        labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
             color = "Legend") + ylab(metric.title) +
        xlab("Average Annual WRP Discharge (cfs)") +
        scale_color_manual(values = colors, labels = labels, name="Water Year Type") +
        scale_shape_manual(values = c("Stormwater + WRP"=2, "WRP"=20), name="Scenario Type") +
        scale_size_manual(values = c("Stormwater + WRP"=4, "WRP"=2), name="Scenario Type") +
        theme_bw() +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
      #print plot
      print(p.bmp)
      #save
      file.name <- paste0(out.dir.bmp, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_BMP_WRP.jpg")
      ggsave(p.bmp, filename=file.name, dpi=300, height=5, width=7)
      
    }
  }
}


