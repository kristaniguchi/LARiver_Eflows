#WRP Scenarios Sensitivity Curves for FFMs - all reporting nodes
#loop through all nodes and generate sensitivity curves
  #curves for WRP and overlay with flow ranges

#other packages
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")

#read in flow ranges for specific reporting nodes and species-lifestage
ranges <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/Typha_Steelhead_Cladophora_SAS_Willow_02_05_2021_updated_KI.csv")
#find unique nodes where sensitivity curves should be generated
node.ranges <- unique(ranges$Node)


#Read in FFM percentiles from scenarios WRP
#ffm.all <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/FFM_percentiles_allnodes_scenarios_02172021.csv")
ffm.all <- read.csv("C:/Users/KristineT/Documents/Git/LARiver_Eflows/FFM_percentiles_allnodes_scenarios_02172021.csv")

#GLEN only to test
#ffm.all <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/FFM_percentiles_GLEN_scenarios_02172021.csv")


#add in baseline FFM percentiles and combine with ffm.all
#baseline.ffm <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/FFM_percentiles_reportingnodes_all_02172021.csv")
baseline.ffm <- read.csv("C:/Users/KristineT/Documents/Git/LARiver_Eflows/FFM_percentiles_reportingnodes_all_02172021.csv")

#save scenario as 0 - baseline
baseline.ffm$Scenario <- 0
#add rows to ffm.all
ffm.all <- add_row(ffm.all, baseline.ffm)


#WRP scenario labels
iterations <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/iterations_labeled.csv")
#find max av_q_cfs and half of that to get WRP100 and WRP50
WRP100_Qcfs <- 70.9
WRP50_Qcfs <- WRP100_Qcfs/2
#add in baseline scenario 0  as 70.9 cfs
iterations[501,] <- c(0, 44.75, 8.85, 17.31, WRP100_Qcfs, 73.03)

#read in seasonal WRP values and combine with iterations
seasonal.wrp <- read.csv("C:/Users/KristineT/Documents/Git/LARiver_Eflows/seasonal_WRP_discharge_GLEN.csv") %>% 
  rename(Scenario = scenario)
#merge with iterations
iterations <- iterations %>% 
  join(seasonal.wrp, by = "Scenario")
#UPDATE WITH BASELINE VALUES: add in baseline 0 seasonal Q
baseline.dry <- 67.46948696
baseline.wet <- 74.9882904
baseline.spring <- 67.52783375
#save in baseline scenario 501
iterations$dry_season[501] <- baseline.dry
iterations$wet_season[501] <- baseline.wet
iterations$spring[501] <- baseline.spring

#read in FFM percentiles from SUSTAIN stormwater scenarios and urban baseflow [in same dataframe], includes baseline
#ffm.bmp.urbn <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterUrbanDroolScenarios_02022021/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenariosUrbn.csv")
ffm.bmp.urbn <- read.csv("C:/Users/KristineT/Documents/Git/LARiver_Eflows/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenariosUrbn.csv")

#SUSTAIN scenario labels
sustain.scenarios <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Sustain_ScenarioNumbers.csv")

#output directory where sensitivity curves to be saved
out.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/scenario_curves_FFM/"
#output directory for BMP curves
out.dir.bmp <- paste0(out.dir, "Stormwater_Urbn_Scenario_Curves/")
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
  merge(iterations, by = "Scenario") %>% 
  mutate(ScenarioType = "WRP") %>% 
  mutate(ScenarioType2 = "WRP")


#join Stormwater urbn with reporting node names and FFM labels: to get the order of the nodes (upstream to downtream) and the reach order3
ffm.all.join.bmp.urbn <- merge(ffm.bmp.urbn, reporting.node.names, by= "ReportingNode") %>% 
  select("X.","ReportingNode", "Description","p10","p25","p50","p75","p90","metric", "Scenario", "Reach", "order","order3","Reporting_Reach") %>% 
  merge(ffm.labels, by = "metric") %>% 
  merge(sustain.scenarios, by = "Scenario") %>% 
  mutate(ScenarioType = "Stormwater, Stormdrain Diversions") %>% 
  mutate(ScenarioType2 = Scenario)


#add in Avg_Q_cfs which is for WRP0, 50, 100
#Percentiles:
ffm.all.join.bmp.urbn$Avg_Q_cfs <- 0
#find ind WRP50 and WRP100 scenarios and put i appropriate values
ind.WRP50 <- grep("WRP50", ffm.all.join.bmp.urbn$Scenario)
ffm.all.join.bmp.urbn$Avg_Q_cfs[ind.WRP50] <- WRP50_Qcfs
#WRP 100
ind.WRP100 <- grep("WRP100", ffm.all.join.bmp.urbn$Scenario)
ffm.all.join.bmp.urbn$Avg_Q_cfs[ind.WRP100] <- WRP100_Qcfs

#Add in seasonal percentiles for each WRP
ffm.all.join.bmp.urbn$dry_season <- 0
ffm.all.join.bmp.urbn$wet_season <- 0
ffm.all.join.bmp.urbn$spring <- 0
#replace all WRP50 with seasonal wrp/2
ffm.all.join.bmp.urbn$dry_season[ind.WRP50] <- baseline.dry/2
ffm.all.join.bmp.urbn$wet_season[ind.WRP50] <- baseline.wet/2
ffm.all.join.bmp.urbn$spring[ind.WRP50] <- baseline.spring/2
#replace all ind.WRP100 with seasonal wrp
ffm.all.join.bmp.urbn$dry_season[ind.WRP100] <- baseline.dry
ffm.all.join.bmp.urbn$wet_season[ind.WRP100] <- baseline.wet
ffm.all.join.bmp.urbn$spring[ind.WRP100] <- baseline.spring



#loop through the scenarios to plot percentiles scenarios curve
unique.nodes <- unique(ffm.all.join$ReportingNode)
#only run for GLEN first
i <- grep("GLEN", unique.nodes)


#for(i in 1:length(unique.nodes)){
  
  #subset flow ranges to node i
  ranges.sub <- ranges[ranges$Node == unique.nodes[i],]
  
  #subset percentiles
  #subset WRP to node i
  ffm.sub <- ffm.all.join[ffm.all.join$ReportingNode == unique.nodes[i],]
  #subset SUSTAIN scenarios to node i
  ffm.sub.urban <- ffm.all.join.bmp.urbn[ffm.all.join.bmp.urbn$ReportingNode == unique.nodes[i],]
  

  #loop to create sensitivity curves for each metric
  for(j in 1:length(metrics.to.plot)){
    
    #subset percentiles
    #subset WRP to metric j
    ffm.sub.metric.j <-  ffm.sub[ffm.sub$metric == metrics.to.plot[j],]
    #subset SUSTAIN scenarios to metric j
    ffm.sub.metric.j.urban <-  ffm.sub.urban[ffm.sub.urban$metric == metrics.to.plot[j],]
    
    #subset ranges to metric j
    ranges.metric.j <- ranges.sub[ranges.sub$metric == metrics.to.plot[j],]
    #UPDATE: EXCLUDE CLADOPHORA FOR NOW, OUTSIDE OF RANGE FOR GLEN
    ranges.metric.j <- ranges.metric.j[ranges.metric.j$Species_Label != "Cladophora Adult",]
    #subset to existing and future BU
    existing.ranges.metric.j <- ranges.metric.j[ranges.metric.j$Designation == "Existing",]
    future.ranges.metric.j <- ranges.metric.j[ranges.metric.j$Designation == "Future",]
    #subset to medium and high probability - Existing
    #find indices of medium and high and subset to not those rows to always include thresholds without probability
    ind.medium <- grep("Medium", existing.ranges.metric.j$Probability_Threshold)
    ind.high <- grep("High", existing.ranges.metric.j$Probability_Threshold)
    #subset to medium and high probability
    existing.ranges.metric.j.med <- existing.ranges.metric.j[-ind.high,]
    existing.ranges.metric.j.high <- existing.ranges.metric.j[-ind.medium,]
    #subset to medium and high probability - future
    #find indices of medium and high and subset to not those rows to always include thresholds without probability
    ind.medium <- grep("Medium", future.ranges.metric.j$Probability_Threshold)
    ind.high <- grep("High", future.ranges.metric.j$Probability_Threshold)
    ind.NA <- which(is.na(future.ranges.metric.j$Probability_Threshold))
    #subset to medium and high probability
    future.ranges.metric.j.med <- future.ranges.metric.j[-ind.high,]
    future.ranges.metric.j.high <- future.ranges.metric.j[-ind.medium,]
    
    
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
      x.axis <- "Average Dry-Season WRP Discharge (cfs)"
      baseline.metric <- baseline.dry
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
      x.axis <- "Average Wet-Season WRP Discharge (cfs)"
      baseline.metric <- baseline.wet
    }
    
    ###create sensitivity curve for WRP only using seasonal WRP and percentile points showing distribution
    p <- ggplot(data.plot, aes(x=seasonal.wrp.Q, y = Value, color = Percentile)) +
      geom_point() +
      labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
           color = "Legend") + ylab(metric.title) +
      xlab(x.axis) +
      scale_color_manual(values = colors, labels = labels1, name="Flow Percentile") +
      theme_bw() +
      geom_vline(xintercept=baseline.metric, linetype="dashed", color = "black") +
      geom_text(aes(x= baseline.metric-1, y = 25, label = "Baseline WRP", angle=90), color = "black")+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    #print plot
    print(p)
    #save
    file.name <- paste0(out.dir, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_seasonal.jpg")
    ggsave(p, filename=file.name, dpi=300, height=5, width=7)
    
   #DRY plot only: take out wet and moderate and only show dry
    p.50 <- p + 
    geom_point(ffm.sub.metric.j, mapping = aes(x=seasonal.wrp.Q, y = p90), color = "white") +
    geom_point(ffm.sub.metric.j, mapping = aes(x=seasonal.wrp.Q, y = p10), color = "white") 
    #save
    file.name <- paste0(out.dir, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_p50.jpg")
    ggsave(p.50, filename=file.name, dpi=300, height=5, width=7)
    
    
    
    ####################################
    #####SMOOTH SENSITIVITY CURVES:
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
      geom_text(aes(x= baseline.metric-1, y = 25, label = "Baseline WRP", angle=90), color = "black")+
      xlab(x.axis) + 
      theme_bw() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    #save
    file.name2 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve.jpg")
    ggsave(p2, filename=file.name2, dpi=300, height=5, width=5)
    
    ###########add in optimal flow range lower limit to WYT plot
    #overlay optimal flow range on dry year for GLEN only
    if(unique.nodes[i] == "GLEN"){
      #smooth curve
      p.flowrange2 <- p2 + 
        geom_hline(yintercept=77, color = "grey") 
      #save
      file.name <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_optimal.lowerlimit.jpg")
      ggsave(p.flowrange2, filename=file.name, dpi=300, height=5, width=5)
      
      #points plot
      p.flowrange <- p + 
        geom_hline(yintercept=77, color = "grey") 
      #save
      file.name <- paste0(out.dir, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_optimal.lowerlimit.jpg")
      ggsave(p.flowrange, filename=file.name, dpi=300, height=5, width=7)
      
      
    }
    
    
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
    
    
    #50% reduction plot with smooth curve
    urban0.p <- p2 +
      geom_ribbon(data = urban0, aes(ymin=lower.bound0, ymax=upper.bound0), alpha=0.2, fill = "red") +
      geom_line(data = urban0, mapping = aes(x=seasonal.wrp.Q, y = p50),color="#d73027", linetype="twodash", lwd=1) 
    
    #save
    file.name2 <- paste0(out.dir.bmp, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_0Urbanflow.jpg")
    ggsave(urban0.p, filename=file.name2, dpi=300, height=5, width=5)
    
    
    #100% reduction plot with smooth curve
    urban50.p <- urban0.p +
      geom_ribbon(data = urban50, aes(ymin=lower.bound50, ymax=upper.bound50), alpha=0.5, fill = "#fee090") +
      geom_line(data = urban50, mapping = aes(x=seasonal.wrp.Q, y = p50),color="#fc8d59", linetype="twodash", lwd=1) 
    
    #save
    file.name2 <- paste0(out.dir.bmp, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_50Urbanflow.jpg")
    ggsave(urban50.p, filename=file.name2, dpi=300, height=5, width=5)
    
    


    
    
    
    
    
    #summary of scenarios - % reduction in metric from baseline for p50
    #p50 fit and find values 
    fit.p50 <- lm(p50 ~ seasonal.wrp.Q, data = ffm.sub.metric.j)
    #predict metric for a given WRP discharge
    #data.frame of predictions
    seasonal.wrp.Q <- c(baseline.metric/2, 0)
    scenario <- c("50% reduction WRP", "0 cfs WRP")
    test <- data.frame(cbind(as.numeric(seasonal.wrp.Q), scenario))
    wrp.50pct.100pct <- predict(fit.p50, newdata = test)
    baseline.50 <- max(ffm.sub.metric.j$p50)
    #find %change from baseline
    pct.change <- (baseline.50-wrp.50pct.100pct)/baseline.50 *100
    cfs.change <- baseline.50 - wrp.50pct.100pct
    #find change in no urban baseflow scenario
    #p50 fit and find values
    fit.p50.nourban <- lm(p50 ~ seasonal.wrp.Q, data = ffm.sub.metric.j.urban)
    urban.predict <- predict(fit.p50.nourban, newdata = test)
    #find %change from baseline
    pct.change.urban <- (baseline.50-urban.predict)/baseline.50 *100
    cfs.change.urban <- baseline.50 - urban.predict
    baselinewrp.nourban <- max(ffm.sub.metric.j.urban$p50)
    pct.change.baselinewrp.nourban <- (baseline.50-baselinewrp.nourban)/baseline.50 *100
    ###
    

    
    # p <- ggplot(data.plot, aes(x=Avg_Q_cfs, y = Value, color = Percentile)) +
    #   geom_point() +
    #   labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
    #        color = "Legend") + ylab(metric.title) +
    #   xlab("Average Annual WRP Discharge (cfs)") +
    #   scale_color_manual(values = colors, labels = labels, name="Water Year Type") +
    #   theme_bw() +
    #   geom_vline(xintercept=70.9, linetype="dashed", color = "black") +
    #   geom_text(aes(x= 69, y = 25, label = "Baseline WRP", angle=90), color = "black")+
    #   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    #         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    # #print plot
    # print(p)
    # #save
    # file.name <- paste0(out.dir, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_allWYT.jpg")
    # ggsave(p, filename=file.name, dpi=300, height=5, width=7)
    # 
    # #DRY plot only: take out wet and moderate and only show dry
    # p.10 <- p + 
    #   geom_point(ffm.sub.metric.j, mapping = aes(x=Avg_Q_cfs, y = p90), color = "white") +
    #   geom_point(ffm.sub.metric.j, mapping = aes(x=Avg_Q_cfs, y = p50), color = "white") 
    # #save
    # file.name <- paste0(out.dir, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_dryp10.jpg")
    # ggsave(p.10, filename=file.name, dpi=300, height=5, width=7)
    # 
    # #Wet plot only: take out dry and moderate and only show wet
    # p.90 <- p + 
    #   geom_point(ffm.sub.metric.j, mapping = aes(x=Avg_Q_cfs, y = p10), color = "white") +
    #   geom_point(ffm.sub.metric.j, mapping = aes(x=Avg_Q_cfs, y = p50), color = "white") 
    # #save
    # file.name <- paste0(out.dir, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_wetp90.jpg")
    # ggsave(p.90, filename=file.name, dpi=300, height=5, width=7)
    
    #change from baseline to 0 WRP
    dry.change <- (min(ffm.sub.metric.j$p10)-max(ffm.sub.metric.j$p10))/max(ffm.sub.metric.j$p10)*100
    med.change <- (min(ffm.sub.metric.j$p50)-max(ffm.sub.metric.j$p50))/max(ffm.sub.metric.j$p50)*100
    wet.change <- (min(ffm.sub.metric.j$p90)-max(ffm.sub.metric.j$p90))/max(ffm.sub.metric.j$p90)*100
    
    ####Create point sensitivity curve with urban baseflow removal scenarios only - Wet year
    #Subset WET 50% reduction urban
    wet.urbn50 <- data.plot.urban.merge2.urbnonly %>% 
      filter(UrbanBaseflow_pctQ == "UBF50") %>% 
      filter(Percentile == "p90")
    #Subset WET 100% reduction urban
    wet.urbn0 <- data.plot.urban.merge2.urbnonly %>% 
      filter(UrbanBaseflow_pctQ == "UBF0") %>% 
      filter(Percentile == "p90")
    
    p.wet2 <- p.90 +
      geom_line(wet.urbn50,  mapping = aes(x=Avg_Q_cfs, y = Value), color = "white", size=1, lty="F1") +
      geom_line(wet.urbn0,  mapping = aes(x=Avg_Q_cfs, y = Value), color = "white", size=1, lty="F1") +
      labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
           color = "Legend") + ylab(metric.title) +
      xlab("Average Annual WRP Discharge (cfs)") +
      scale_shape_manual(values = c("100% Urban Baseflow Removal"=2, "50% Urban Baseflow Removal"=1, "WRP"=20), name="Scenario Type") +
      scale_size_manual(values = c("100% Urban Baseflow Removal"=4, "50% Urban Baseflow Removal"=4, "WRP"=2), name="Scenario Type") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    #save
    file.name <- paste0(out.dir.bmp, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_wet.jpg")
    ggsave(p.wet2, filename=file.name, dpi=300, height=5, width=7)
    
    
    p.wet.urbn50 <- p.90 +
      geom_line(wet.urbn50,  mapping = aes(x=Avg_Q_cfs, y = Value), color = "blue", size=1, lty="F1") +
      geom_line(wet.urbn0,  mapping = aes(x=Avg_Q_cfs, y = Value), color = "white", size=1, lty="F1") +
      labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
           color = "Legend") + ylab(metric.title) +
      xlab("Average Annual WRP Discharge (cfs)") +
      scale_shape_manual(values = c("100% Urban Baseflow Removal"=2, "50% Urban Baseflow Removal"=1, "WRP"=20), name="Scenario Type") +
      scale_size_manual(values = c("100% Urban Baseflow Removal"=4, "50% Urban Baseflow Removal"=4, "WRP"=2), name="Scenario Type") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    #save
    file.name <- paste0(out.dir.bmp, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_urban50_wet.jpg")
    ggsave(p.wet.urbn50, filename=file.name, dpi=300, height=5, width=7)
    
    p.wet.urbn50.0 <- p.90 +
      geom_line(wet.urbn50,  mapping = aes(x=Avg_Q_cfs, y = Value), color = "grey", size=1, lty="F1") +
      geom_line(wet.urbn0,  mapping = aes(x=Avg_Q_cfs, y = Value), color = "blue", size=1, lty="F1") +
      labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
           color = "Legend") + ylab(metric.title) +
      xlab("Average Annual WRP Discharge (cfs)") +
      scale_shape_manual(values = c("100% Urban Baseflow Removal"=2, "50% Urban Baseflow Removal"=1, "WRP"=20), name="Scenario Type") +
      scale_size_manual(values = c("100% Urban Baseflow Removal"=4, "50% Urban Baseflow Removal"=4, "WRP"=2), name="Scenario Type") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    #save
    file.name <- paste0(out.dir.bmp, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_urban50_0_wet.jpg")
    ggsave(p.wet.urbn50.0, filename=file.name, dpi=300, height=5, width=7)
    
    p.all.urbn <- p +
      geom_line(wet.urbn50,  mapping = aes(x=Avg_Q_cfs, y = Value), color = "grey", size=1, lty="F1") +
      geom_line(wet.urbn0,  mapping = aes(x=Avg_Q_cfs, y = Value), color = "blue", size=1, lty="F1") +
      labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
           color = "Legend") + ylab(metric.title) +
      xlab("Average Annual WRP Discharge (cfs)") +
      scale_shape_manual(values = c("100% Urban Baseflow Removal"=2, "50% Urban Baseflow Removal"=1, "WRP"=20), name="Scenario Type") +
      scale_size_manual(values = c("100% Urban Baseflow Removal"=4, "50% Urban Baseflow Removal"=4, "WRP"=2), name="Scenario Type") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    #save
    file.name <- paste0(out.dir.bmp, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_urban50_0_allWYT.jpg")
    ggsave(p.all.urbn, filename=file.name, dpi=300, height=5, width=7)
    
    
    
    
    
    

    ####################################
    ########ADD in flow ranges for each node and wet or dry season baseflow plot if flow ranges are available for that node
    
    #lower limits for existing BU, med probability
    if(length(existing.ranges.metric.j.med$Probability_Threshold) > 0){
      #Exising BU designations low flow limit overliad on sensitivity plot
      existing.p.med <- p +
        #Add in species thresholds
        geom_hline(yintercept=as.numeric(existing.ranges.metric.j.med$Lower_Limit), linetype="twodash", color = "blue") +
        annotate(geom = "text", x = 30, y = as.numeric(existing.ranges.metric.j.med$Lower_Limit), label = existing.ranges.metric.j.med$Species_Label) 
      
      #save this plot for existing supported species
      file.name4 <- paste0(out.dir, unique.nodes[i], "_", metric.info$metric, "_WRPCurve_existingspp_med.jpg")
      ggsave(existing.p.med, filename=file.name4, dpi=300, height=5, width=7)
    }
    #lower limits for existing BU, high probability
    if(length(existing.ranges.metric.j.high$Probability_Threshold) > 0){
      #Exising BU designations low flow limit overliad on sensitivity plot
      existing.p.high <- p +
        #Add in species thresholds
        geom_hline(yintercept=as.numeric(existing.ranges.metric.j.high$Lower_Limit), linetype="twodash", color = "blue") +
        annotate(geom = "text", x = 30, y = as.numeric(existing.ranges.metric.j.high$Lower_Limit), label = existing.ranges.metric.j.high$Species_Label) 
      
      #save this plot for existing supported species
      file.name5 <- paste0(out.dir, unique.nodes[i], "_", metric.info$metric, "_WRPCurve_existingspp_high.jpg")
      ggsave(existing.p.high, filename=file.name5, dpi=300, height=5, width=7)
    }
    
    #lower limits for future BU, med probability
    if(length(future.ranges.metric.j.med$Probability_Threshold) > 0){
      #Exising BU designations low flow limit overliad on sensitivity plot
      future.p.med <- p +
        #Add in species thresholds
        geom_hline(yintercept=as.numeric(future.ranges.metric.j.med$Lower_Limit), linetype="twodash", color = "blue") +
        annotate(geom = "text", x = 30, y = as.numeric(future.ranges.metric.j.med$Lower_Limit), label = future.ranges.metric.j.med$Species_Label) 
      
      #save this plot for future supported species
      file.name4 <- paste0(out.dir, unique.nodes[i], "_", metric.info$metric, "_WRPCurve_futurespp_med.jpg")
      ggsave(future.p.med, filename=file.name4, dpi=300, height=5, width=7)
    }
    #lower limits for future BU, high probability
    if(length(future.ranges.metric.j.high$Probability_Threshold) > 0){
      #Exising BU designations low flow limit overliad on sensitivity plot
      future.p.high <- p +
        #Add in species thresholds
        geom_hline(yintercept=as.numeric(future.ranges.metric.j.high$Lower_Limit), linetype="twodash", color = "blue") +
        annotate(geom = "text", x = 30, y = as.numeric(future.ranges.metric.j.high$Lower_Limit), label = future.ranges.metric.j.high$Species_Label) 
      
      #save this plot for future supported species
      file.name5 <- paste0(out.dir, unique.nodes[i], "_", metric.info$metric, "_WRPCurve_futurespp_high.jpg")
      ggsave(future.p.high, filename=file.name5, dpi=300, height=5, width=7)
    }
  
  }
}




##################
###Sensitivity Curves for stormwater capture, urban drool removal, and combined - loop through all metrics and evaluate peak flows

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


