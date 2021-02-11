#WRP Scenarios Sensitivity Curves for FFMs
#plots

#other packages
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")


#Read in FFM percentiles from scenarios
ffm.all <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/FFM_percentiles_GLEN_F319wardlow_scenarios.csv")
#ffm.all <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/FFM_percentiles_allnodes_scenarios.csv")
#add in baseline FFM percentiles and combine with ffm.all
baseline.ffm <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/FFM_percentiles_reportingnodes_all.csv")
#save scenario as 0 - baseline
baseline.ffm$Scenario <- 0
#add rows to ffm.all
ffm.all <- add_row(ffm.all, baseline.ffm)

#WRP scenario labels
iterations <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/iterations_labeled.csv")
#find max av_q_cfs and half of that to get WRP100 and WRP50
WRP100_Qcfs <- 70.9
WRP50_Qcfs <- WRP100_Qcfs/2
#add in scenario 0 as 70.9 cfs
iterations[501,] <- c(0, 44.75, 8.85, 17.31, WRP100_Qcfs, 73.03)

#read in FFM percentiles from WRP scenarios with 0 urban baseflow:
ffm.urbn.capture <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_GLEN_urbanbaseflowscenario//FFM_percentiles_GLEN_baseline_WRP_0pcturbanbaseflow.csv")
#change scenario to just the number as.numeric
ffm.urbn.capture$Scenario <- as.numeric(gsub("_NoUrbanBaseflows", "", ffm.urbn.capture$Scenario))

#read in FFM percentiles from stormwater scenarios
ffm.bmp <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_StormwaterScenarios/FFM_percentiles_SUSTAIN_Junctions_StormwaterScenarios.csv")

#output directory where sensitivity curves to be saved
out.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/scenario_curves_FFM/"
#output directory for BMP curves
out.dir.bmp <- paste0(out.dir, "Stormwater_Scenario_Curves/")

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
#metrics.to.plot <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "DS_Mag_50", "DS_Mag_90")
metrics.to.plot <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "DS_Mag_50", "DS_Mag_90", "Peak_2")

#join with reporting node names and FFM labels: to get the order of the nodes (upstream to downtream) and the reach order3
ffm.all.join <- merge(ffm.all, reporting.node.names, by= "ReportingNode") %>% 
  select("X.","ReportingNode", "Description","p10","p25","p50","p75","p90","metric", "Scenario", "Reach", "order","order3","Reporting_Reach") %>% 
  merge(ffm.labels, by = "metric") %>% 
  merge(iterations, by = "Scenario") %>% 
  mutate(ScenarioType = "WRP")

#join with reporting node names and FFM labels: to get the order of the nodes (upstream to downtream) and the reach order3
ffm.all.join.urban <- merge(ffm.urbn.capture, reporting.node.names, by= "ReportingNode") %>% 
  select("X.","ReportingNode", "Description","p10","p25","p50","p75","p90","metric", "Scenario", "Reach", "order","order3","Reporting_Reach") %>% 
  merge(ffm.labels, by = "metric") %>% 
  merge(iterations, by = "Scenario") %>% 
  mutate(ScenarioType = "Urban Capture")


#join with stormwater scenarios reporting node names and FFM labels: to get the order of the nodes (upstream to downtream) and the reach order3
ffm.all.join.bmp <- merge(ffm.bmp, reporting.node.names, by= "ReportingNode") %>% 
  select("X.","ReportingNode", "Description","p10","p25","p50","p75","p90","metric", "Scenario", "Reach", "order","order3","Reporting_Reach") %>% 
  merge(ffm.labels, by = "metric") %>% 
  mutate(ScenarioType = "Stormwater + WRP")

#add in Avg_Q_cfs which is for WRP0, 50, 100
ffm.all.join.bmp$Avg_Q_cfs <- 0
#find ind WRP50 and WRP100 scenarios and put i appropriate values
ind.WRP50 <- grep("WRP50", ffm.all.join.bmp$Scenario)
ffm.all.join.bmp$Avg_Q_cfs[ind.WRP50] <- WRP50_Qcfs
#WRP 100
ind.WRP100 <- grep("WRP100", ffm.all.join.bmp$Scenario)
ffm.all.join.bmp$Avg_Q_cfs[ind.WRP100] <- WRP100_Qcfs


#loop through the scenarios to plot percentiles scenarios curve
unique.nodes <- unique(ffm.all.join$ReportingNode)
#only run for GLEN
i <- grep("GLEN", unique.nodes)


#for(i in 1:length(unique.nodes)){
  #subset WRP to node i
  ffm.sub <- ffm.all.join[ffm.all.join$ReportingNode == unique.nodes[i],]
  
  #subset WRP to node i
  ffm.sub.urban <- ffm.all.join.urban[ffm.all.join.urban$ReportingNode == unique.nodes[i],]
  
  #subset bmps scenarios to node i
  ffm.sub.bmp <- ffm.all.join.bmp[ffm.all.join.bmp$ReportingNode == unique.nodes[i],]
  
  #loop to create sensitivity curves for each metric
  for(j in 1:length(metrics.to.plot)){
    #subset WRP to metric j
    ffm.sub.metric.j <-  ffm.sub[ffm.sub$metric == metrics.to.plot[j],]
    
    #subset WRP to metric j
    ffm.sub.metric.j.urban <-  ffm.sub.urban[ffm.sub.urban$metric == metrics.to.plot[j],]
    
    #subset bmps to metric j
    ffm.sub.metric.j.bmp <-  ffm.sub.bmp[ffm.sub.bmp$metric == metrics.to.plot[j],]
    
    ####################################
    ####POINT sensitivity Curves, plotting raw flow metric data for p10-p90
    #set colors for point curve
    colors <- c("#0571b0", "#fdae61",  "#ca0020")
    Percentile <- c("p90", "p50", "p10")
    labels <- factor(c("Wet", "Moderate", "Dry"), levels = c("Wet", "Moderate", "Dry"))
    lookup <- data.frame(cbind(colors, Percentile, labels))
    lookup$Percentile <- factor(Percentile, levels = Percentile)
    
    #get metric info
    metric.info <- ffm.labels[ffm.labels$flow_metric == metrics.to.plot[j],]
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
    
    ###create sensitivity curve for WRP only
    p <- ggplot(data.plot, aes(x=Avg_Q_cfs, y = Value, color = Percentile)) +
      geom_point() +
      labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
           color = "Legend") + ylab(metric.title) +
      xlab("Average Annual WRP Discharge (cfs)") +
      scale_color_manual(values = colors, labels = labels, name="Water Year Type") +
      theme_bw() +
      geom_vline(xintercept=70.9, linetype="dashed", color = "black") +
      geom_text(aes(x= 69, y = 25, label = "Baseline WRP", angle=90), color = "black")+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    #print plot
    print(p)
    #save
    file.name <- paste0(out.dir, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve.jpg")
    ggsave(p, filename=file.name, dpi=300, height=5, width=7)
    
    #take out wet and moderate and only show dry
    p.10 <- p + 
      geom_point(ffm.sub.metric.j, mapping = aes(x=Avg_Q_cfs, y = p90), color = "white") +
      geom_point(ffm.sub.metric.j, mapping = aes(x=Avg_Q_cfs, y = p50), color = "white") 
    #save
    file.name <- paste0(out.dir, "points_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_dryp10.jpg")
    ggsave(p.10, filename=file.name, dpi=300, height=5, width=7)
    
    #change from baseline to 0 WRP
    dry.change <- (min(ffm.sub.metric.j$p10)-max(ffm.sub.metric.j$p10))/max(ffm.sub.metric.j$p10)*100
    med.change <- (min(ffm.sub.metric.j$p50)-max(ffm.sub.metric.j$p50))/max(ffm.sub.metric.j$p50)*100
    wet.change <- (min(ffm.sub.metric.j$p90)-max(ffm.sub.metric.j$p90))/max(ffm.sub.metric.j$p90)*100
    
    ####Create point sensitivity curve with WRP and stormwater combined
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
    
    ####################################
    #####SMOOTH SENSITIVITY CURVES:
    #Plot as a smooth curve with shaded band (linear reg for p50 with the upper bound being p90 and lower p10) 
    
    #upper bound (p90)
    #fit p90
    fit.p90 <- lm(p90 ~ Avg_Q_cfs, data = ffm.sub.metric.j)
    summary(fit.p90)
    #predict values 
    upper.bound <- predict(fit.p90, newdata = ffm.sub.metric.j)
    #lower bound (p10)
    #fit p10
    fit.p10 <- lm(p10 ~ Avg_Q_cfs, data = ffm.sub.metric.j)
    summary(fit.p10)
    #predict values 
    lower.bound <- predict(fit.p10, newdata = ffm.sub.metric.j)
    
    #plot with stat smoth
    p2 <- ggplot(ffm.sub.metric.j, aes(x=Avg_Q_cfs, y = p50)) +
      #geom_point() +
      geom_smooth(method = "lm", color="black") +
      geom_ribbon(aes(ymin=lower.bound, ymax=upper.bound), alpha=0.2) +
      labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
      color = "Legend") + ylab(metric.title) +
      #geom_vline(xintercept=70.9, linetype="dashed", color = "black") +
      #geom_text(aes(x= 69, y = 25, label = "Baseline WRP", angle=90), color = "black")+
      xlab("Average Annual WRP Discharge (cfs)") + 
      theme_bw() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    #save
    file.name2 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve.jpg")
    ggsave(p2, filename=file.name2, dpi=300, height=5, width=6)
    
    ###
    #summary of scenarios - % reduction in metric from baseline for p50
    #p50 fit and find values
    fit.p50 <- lm(p50 ~ Avg_Q_cfs, data = ffm.sub.metric.j)
    #predict metric for a given WRP discharge
    #data.frame of predictions
    Avg_Q_cfs <- c(35.45, 0)
    scenario <- c("50% reduction WRP", "0 cfs WRP")
    test <- data.frame(cbind(as.numeric(Avg_Q_cfs), scenario))
    wrp.50pct.100pct <- predict(fit.p50, newdata = test)
    baseline.50 <- max(ffm.sub.metric.j$p50)
    #find %change from baseline
    pct.change <- (baseline.50-wrp.50pct.100pct)/baseline.50 *100
    cfs.change <- baseline.50 - wrp.50pct.100pct
    #find change in no urban baseflow scenario
    #p50 fit and find values
    fit.p50.nourban <- lm(p50 ~ Avg_Q_cfs, data = ffm.sub.metric.j.urban)
    urban.predict <- predict(fit.p50.nourban, newdata = test)
    #find %change from baseline
    pct.change.urban <- (baseline.50-urban.predict)/baseline.50 *100
    cfs.change.urban <- baseline.50 - urban.predict
    baselinewrp.nourban <- max(ffm.sub.metric.j.urban$p50)
    pct.change.baselinewrp.nourban <- (baseline.50-baselinewrp.nourban)/baseline.50 *100
    ###
    
    #Smooth Curve: add in urban baseflow removal scenario to plot
    #upper bands for urban removal scenario
    #upper bound (p90)
    #fit p90
    fit.p90.urban <- lm(p90 ~ Avg_Q_cfs, data = ffm.sub.metric.j.urban)
    summary(fit.p90.urban)
    #predict values 
    upper.bound.urban <- predict(fit.p90.urban, newdata = ffm.sub.metric.j.urban)
    #lower bound (p10)
    #fit p10
    fit.p10.urban <- lm(p10 ~ Avg_Q_cfs, data = ffm.sub.metric.j.urban)
    summary(fit.p10.urban)
    #predict values 
    lower.bound.urban <- predict(fit.p10.urban, newdata = ffm.sub.metric.j.urban)
    
    #plot with WRP curve
    p2.urban.removal <- p2 +
      geom_smooth(data = ffm.sub.metric.j.urban, mapping = aes(x=Avg_Q_cfs, y = p50), method = "lm", color="#238b45", linetype="twodash") +
      geom_ribbon(aes(ymin=lower.bound.urban, ymax=upper.bound.urban), alpha=0.2, fill = "#238b45")
    p2.urban.removal
    #save
    #save
    file.name2 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_0Urbanflow.jpg")
    ggsave(p2.urban.removal, filename=file.name2, dpi=300, height=5, width=6)
    #will add in willow lines below

    
    ########ADD in species thresholds for each example node and wet or dry season baseflow plot
    
    #####GLENDALE
    ##if GLEN and dry season baseflow mag, add in species lines
    if(unique.nodes[i] == "GLEN" & metric.info$flow_component == "Dry-season baseflow"){
      #for GLEN 
      #add in shaded thresholds for typha
      p3 <- p2 +
        #geom_ribbon(aes(ymin=80, ymax=270), alpha=0.2, color="blue") +
        #Add in species thresholds
        geom_hline(yintercept=80, linetype="dashed", color = "blue") +
        annotate(geom = "text", x = 40, y = 83, label = "Typha Adult - Lower Limit") +
        geom_hline(yintercept=89.9, linetype="dashed", color = "blue") +
        annotate(geom = "text", x = 40, y = 92, label = "Typha Growth - Upper Limit") +
        geom_hline(yintercept=22, linetype="dashed", color = "red") +
        annotate(geom = "text", x = 40, y = 19.5, label = "Willow Growth - Lower Limit") 
      #save this plot for existing supported species
      file.name3 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_existing_spp.jpg")
      ggsave(p3, filename=file.name3, dpi=300, height=5, width=6)
      
      #typha alone
      p3.typha <- p2 +
        #geom_ribbon(aes(ymin=80, ymax=270), alpha=0.2, color="blue") +
        #Add in species thresholds
        geom_hline(yintercept=80, linetype="dashed", color = "blue") +
        annotate(geom = "text", x = 40, y = 83, label = "Typha Adult - Lower Limit") +
        geom_hline(yintercept=89.9, linetype="dashed", color = "blue") +
        annotate(geom = "text", x = 40, y = 92, label = "Typha Growth - Upper Limit")
      #save this plot for existing supported species
      file.name3.typha <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_existing_spp_typha.jpg")
      ggsave(p3.typha, filename=file.name3.typha, dpi=300, height=5, width=6)
      
      #add in urban baseflow removal with willow lines
      p3.urban.willow <- p2.urban.removal +
        geom_hline(yintercept=22, linetype="dashed", color = "red") +
        annotate(geom = "text", x = 40, y = 19.5, label = "Willow Growth - Lower Limit") 
      #save
      file.name2 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_0Urbanflow_willow.jpg")
      ggsave(p3.urban.willow, filename=file.name2, dpi=300, height=5, width=6)
      
      
      #add in shaded thresholds for depth - future species, high
      p4 <- p2 +
        #geom_ribbon(aes(ymin=80, ymax=270), alpha=0.2, color="blue") +
        #Add in species thresholds
        #future:
        geom_hline(yintercept=22.98, linetype="dashed", color = "green") +
        annotate(geom = "text", x = 45, y = 21, label = "SAS Fry - Upper Limit") +
        geom_hline(yintercept=35.08, linetype="dashed", color = "green") +
        annotate(geom = "text", x = 45, y = 38, label = "SAS Juvenile - Lower Limit") +
        geom_hline(yintercept=99.67, linetype="dashed", color = "green") +
        annotate(geom = "text", x = 45, y = 102, label = "SAS Adult - Lower (High P)") +
        geom_hline(yintercept=23.48, linetype="dashed", color = "green") +
        annotate(geom = "text", x = 45, y = 26, label = "SAS Adult - Lower (Medium & Low P)") +
        geom_hline(yintercept=22.98, linetype="dashed", color = "green") 
        #annotate(geom = "text", x = 45, y = 25, label = "SAS Adult - Lower (Low P)") +
        
      #save this plot for existing supported species
      file.name4 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_future_spp.jpg")
      ggsave(p4, filename=file.name4, dpi=300, height=5, width=6)
    }
    
    ##if GLEN and wet season baseflow mag, add in species lines
    if(unique.nodes[i] == "GLEN" & metric.info$flow_component == "Wet-season baseflow"){
      #for GLEN 
      #add in shaded thresholds for depth - existing species, high
      p3 <- p2 +
        #geom_ribbon(aes(ymin=80, ymax=270), alpha=0.2, color="blue") +
        #Add in species thresholds
        geom_hline(yintercept=80, linetype="dashed", color = "blue") +
        annotate(geom = "text", x = 40, y = 83, label = "Typha Adult - Lower Limit")
      #save this plot for existing supported species
      file.name3 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_existing_spp.jpg")
      ggsave(p3, filename=file.name3, dpi=300, height=5, width=6)
      
      #add in shaded thresholds for depth - future species, high
      p4 <- p2 +
        #geom_ribbon(aes(ymin=80, ymax=270), alpha=0.2, color="blue") +
        #Add in species thresholds
        #current:
        geom_hline(yintercept=80, linetype="dashed", color = "blue") +
        annotate(geom = "text", x = 40, y = 83, label = "Typha Adult - Lower Limit") +
        #future:
        geom_hline(yintercept=22.98, linetype="dashed", color = "grey") +
        annotate(geom = "text", x = 45, y = 21, label = "SAS Fry - Upper Limit") +
        geom_hline(yintercept=99.67, linetype="dashed", color = "grey") +
        annotate(geom = "text", x = 45, y = 102, label = "SAS Adult - Lower (High P)") +
        geom_hline(yintercept=23.48, linetype="dashed", color = "grey") +
        annotate(geom = "text", x = 45, y = 26, label = "SAS Adult - Lower (Medium & Low P)") +
        geom_hline(yintercept=22.98, linetype="dashed", color = "grey") +
        geom_hline(yintercept=22.98, linetype="dashed", color = "orange") +
        annotate(geom = "text", x = 45, y = 30, label = "Stealhead Migration - Lower Limit")
      #annotate(geom = "text", x = 45, y = 25, label = "SAS Adult - Lower (Low P)") +
      
      #save this plot for existing supported species
      file.name4 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_future_spp.jpg")
      ggsave(p4, filename=file.name4, dpi=300, height=5, width=6)
    }
    
    #####F319 Wardlow
    ##if F319 and dry season baseflow mag, add in species lines
    if(unique.nodes[i] == "F319" & metric.info$flow_component == "Dry-season baseflow"){
      #for F319 
      #add in shaded thresholds for depth - existing species, high
      p3 <- p2 +
        #geom_ribbon(aes(ymin=80, ymax=270), alpha=0.2, color="blue") +
        #Add in species thresholds
        geom_hline(yintercept=122.73, linetype="dashed", color = "blue") +
        annotate(geom = "text", x = 25, y = 125.73, label = "Cladophora Adult - Lower Limit") 
      #save this plot for existing supported species
      file.name3 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_existing_spp.jpg")
      ggsave(p3, filename=file.name3, dpi=300, height=5, width=6)
      
    #No thresholds for future species, only relates to wet season baseflow
    }
    
    ##if F319 and wet season baseflow mag, add in species lines
    if(unique.nodes[i] == "F319" & metric.info$flow_component == "Wet-season baseflow"){
      #for F319 
      #No thresholds for existing species in wet season baseflow, only future

      #add in shaded thresholds for depth - future species, high
      p4 <- p2 +
        #geom_ribbon(aes(ymin=80, ymax=270), alpha=0.2, color="blue") +
        #Add in species thresholds
        #future:
        geom_hline(yintercept=62.58, linetype="dashed", color = "orange") +
        annotate(geom = "text", x = 45, y = 65.58, label = "Stealhead Migration & Smolts - Lower Limit")

      #save this plot for existing supported species
      file.name4 <- paste0(out.dir, "smoothcurve_", unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_future_spp.jpg")
      ggsave(p4, filename=file.name4, dpi=300, height=5, width=6)
    }
    
  }
  
}




##################
###Sensitivity Curves for stormwater capture - loop through all metrics and evaluate peak flows

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


