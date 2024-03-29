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

# #read in flow ranges for specific reporting nodes and species-lifestage
# ranges <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/MM_March02_Typha_Steelhead_Cladophora_SAS_Willow_02_05_2021_updated_KI_MM.csv")
# #find unique nodes where sensitivity curves should be generated
# node.ranges <- unique(ranges$Node)

#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric
#change all the units of cfs to cms in ffm.labels df
ffm.labels[] <- lapply(ffm.labels, function(x) gsub("cfs", "cms", x))
#unique flow metrics 
unique.metrics <- unique(ffm.labels$metric)
#subset to only mag and wet and dry 
metrics.to.plot <- c("Wet_BFL_Mag_10", "DS_Mag_50") #, "Wet_BFL_Mag_50", "DS_Mag_90")
#metrics.to.plot <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "DS_Mag_50", "DS_Mag_90", "Peak_2")

#Read in urban baseflow curves, Q predicted = Const + X.Coeff * (relevant WRP Q). for Q in cfs
ubf.all <- read.csv("UBFRatingCurves_StormdrainReduction.csv") 

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

#filter to only GLEN and LA3 to make processing faster
ffm.all <- ffm.all[ffm.all$ReportingNode == "GLEN" | ffm.all$ReportingNode == "LA3",]
#convert all mag metrics from cfs to cms
#create list of magnitude metrics that need to be converted
mag.metrics <- ffm.labels$metric[grep("cms",ffm.labels$flow_characteristic)]

#make copy to update from cfs to cms for mag metrics
ffm.all2 <- ffm.all

#convert all percentiles of magnitude metrics to cms
for(i in 1:length(ffm.all2$p10)){
  sub <- ffm.all2[i,]
  if(length(which(sub$metric == mag.metrics)) > 0){
    #if mag metric, convert percentiles to cms
    sub$p10 <- sub$p10*0.028316846592
    sub$p25 <- sub$p25*0.028316846592
    sub$p50 <- sub$p50*0.028316846592
    sub$p75 <- sub$p75*0.028316846592
    sub$p90 <- sub$p90*0.028316846592
    
    ffm.all2[i,] <- sub
  }
}


#read in WRP scenario labels with seasonal WRP  and various values for each reporting node based on which WRP discharges to it
iterations <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/manuscripts/sensitivitycurves_FFMs/01_data/summary_seasonalWRP_node.csv") %>% 
  rename(dry_season=dry, wet_season=wet, spring=spr)
#add in baseline iterations
baseline.iterations <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/manuscripts/sensitivitycurves_FFMs/01_data/summary_seasonalWRP_node_baselineonly.csv") %>% 
  rename(dry_season=dry, wet_season=wet, spring=spr)

#combine scenario wrp with baselin
iterations <- add_row(iterations, baseline.iterations)
#create copy of iterations cfs
iterations.cfs <- iterations

#change all the units of cfs to cms in ffm.labels df
names(iterations) <-gsub("cfs", "cms", names(iterations))
#convert all WRP Q to cms
for(i in 2:4){
  #for col i convert to cms
  sub <- as.numeric(iterations[,i])*0.028316846592
  #save back into scenario.WRP
  iterations[,i] <- sub
}


#output directory where sensitivity curves to be saved
out.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/manuscripts/sensitivitycurves_FFMs/02_figures/"
dir.create(out.dir)

#Reporting node description file
reporting.node.names <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- rename(reporting.node.names, ReportingNode = SWMM.Node )



#join WRP with reporting node names and FFM labels: to get the order of the nodes (upstream to downtream) and the reach order3
#WRP percentiles join
ffm.all.join <- merge(ffm.all2, reporting.node.names, by= "ReportingNode") %>% 
  select("X.","ReportingNode", "Description","p10","p25","p50","p75","p90","metric", "Scenario", "Reach", "order","order3","Reporting_Reach") %>% 
  merge(ffm.labels, by = "metric") %>% 
  #merge(iterations, by = "Scenario") %>% 
  mutate(ScenarioType = "WRP") %>% 
  mutate(ScenarioType2 = "WRP")


#output dataframes for WRP discharge associated with lower limit intersections of sensitivity curves
wrp.lowerlimits <- data.frame(matrix(NA, nrow=1, ncol=5))
names(wrp.lowerlimits) <- c("limit1", "limit2", "limit1.2.names","node", "metric")

#loop through the scenarios to plot percentiles scenarios curve
#unique.nodes
#only plot GLEN and LA3
unique.nodes <- c("GLEN", "LA3")

for(i in 1:length(unique.nodes)){
  
  # #subset flow ranges to node i
  # ranges.sub <- ranges[ranges$Node == unique.nodes[i],]
  
  #subset urban baseflow reduction equations
  ubf.all.sub <- ubf.all[ubf.all$ReportingNode == unique.nodes[i],]

  #subset percentiles
  #subset WRP to node i
  ffm.sub <- ffm.all.join[ffm.all.join$ReportingNode == unique.nodes[i],]

  #add in scenario WRP Q data for node i
  #subset WRP iterations for node i
  iterations.node <- iterations[iterations$ReportingNode == unique.nodes[i],] %>% 
    select(Scenario, dry_season, wet_season, spring)
  #WRP iterations merge with ffm.sub
  ffm.sub <- ffm.sub %>% 
    merge(iterations.node, by="Scenario")
  
  #find baseline dry, wet, spring for that node
  baselines <- iterations.node[iterations.node$Scenario == 0,]
  
  
  #loop to create sensitivity curves for each metric
  for(j in 1:length(metrics.to.plot)){
    
    #subset percentiles
    #subset WRP to metric j
    ffm.sub.metric.j <-  ffm.sub[ffm.sub$metric == metrics.to.plot[j],]
    #subset urban baseflow scenarios curves to metric j
    ubf.all.sub.metric <-  ubf.all.sub[ubf.all.sub$Metric == metrics.to.plot[j],]
    
    
    
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
    

    #if dry season metric, set x to dry season wrp
    dry.ind <- grep("Dry", metric.info$title_name)
    if(length(dry.ind) > 0){
      #rename dry season WRP to generic x name
      data.plot <- data.plot %>% 
        rename(seasonal.wrp.Q = dry_season)
      ffm.sub.metric.j <- ffm.sub.metric.j %>% 
        rename(seasonal.wrp.Q = dry_season)
      #ffm.sub.metric.j.urban <- ffm.sub.metric.j.urban %>% 
        #rename(seasonal.wrp.Q = dry_season)
      #data.plot.urban.merge2.urbnonly <- data.plot.urban.merge2.urbnonly %>% 
        #rename(seasonal.wrp.Q = dry_season)
      
      
      #x axis label for seasonal wrp
      x.axis <- "Average Annual Dry-Season WRP Discharge (cms)"
      baseline.metric <- baselines$dry_season
      #y max needs to be max of wetseason baseflow (same scale)
      #subset to 
      #subset WRP to Wet_BFL_Mag_10
      ffm.sub.wet <-  ffm.sub[ffm.sub$metric == "Wet_BFL_Mag_10",]
      #find max from wet season baseflow to use same scale on both plots
      y.max.WRP <- max(ffm.sub.wet$p90)
      
    }else{
      #rename wet season WRP to generic x name 
      #wrp data
      data.plot <- data.plot %>% 
        rename(seasonal.wrp.Q = wet_season)
      ffm.sub.metric.j <- ffm.sub.metric.j %>% 
        rename(seasonal.wrp.Q = wet_season)
      #ffm.sub.metric.j.urban <- ffm.sub.metric.j.urban %>% 
        #rename(seasonal.wrp.Q = wet_season)
      #data.plot.urban.merge2.urbnonly <- data.plot.urban.merge2.urbnonly %>% 
        #rename(seasonal.wrp.Q = wet_season)
      
      
      #x axis label for seasonal wrp
      x.axis <- "Average Annual Wet-Season WRP Discharge (cms)"
      baseline.metric <- baselines$wet_season
      #find max from wet season baseflow to use same y scale on both plots
      y.max.WRP <- max(ffm.sub.metric.j$p90)
      
    }
    

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
    #find the intersection of lower limits with the p50 curve
    #fit p50
    fit.p50 <- lm(p50 ~ seasonal.wrp.Q, data = ffm.sub.metric.j)
    summary(fit.p50)
    #predict values 
    #predict values from 0 to 100 in case lower limit is outside of the baseline WRP discharge of 2 cms
    predict.wrp <- data.frame(list(0:100))
    names(predict.wrp) <- "seasonal.wrp.Q"
    p50.predict <- predict(fit.p50, newdata = predict.wrp)
    
    #find values for urban baseflow removal scenarios UBF0 and UBF50: Q predicted = Const + X.Coeff * (relevant WRP Q).
    seasonal.wrp.Q.cfs <- ffm.sub.metric.j$seasonal.wrp.Q/0.028316846592
    #50% reduction p10 curve
    index.UBF50.p10 <- which(ubf.all.sub.metric$UBF == "UBF50" & ubf.all.sub.metric$Percentile == "p10")
    UBF50.p10 <-  (ubf.all.sub.metric$Const[index.UBF50.p10] + (ubf.all.sub.metric$X.Coeff[index.UBF50.p10] * seasonal.wrp.Q.cfs)) * 0.028316846592 #convert back to cms
    #50% reduction p50 curve
    index.UBF50.p50 <- which(ubf.all.sub.metric$UBF == "UBF50" & ubf.all.sub.metric$Percentile == "p50")
    UBF50.p50 <-  (ubf.all.sub.metric$Const[index.UBF50.p50] + (ubf.all.sub.metric$X.Coeff[index.UBF50.p50] * seasonal.wrp.Q.cfs)) * 0.028316846592 #convert back to cms
    #50% reduction p90 curve
    index.UBF50.p90 <- which(ubf.all.sub.metric$UBF == "UBF50" & ubf.all.sub.metric$Percentile == "p90")
    UBF50.p90 <-  (ubf.all.sub.metric$Const[index.UBF50.p90] + (ubf.all.sub.metric$X.Coeff[index.UBF50.p90] * seasonal.wrp.Q.cfs)) * 0.028316846592 #convert back to cms
    #100% reduction p10 curve
    index.UBF0.p10 <- which(ubf.all.sub.metric$UBF == "UBF0" & ubf.all.sub.metric$Percentile == "p10")
    UBF0.p10 <-  (ubf.all.sub.metric$Const[index.UBF0.p10] + (ubf.all.sub.metric$X.Coeff[index.UBF0.p10] * seasonal.wrp.Q.cfs)) * 0.028316846592 #convert back to cms
    #100% reduction p50 curve
    index.UBF0.p50 <- which(ubf.all.sub.metric$UBF == "UBF0" & ubf.all.sub.metric$Percentile == "p50")
    UBF0.p50 <-  (ubf.all.sub.metric$Const[index.UBF0.p50] + (ubf.all.sub.metric$X.Coeff[index.UBF0.p50] * seasonal.wrp.Q.cfs)) * 0.028316846592 #convert back to cms
    #100% reduction p90 curve
    index.UBF0.p90 <- which(ubf.all.sub.metric$UBF == "UBF0" & ubf.all.sub.metric$Percentile == "p90")
    UBF0.p90 <-  (ubf.all.sub.metric$Const[index.UBF0.p90] + (ubf.all.sub.metric$X.Coeff[index.UBF0.p90] * seasonal.wrp.Q.cfs)) * 0.028316846592 #convert back to cms
    #make dataframe for the geom_lines p50 including no reductions
    UBF50.data <- data.frame(cbind(ffm.sub.metric.j$seasonal.wrp.Q, ffm.sub.metric.j$p50, UBF50.p50, UBF0.p50))
    names(UBF50.data)[1:2]<- c("seasonal.wrp.Q", "UBF100.p50")
    #pivot longer to plot lines as diff colors and add to plot
    UBF50.data.wider <- UBF50.data %>% 
      pivot_longer(cols=c("UBF100.p50", "UBF50.p50", "UBF0.p50"), names_to = "Scenario", values_to = "Value") %>% 
      data.frame()
    #set levels and rename scenarios
    UBF50.data.wider$Scenario <- factor(UBF50.data.wider$Scenario, levels = c("UBF100.p50", "UBF50.p50", "UBF0.p50"))
    
    #plot with stat smooth WRP curve
    p2 <- ggplot(ffm.sub.metric.j, aes(x=seasonal.wrp.Q, y = p50)) +
      #geom_point() +
      #geom_smooth(method = "lm", color="black") +
      geom_ribbon(aes(ymin=lower.bound, ymax=upper.bound), alpha=0.2) +
      #add in ubf curves 100% reduction
      geom_ribbon(aes(ymin=UBF0.p10, ymax=UBF0.p90), alpha=0.2, fill="red") +
      #add in ubf curves 50% reduction
      geom_ribbon(aes(ymin=UBF50.p10, ymax=UBF50.p90), alpha=0.5, fill="#fee090") +
      #median curves for all scenarios
      geom_smooth(UBF50.data.wider, method = "lm", mapping= aes(x=seasonal.wrp.Q, y=Value, color=Scenario, linetype=Scenario)) +
      scale_colour_manual(values= c("black", "#fc8d59", "red"), labels= c("WRP Reduction", "50% Stormdrain Reduction", "100% Stormdrain Reduction"), name="") +
      scale_linetype_manual(values= c("solid", "twodash", "dotted"), guide=FALSE) +
      #guides(color=guide_legend(override.aes=list(fill=c("grey", "#fee090", "lightpink")))) +
      guides(color=guide_legend(override.aes=list(fill=c("grey", "#fee090", "lightpink"), linetype = c("solid", "twodash", "dotted")))) +
      #scale_linetype(guide = FALSE) +
      #labs(title = paste0("WRP Sensitivity Curve: ", unique.nodes[i]), subtitle=metric.info$title_name,
      #     color = "Legend") + 
      ylab(metric.title) +
      #geom_vline(xintercept=baseline.metric, linetype="dashed", color = "black") +
      #geom_text(aes(x= baseline.metric-.1, y = min(data.plot$Value)+.5, label = "Baseline WRP", angle=90), color = "black")+
      xlab(x.axis) + 
      ylim(c(NA, y.max.WRP)) +
      theme_bw() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            text = element_text(size=12))
    
    plot(p2)

    #baseline WRP p50
    baselinep50 <- max(ffm.sub.metric.j$p50)
    WRP0_p50 <- min(ffm.sub.metric.j$p50)
    change.baseline.0 <- baselinep50 - WRP0_p50
    
    ###########add in optimal flow range lower limit to curve
    #overlay optimal flow range for GLEN only
    if(unique.nodes[i] == "GLEN"){
      #smooth curve
      
      #typha limit
      typha.low.limit <- 77*0.028316846592
      willow.low.limit <- 23*0.028316846592
      p.flowrange2 <- p2 + 
        geom_hline(yintercept=typha.low.limit, color = "blue", lty="dashed") +
        annotate(geom = "text", x = 0.4, y = typha.low.limit+.1, label = "Cattail Lower Limit") +
        geom_hline(yintercept=willow.low.limit, color = "grey", lty="solid") +
        annotate(geom = "text", x = 1.4, y = willow.low.limit+.1, label = "Willow Lower Limit") +
        #baseline WRP
        geom_vline(xintercept=baseline.metric, lty="dashed") +
        annotate(geom = "text", x = baseline.metric-0.05, y = 1.25, label = "Current WRP", angle = 90)
        
      print(p.flowrange2)
      

      # Approximate seasonal WRP when wet-season baseflow is lower limits for typha and willow (intersection of flow limit with curve p50)
      predict.wrp.typha.willow <- approx(x = p50.predict, y = predict.wrp$seasonal.wrp.Q, xout = c(typha.low.limit, willow.low.limit))$y  
      #add in node and metric
      row <- c(predict.wrp.typha.willow, "Limit1: typha, Limit2: willow", unique.nodes[i], metric.title)
      #save into output df of overall wrp.lowerlimits
      wrp.lowerlimits <- rbind(wrp.lowerlimits, row)
        
      
      #if dry-season plot rec limits
      if(length(dry.ind) > 0){
        #kayaking flow limit for GLEN
        kayak.flow.limit <- 64.9*0.028316846592
        fishing.flow.limit <- 96.02036384*0.028316846592
        
        p.flowrange2 <- p.flowrange2 +
          geom_hline(yintercept=kayak.flow.limit,linetype="dotted", size=.75) +
          annotate(geom = "text", x = 0.4, y = kayak.flow.limit+.1, label = "Kayaking Lower Limit")
        
        print(p.flowrange2)
        
        # Approximate seasonal WRP when wet-season baseflow is lower limits for kayaking
        predict.wrp.kayak <- approx(x = p50.predict, y = predict.wrp$seasonal.wrp.Q, xout = kayak.flow.limit)$y  
        #add in node and metric
        row2 <- c(predict.wrp.kayak, NA,"Limit1: kayaking, Limit2: NA", unique.nodes[i], metric.title)
        #save into output df of overall wrp.lowerlimits
        wrp.lowerlimits <- rbind(wrp.lowerlimits, row2)
        
      }
        
      #save
      file.name <- paste0(out.dir, unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_lowerlimits.jpg")
      ggsave(p.flowrange2, filename=file.name, dpi=300, height=6, width=6)
    }
    
    #overlay optimal flow range for GLEN only
    if(unique.nodes[i] == "LA3"){
      #smooth curve
      
      #steelhead migration and Cladophora limit
      steelhead.low.limit <- 62*0.028316846592
      cladophora.low.limit <- 1.75
      p.flowrange2 <- p2 + 
        geom_hline(yintercept=steelhead.low.limit, color = "blue", lty="dashed") +
        annotate(geom = "text", x = 1.15, y = steelhead.low.limit+.1, label = "Steelhead Migration & Green Algae Lower Limit") +
        #baseline WRP
        geom_vline(xintercept=baseline.metric, lty="dashed") +
        #annotate(geom = "text", x = baseline.metric-0.05, y = 2.5, label = "Current WRP", angle = 90) +
        annotate(geom = "text", x = baseline.metric-0.04, y = 1, label = "Current WRP", angle = 90) +
        geom_hline(yintercept=cladophora.low.limit, color = "red", lty="dashed") 
        #annotate(geom = "text", x = 1, y = cladophora.low.limit+.1, label = "Cladophora Lower Limit")
      
      print(p.flowrange2)
      
      # Approximate seasonal WRP when wet-season baseflow is lower limits for steelhead and cladaphora (intersection of flow limit with curve p50)
      predict.wrp.steelhead.clad <- approx(x = p50.predict, y = predict.wrp$seasonal.wrp.Q, xout = c(steelhead.low.limit, cladophora.low.limit))$y  
      #add in node and metric
      row <- c(predict.wrp.steelhead.clad, "Limit1: steelhead, Limit2: cladaphora", unique.nodes[i], metric.title)
      #save into output df of overall wrp.lowerlimits
      wrp.lowerlimits <- rbind(wrp.lowerlimits, row)
      
      
      #save
      file.name <- paste0(out.dir, unique.nodes[i], "_", metric.info$metric, "_WRP_Sensitivity_Curve_lowerlimits.jpg")
      ggsave(p.flowrange2, filename=file.name, dpi=300, height=6, width=6)
    }
  }
}

#get water year day
library(EflowStats)
x <- seq(from=as.Date("2010-10-01"),to=as.Date("2011-09-30"),by="1 days")
WYD <- as.numeric(get_waterYearDay(x))
wyd.out <- data.frame(cbind(as.character(x), as.character(WYD)))
names(wyd.out) <- c("x", "WYD")
#find
wyd.out$x[wyd.out$WYD == 101]
