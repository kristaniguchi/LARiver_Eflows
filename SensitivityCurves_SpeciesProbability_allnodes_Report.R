#WRP Scenarios Species Occurrence Sensitivity Curves for FFMs - all reporting nodes using seasonal mean probability (scaled from 1-0 prob)
#Loop through all nodes and generate species-based sensitivity curves
#curves for WRP scenarios only - will add stormwater/stormdrain scenarios later
########need to subset to limiting factor for lifestage - plot bunches all of them up
#######Update: scale y axis to 1 for all plots

#other packages
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")


#read in the probability of occurrence df for all species and nodes WRP scenarios
prob.all <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/X2a_mean_probs_all_spp_best_slices_WRP_scenarios_updatedApril2021.csv") %>% 
  mutate(species.lifestage = paste0(SpeciesName, " ", LifeStageName )) %>% 
  mutate(species.lifestage.hyd = paste0(species.lifestage, " ", HydraulicName))
#convert to upper case species.lifestage
prob.all$species.lifestage <- str_to_title(prob.all$species.lifestage)
prob.all$LifeStageName <- str_to_title(prob.all$LifeStageName)
prob.all$species.lifestage.hyd <- str_to_title(prob.all$species.lifestage.hyd)

#summarize percentiles based on node, scenario, species.lifestage
prob.all2 <- prob.all %>% 
  group_by(Node, Scenario, SpeciesName, species.lifestage, species.lifestage.hyd, season) %>% 
  summarize(p90 = quantile(MeanProbability, .90),
            p50 = quantile(MeanProbability, .50),
            p10 = quantile(MeanProbability, .10)) %>% 
  ungroup() %>% 
  rename(ReportingNode = Node) %>% 
  data.frame()

#add in baseline results - this file is actually the stormwater scenario results but it includes baseline scenario #22, will filter to that first
prob.baseline <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/SW2a_mean_probs_all_spp_best_slices_stormwater_scenarios_updatedApril2021.csv") %>% 
  mutate(species.lifestage = paste0(SpeciesName, " ", LifeStageName, sep="" )) %>% 
  mutate(species.lifestage.hyd = paste0(species.lifestage, " ", HydraulicName)) %>% 
  #filter to scenario 22 from stormwater scenarios for baseline
  filter(Scenario == 22)
#save scenario as 0 
prob.baseline$Scenario <- "0"

#test prob baseline GLEN
prob.baseline.LA13 <- prob.baseline[prob.baseline$Node == "LA13",]
#convert to upper case species.lifestage
prob.baseline$species.lifestage <- str_to_title(prob.baseline$species.lifestage)
prob.baseline$LifeStageName <- str_to_title(prob.baseline$LifeStageName)
prob.baseline$species.lifestage.hyd <- str_to_title(prob.baseline$species.lifestage.hyd)

#summarize percentiles based on node, scenario, species.lifestage
prob.baseline2 <- prob.baseline %>% 
  group_by(Node, Scenario, SpeciesName, species.lifestage, species.lifestage.hyd, season) %>% 
  summarize(p90 = quantile(MeanProbability, .90),
            p50 = quantile(MeanProbability, .50),
            p10 = quantile(MeanProbability, .10)) %>% 
  ungroup() %>% 
  rename(ReportingNode = Node) %>% 
  data.frame()

#add baseline rows to ffm.all
prob.all.WRP <- add_row(prob.all2, prob.baseline2)
##exclude the tidal probabilities
prob.all.WRP <- prob.all.WRP[prob.all.WRP$ReportingNode != "LA1" & prob.all.WRP$ReportingNode != "LA2",]
#save scenario as numeric (now that tidal excluded WRP0, WRP50, WRP75, WRP100)
prob.all.WRP$Scenario <- as.numeric(prob.all.WRP$Scenario)


#combine with ffm species lookup table
#prob.all.WRP <- prob.all.WRP %>% 
# merge(ffm.species.lookup, by = "species.lifestage")


#read in WRP scenario labels with seasonal WRP  and various values for each reporting node based on which WRP discharges to it
iterations <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/manuscripts/sensitivitycurves_FFMs/01_data/summary_seasonalWRP_node.csv") %>% 
  rename(dry_season=dry, wet_season=wet, spring=spr)


#SUSTAIN scenario labels
sustain.scenarios <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Sustain_ScenarioNumbers.csv") %>% 
  rename(ScenarioName = Scenario, Scenario = ScenarioNumber)

#read in SUSTAIN scenarios modeled
prob.all.urban <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/SW2a_mean_probs_all_spp_best_slices_stormwater_scenarios_updatedApril2021.csv") %>% 
  mutate(species.lifestage = paste0(SpeciesName, " ", LifeStageName )) %>% 
  rename(ReportingNode = Node) %>% 
  mutate(species.lifestage.hyd = paste0(species.lifestage, " ", HydraulicName))
#convert to upper case species.lifestage
prob.all.urban$species.lifestage <- str_to_title(prob.all.urban$species.lifestage)
prob.all.urban$LifeStageName <- str_to_title(prob.all.urban$LifeStageName)
prob.all.urban$species.lifestage.hyd <- str_to_title(prob.all.urban$species.lifestage.hyd)

#summarize percentiles based on node, scenario, species.lifestage
prob.all.urban2 <- prob.all.urban %>% 
  group_by(ReportingNode, Scenario, SpeciesName, species.lifestage, species.lifestage.hyd, season) %>% 
  summarize(p90 = quantile(MeanProbability, .90),
            p50 = quantile(MeanProbability, .50),
            p10 = quantile(MeanProbability, .10)) %>% 
  ungroup() %>% 
  data.frame()
#exclude the tidal probabilities
prob.all.urban2 <- prob.all.urban2[prob.all.urban2$ReportingNode != "LA1" & prob.all.urban2$ReportingNode != "LA2",]

#read in limiting factors by node (which hydraulics to plot for each species life stage node)
limiting.factors <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/limiting_factors_by_node.csv") %>% 
  mutate(species.lifestage = paste(Species, LifeStage, sep=" ")) %>% 
  mutate(species.lifestage.hyd = paste0(species.lifestage, " ", LimitingFactor))

#rename first col to ReportingNode
names(limiting.factors)[1] <- "ReportingNode"
#convert to upper case species.lifestage
limiting.factors$species.lifestage <- str_to_title(limiting.factors$species.lifestage)
limiting.factors$species.lifestage.hyd <- str_to_title(limiting.factors$species.lifestage.hyd)


#remove rows with none limiting factors
limiting.factors.plot <- limiting.factors %>% 
  filter(LimitingFactor == "Depth" | LimitingFactor == "Velocity")
#update names of growth stage
limiting.factors.plot$species.lifestage[limiting.factors.plot$species.lifestage == "Sas Growth"] <- "Sas Juvenile"
limiting.factors.plot$species.lifestage[limiting.factors.plot$species.lifestage == "Typha Growth"] <- "Typha Seedling"
limiting.factors.plot$species.lifestage.hyd <- gsub("Sas Growth", "Sas Juvenile", limiting.factors.plot$species.lifestage.hyd)
limiting.factors.plot$species.lifestage.hyd <- gsub("Typha Growth", "Typha Seedling", limiting.factors.plot$species.lifestage.hyd)


#output directory where sensitivity curves to be saved
#out.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/results_FFMs/scenario_curves_FFM_all/seasonal/"
#save in report folder
out.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/sensitivity_curves_all/SeasonalWRP_SpeciesBased/"
dir.create(out.dir)

#output directory for BMP curves
#out.dir.bmp <- paste0(out.dir, "Dryweather_Stormdrain_allhydraulics/")
#change if running only limiting factors
out.dir.bmp <- paste0(out.dir, "Dryweather_Stormdrain_limitingfactors/")
dir.create(out.dir.bmp)

#create temp output for species 
#temp.output <- paste0(out.dir, "Temp_SpeciesPlot_allhydraulics/")
#change if running only limiting factors
temp.output <- paste0(out.dir, "Temp_SpeciesPlot_limitingfactors/")
dir.create(temp.output)

#create output for species life stage limiting factors plots
#out.dir.species.lifestage.limitingfactor <- paste0(out.dir, "Species_lifestage_allhydraulics/")
#change if only plotting limiting factors species life stage
out.dir.species.lifestage.limitingfactor <- paste0(out.dir, "Species_lifestage_limitingfactors/")
dir.create(out.dir.species.lifestage.limitingfactor)


#Reporting node description file
reporting.node.names <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- rename(reporting.node.names, ReportingNode = SWMM.Node )


#save seasons to plot wet or dry
seasons.to.plot <- c("Wet", "Dry")


#join WRP with reporting node names and FFM labels: to get the order of the nodes (upstream to downtream) and the reach order3
#WRP percentiles join

prob.all.WRP.join <- merge(prob.all.WRP, reporting.node.names, by= "ReportingNode") %>% 
  #Update to include metric
  select("ReportingNode", "Description", "species.lifestage", "species.lifestage.hyd", "SpeciesName","p10","p50","p90","Scenario", "season", "Reach", "order","order3","Reporting_Reach") %>% 
  #merge(ffm.labels, by = "metric") %>% 
  #merge(iterations, by = "Scenario") %>% 
  mutate(ScenarioType = "WRP") %>% 
  mutate(ScenarioType2 = "WRP")

#need to add stormwater/stormdrain scenarios after data is processed
#join Stormwater urbn with reporting node names and FFM labels: to get the order of the nodes (upstream to downtream) and the reach order3
prob.all.join.bmp.urbn <- merge(prob.all.urban2, reporting.node.names, by= "ReportingNode") %>% 
  select("ReportingNode", "Description", "species.lifestage","species.lifestage.hyd","SpeciesName","p10","p50","p90","Scenario", "season", "Reach", "order","order3","Reporting_Reach") %>% 
  #merge(ffm.labels, by = "metric") %>% 
  merge(sustain.scenarios, by = "Scenario") %>% 
  mutate(ScenarioType = "Dry Weather Stormdrain Reductions") %>% 
  mutate(ScenarioType2 = ScenarioName)



#loop through the scenarios to plot percentiles scenarios curve
#change to unique nodes from bmp urban scenarios
unique.nodes <- unique(prob.all.join.bmp.urbn$ReportingNode)
#unique.nodes <- unique(ffm.all.join$ReportingNode)
unique.urban.nodes <- unique(prob.all.join.bmp.urbn$ReportingNode)
unique.wrp.nodes <- unique(prob.all.WRP.join$ReportingNode)
#find missing nodes not in urban
unique.wrp.nodes[!(unique.wrp.nodes %in% unique.urban.nodes)]

#only run for GLEN for TAC presentation
#i <- grep("GLEN", unique.nodes)
#do not make sensitivity curves for LA1 and LA2 [tidal influence] and CC F45B and Upstream Tillman LA20
unique.nodes <- unique.nodes[unique.nodes != "LA1" & unique.nodes != "LA2" & unique.nodes != "F45B" & unique.nodes != "LA20"]



for(i in 1:length(unique.nodes)){

  #subset percentiles
  #subset WRP to node i
  wrp.sub <- prob.all.WRP.join[prob.all.WRP.join$ReportingNode == unique.nodes[i],]
  #subset SUSTAIN scenarios to node i
  prob.sub.urban <- prob.all.join.bmp.urbn[prob.all.join.bmp.urbn$ReportingNode == unique.nodes[i],]
  #subset limiting factors to plot
  limiting.sub <- limiting.factors.plot[limiting.factors.plot$ReportingNode == unique.nodes[i],]
  #add in limiting factors to plot for urban [if different than WRP]
  
  #add in scenario WRP Q data for node i
  #subset WRP iterations for node i
  iterations.node <- iterations[iterations$ReportingNode == unique.nodes[i],] %>% 
    select(Scenario, dry_season, wet_season, spring)
  #WRP iterations merge with wrp.sub
  wrp.sub <- wrp.sub %>% 
    merge(iterations.node, by="Scenario")
  
  #add in scenario WRPQ data for node i - urban scenarios
  #find ind WRP50 and WRP100 scenarios and put i appropriate values
  ind.WRP50 <- grep("WRP50", prob.sub.urban$ScenarioName)
  #WRP 100
  ind.WRP100 <- grep("WRP100", prob.sub.urban$ScenarioName)
  #find baseline dry, wet, spring for that node
  baselines <- iterations.node[iterations.node$Scenario == 0,]
  
  #Add in seasonal WRP Q, start with all 0 for WRP0 and will replace with values
  prob.sub.urban$dry_season <- 0
  prob.sub.urban$wet_season <- 0
  prob.sub.urban$spring <- 0
  #replace all WRP50 with seasonal wrp/2
  prob.sub.urban$dry_season[ind.WRP50] <- baselines$dry_season/2
  prob.sub.urban$wet_season[ind.WRP50] <- baselines$wet_season/2
  prob.sub.urban$spring[ind.WRP50] <- baselines$spring/2
  #replace all ind.WRP100 with seasonal wrp
  prob.sub.urban$dry_season[ind.WRP100] <- baselines$dry_season
  prob.sub.urban$wet_season[ind.WRP100] <- baselines$wet_season
  prob.sub.urban$spring[ind.WRP100] <- baselines$spring
  
  #loop to create sensitivity curves for each season
  for(j in 1:length(seasons.to.plot)){
    
    #subset percentiles to season j - these contain all hyd factors
    #subset WRP to season j
    wrp.sub.metric.j1 <-  wrp.sub[wrp.sub$season == seasons.to.plot[j],]
    # #if want to plot all variables use this an omit 2 lines that subset to limiting factors
     #wrp.sub.metric.j <-  wrp.sub[wrp.sub$season == seasons.to.plot[j],]
    
    #subset SUSTAIN scenarios to season j
    wrp.sub.metric.j.urban1 <-  prob.sub.urban[prob.sub.urban$season == seasons.to.plot[j],]
    # #if want to plot all variables use this an omit 2 lines that subset to limiting factors
     #wrp.sub.metric.j.urban <-  prob.sub.urban[prob.sub.urban$season == seasons.to.plot[j],]
    
    #limiting factors sub
    limiting.sub2 <- limiting.sub[limiting.sub$Season == seasons.to.plot[j],]
    
    #subset to only those to plot - only limiting hydraulic factors
    wrp.sub.metric.j <-  wrp.sub.metric.j1[wrp.sub.metric.j1$species.lifestage.hyd %in% limiting.sub2$species.lifestage.hyd,]
    wrp.sub.metric.j.urban <-  wrp.sub.metric.j.urban1[wrp.sub.metric.j.urban1$species.lifestage.hyd %in% limiting.sub2$species.lifestage.hyd,]

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
    
    #pivot longer WRP to format correctly
    data.plot <- pivot_longer(wrp.sub.metric.j, cols = c("p90", "p50", "p10"), names_to="Percentile", values_to="Value")
    data.plot$Percentile <- factor(data.plot$Percentile, levels = Percentile)
    
    #SUSTAIN pivot data for urban removal scenarios
    #pivot longer .urban to format correctly
    data.plot.urban <- pivot_longer(wrp.sub.metric.j.urban, cols = c("p90", "p50", "p10"), names_to="Percentile", values_to="Value")
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
    
    #if dry season, set x to dry season wrp
    
    if(seasons.to.plot[j] == "Dry"){
      #rename dry season WRP to generic x name
      data.plot <- data.plot %>% 
        rename(seasonal.wrp.Q = dry_season)
      wrp.sub.metric.j <- wrp.sub.metric.j %>% 
        rename(seasonal.wrp.Q = dry_season)
      wrp.sub.metric.j.urban <- wrp.sub.metric.j.urban %>% 
        rename(seasonal.wrp.Q = dry_season)
      data.plot.urban.merge2.urbnonly <- data.plot.urban.merge2.urbnonly %>% 
        rename(seasonal.wrp.Q = dry_season)
      
      #x axis label for seasonal wrp
      x.axis <- "Average Annual Dry-Season WRP Discharge (cfs)"
      baseline.metric <- baselines$dry_season
      subtitle.lab <- "Dry-Season Probability of Occurrence"
      y.axis <- "Average Annual Dry-Season Probability of Occurrence"
    }else{
      #rename wet season WRP to generic x name 
      #wrp data
      data.plot <- data.plot %>% 
        rename(seasonal.wrp.Q = wet_season)
      wrp.sub.metric.j <- wrp.sub.metric.j %>% 
        rename(seasonal.wrp.Q = wet_season)
      wrp.sub.metric.j.urban <- wrp.sub.metric.j.urban %>%
        rename(seasonal.wrp.Q = wet_season)
      data.plot.urban.merge2.urbnonly <- data.plot.urban.merge2.urbnonly %>% 
        rename(seasonal.wrp.Q = wet_season)
      
      #x axis label for seasonal wrp
      x.axis <- "Average Annual Wet-Season WRP Discharge (cfs)"
      baseline.metric <- baselines$wet_season
      subtitle.lab <- "Wet-Season Probability of Occurrence"
      y.axis <- "Average Annual Wet-Season Probability of Occurrence"
      
    }
    
    
    
    #another loop to create sensitivity curves for each species
    unique.species <- unique(data.plot$SpeciesName)
    
    for(m in 1:length(unique.species)){
      #subset to species m
      data.plot.species <- data.plot %>% 
        filter(SpeciesName == unique.species[m])
    
      #line plot with all species life stages
      p.all <- ggplot(data.plot.species, aes(x=seasonal.wrp.Q, y = Value, color=species.lifestage.hyd)) +
        geom_point() +
        labs(title = paste0("WRP Species Sensitivity Curve: ", unique.nodes[i]), subtitle=subtitle.lab,
             color = "Legend") + ylab(subtitle.lab) +
        xlab(x.axis) +
        #scale_color_manual(values = colors, labels = labels1, name="Probability Percentile") +
        theme_bw() +
        geom_vline(xintercept=baseline.metric, linetype="dashed", color = "black") +
        geom_text(aes(x= baseline.metric-1, y = .1, label = "Baseline WRP", angle=90), color = "black")+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
      #print plot
      #print(p.all)
      #save
      file.name <- paste0(temp.output, unique.nodes[i], "_", seasons.to.plot[j], "_", unique.species[m],"_Species_Sensitivity_Curve_allstage.jpg")
      ggsave(p.all, filename=file.name, dpi=300, height=5, width=7)
      
      #Loop to plot smooth curve for each life stage hydraulics
      unique.lifestage.hyd <- unique(data.plot.species$species.lifestage.hyd)

      
      for(n in 1:length(unique.lifestage.hyd)){
        ####################################
        #####SMOOTH SENSITIVITY CURVES:
        #Plot as a smooth curve with shaded band (linear reg for p50 with the upper bound being p90 and lower p10) 
        
        #subset 
        wrp.sub.metric.j.life.stage <- wrp.sub.metric.j[wrp.sub.metric.j$species.lifestage.hyd == unique.lifestage.hyd[n],]
        
        #find unique species life stage to get y max (p90 wet) and y min (p10 dry)
        unique.species.lifestage <- unique(wrp.sub.metric.j.life.stage$species.lifestage)

        #make y axis the same for each life stage based on max p90 and min p10 
        #rename dry season WRP to generic x name
        wrp.sub.lifestage <- wrp.sub %>% 
          filter(species.lifestage.hyd == unique.lifestage.hyd[n])
        #sub.life.stage.urban <- wrp.sub.metric.j.urban %>% 
          #filter(species.lifestage == uunique.species.lifestage)

        #y axis max lim
        ymax.wrp <- max(wrp.sub.lifestage$p90) *1.1
        #if max >1, set to 1, else keep as is
        ymax.wrp <- ifelse(ymax.wrp > 1, 1, ymax.wrp)
        ymin.wrp <- min(wrp.sub.lifestage$p10)
        #ymax.urban <- max(wet.life.stage.urban$p90)

        #lower bound curve fit for ribbon
        upper.curve <- loess(p90 ~ seasonal.wrp.Q, data= wrp.sub.metric.j.life.stage) 
        #predict upper limit data to plot for upper limit
        upper.fit <- data.frame(predict(upper.curve, se=TRUE ))
        #if any values above 1, change to 1
        upper.fit$fit[upper.fit$fit > 1] <- 1
        #max(upper.fit$fit)
        
        #lower bound curve fit for ribbon
        lower.curve <- loess(p10 ~ seasonal.wrp.Q, data= wrp.sub.metric.j.life.stage) 
        #predict lower limit data to plot for lower limit
        lower.fit <- data.frame(predict(lower.curve, se=TRUE ))
        #set minimum as zero
        lower.fit$fit[lower.fit$fit < 0] <- 0
        min.lowerfit <- min(lower.fit$fit)
        ymin.wrp <- ifelse(min.lowerfit < ymin.wrp, min.lowerfit, ymin.wrp)
        
        #plot with stat smooth WRP curve
        p2.life.stage <- ggplot(wrp.sub.metric.j.life.stage, aes(x=seasonal.wrp.Q, y = p50), color="blue") +
          #geom_point() +
          geom_smooth(level=0) +
          geom_ribbon(aes(ymin=lower.fit$fit, ymax=upper.fit$fit), alpha=0.2, fill="#4575b4") +
          labs(title = paste0("Species Sensitivity Curve: ", unique.nodes[i]), subtitle=paste0(subtitle.lab, ": ",unique.lifestage.hyd[n]),
               color = "Legend") + ylab(subtitle.lab) +
          geom_vline(xintercept=baseline.metric, linetype="dashed", color = "black") +
          #geom_hline(yintercept=0.5, linetype="dotted", color = "black") +
          geom_text(aes(x= baseline.metric-1, y = ymin.wrp+0.075, label = "Baseline WRP", angle=90), color = "black")+
          xlab(x.axis) + 
          theme_bw() + 
          ylim(c(ymin.wrp*0.7, ymax.wrp)) +
          theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
        
        #p2.life.stage
        #save
        file.name2 <- paste0(out.dir.species.lifestage.limitingfactor, unique.nodes[i], "_", unique.lifestage.hyd[n], "_",seasons.to.plot[j], "_Species_Sensitivity_Curve.jpg")
        ggsave(p2.life.stage, filename=file.name2, dpi=300, height=5, width=5)
        
      
        #####################################################
        #JOSE: do not need to plot the urban baseflow removal scenarios, only WRP plot
        ####Create smooth sensitivity curve with urban baseflow removal scenarios only
        #subset stormdrain scenarios to lifestage hydraulics
        #wrp.sub.metric.j.urban.life.stage <- wrp.sub.metric.j.urban[wrp.sub.metric.j.urban$species.lifestage.hyd == unique.lifestage.hyd[n],]
        data.plot.urban.merge2.urbnonly.lifestage.hyd <- data.plot.urban.merge2.urbnonly[data.plot.urban.merge2.urbnonly$species.lifestage.hyd == unique.lifestage.hyd[n],]
        
        #Subset 50% reduction urban and pivot wider
        urban50 <- data.plot.urban.merge2.urbnonly.lifestage.hyd %>% 
          filter(UrbanBaseflow_pctQ == "UBF50")  %>% 
          pivot_wider(values_from = Value, names_from = Percentile) %>% 
          data.frame()
        
        #50% reduction urban upper and lower bounds based on p90 and p10
        upper.bound50 <- urban50$p90
        #predict values 
        lower.bound50 <- urban50$p10
        
        #Subset 100% reduction urban and pivot wider
        urban0 <- data.plot.urban.merge2.urbnonly.lifestage.hyd %>% 
          filter(UrbanBaseflow_pctQ == "UBF0") %>% 
          pivot_wider(values_from = Value, names_from = Percentile) %>% 
          data.frame()
        #100% reduction urban 
        #upper bound (p90)
        upper.bound0 <- urban0$p90
        #lower bound (p10)
        lower.bound0 <- urban0$p10
        
        #if min of 100% reduction in stormdrain Q is lower than ylim of original WRP curve, find new ylim min and set for stormdrain plots
        ymin <- min(lower.bound0)
        if(ymin < ymin.wrp*0.7){
          #100% reduction plot with smooth curve, new ylim min
          urban0.p <- p2.life.stage +
            geom_ribbon(data = urban0, aes(ymin=lower.bound0, ymax=upper.bound0), alpha=0.2, fill = "red") +
            geom_line(data = urban0, mapping = aes(x=seasonal.wrp.Q, y = p50),color="#d73027", linetype="twodash", lwd=1) +
            ylim(c(ymin, ymax.wrp))
        }else{
          #100% reduction plot with smooth curve
          urban0.p <- p2.life.stage +
            geom_ribbon(data = urban0, aes(ymin=lower.bound0, ymax=upper.bound0), alpha=0.2, fill = "red") +
            geom_line(data = urban0, mapping = aes(x=seasonal.wrp.Q, y = p50),color="#d73027", linetype="twodash", lwd=1)
        }
        
        
        #save
        #file.name2 <- paste0(out.dir.bmp, "smoothcurve_", unique.nodes[i], "_", unique.lifestage.hyd[n], "_",seasons.to.plot[j],"_WRP_Sensitivity_Curve_0Urbanflow.jpg")
        #ggsave(urban0.p, filename=file.name2, dpi=300, height=5, width=5)
        
        #50% reduction plot with smooth curve
        urban50.p <- urban0.p +
          geom_ribbon(data = urban50, aes(ymin=lower.bound50, ymax=upper.bound50), alpha=0.5, fill = "#fee090") +
          geom_line(data = urban50, mapping = aes(x=seasonal.wrp.Q, y = p50),color="#fc8d59", linetype="twodash", lwd=1) 
        
        #save
        file.name2 <- paste0(out.dir.bmp, "stormdrainreduction50100_", unique.nodes[i], "_",seasons.to.plot[j], "_", unique.lifestage.hyd[n],  ".jpg")
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
        
        #change from baseline to 0 WRP
        dry.change <- (min(ffm.sub.metric.j$p10)-max(ffm.sub.metric.j$p10))/max(ffm.sub.metric.j$p10)*100
        med.change <- (min(ffm.sub.metric.j$p50)-max(ffm.sub.metric.j$p50))/max(ffm.sub.metric.j$p50)*100
        wet.change <- (min(ffm.sub.metric.j$p90)-max(ffm.sub.metric.j$p90))/max(ffm.sub.metric.j$p90)*100
        
      }
    }
    
  }
}




















#DONT" NEED
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


