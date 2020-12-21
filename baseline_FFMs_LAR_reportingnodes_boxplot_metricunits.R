#Aquatic life use report
#FFM boxplots: metric units
#UPDATE: exclude LA 1 and 2 from plots for now


#other packages
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")

#COMID for each reporting node
comid.node <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/reportingnodes_COMID.csv")

#Reporting node description file
reporting.node.names <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- rename(reporting.node.names, ReportingNode = SWMM.Node )

#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric
#change all the units of cfs to cms in ffm.labels df
ffm.labels[] <- lapply(ffm.labels, function(x) gsub("cfs", "cms", x))

#create list of magnitude metrics that need to be converted
mag.metrics <- ffm.labels$flow_metric[grep("cms",ffm.labels$flow_characteristic)]


#read in percentile data metric units
percentiles.all <- read.csv(file = "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/FFM_percentiles_reportingnodes_all.csv")
percentiles.all2 <- percentiles.all

#convert all percentiles of magnitude metrics to cms
for(i in 1:length(percentiles.all$X)){
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
percentiles.all3 <- select(percentiles.all2, "ReportingNode", "p10","p25","p50","p75","p90","metric") 

#join with reporting node names: to get the order of the nodes (upstream to downtream) and the reach order3
percentiles.all.join <- full_join(percentiles.all3, reporting.node.names, by= "ReportingNode") %>% 
  select("X.","ReportingNode", "Description","p10","p25","p50","p75","p90","metric", "Reach", "order","order3","Reporting_Reach")

#sort based on metric and X.
percentiles.all.sort <- percentiles.all.join[order(percentiles.all.join$metric, percentiles.all.join$X.),]

#round values
percentiles.all.sort.rnd <- percentiles.all.sort %>% mutate_if(is.numeric, ~round(., 2))
#write.csv(percentiles.all.sort.rnd, file= "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/FFM_percentiles_reportingnodes_all_formattedround.csv")

##### Visualizing flow metrics ###########

#loop to go through each metric, make metric boxplots for each reporting node from downstream to upstream

#unique flow metrics 
unique.metrics <- unique(percentiles.all.sort$metric)

#for the plots, we want to show upstream to downstream, and rio hondo and compton creek where they input
#use order3 to have rio hondo and compton creek plotted at the end
percentiles.all.sort2 <- percentiles.all.join[order(percentiles.all.join$metric, percentiles.all.join$order3),]

#UPDATE: exclude LA1 and LA2 from boxplots --> tidal influence
exclude <- c("LA1", "LA2")
percentiles.all.sort2 <- percentiles.all.sort2[-which(percentiles.all.sort2$ReportingNode %in% exclude),]

for(k in 1:(length(unique.metrics)-1)){
  #### filter  based on metric k#### 
  sub <- filter(percentiles.all.sort2, metric == unique.metrics[k])
  #save reportingnode as vector with order
  sub$ReportingNode <- factor(sub$ReportingNode, levels = unique(sub$ReportingNode))
  #find metric name infor
  metric.info <- ffm.labels[ffm.labels$metric == unique.metrics[k],]
  #make reach a factor
  sub$Reach <- factor(sub$Reach, levels=c("Mainstem", "Rio Hondo", "Compton Creek"))
  #max median metric value
  max <- max(sub$p50)
  min <- min(sub$p50)
  range <- max - ((max - min)/10)
  
  
  ####create boxplots####
  #remove duplicate compton creek values
  ind.cc <- grep("Upper Compton", sub$Description)
  sub2 <- sub[-ind.cc,]
  #max of all values plotted for label positioning
  max.all <- max(sub$p75)
  
  
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
  if(unique.metrics[k] == "Wet_BFL_Mag_50"){
    b <- b + 
      annotate(geom = "text", x = 5.2, y = 5.3, label = "Glendale", angle = 90) +
      annotate(geom = "text", x = 3.2, y = 5.3, label = "Burbank", angle = 90) +
      annotate(geom = "text", x = 2.2, y = 5.3, label = "Tillman", angle = 90) 
  }else{
    b <- b + 
      annotate(geom = "text", x = 5.2, y = max.all, label = "Glendale", angle = 90) +
      annotate(geom = "text", x = 3.2, y = max.all, label = "Burbank", angle = 90) +
      annotate(geom = "text", x = 2.2, y = max.all, label = "Tillman", angle = 90) 
  }

  print(b)
  file.name <- paste0("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/0boxplots_metricunits/", metric.info$metric, "_boxplot_reportingnodes.jpg")
  ggsave(b, filename=file.name, dpi=300, height=5, width=9.5)
  
  
}





