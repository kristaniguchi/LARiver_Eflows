#Baseline Functional Flow Metrics - Reporting Nodes
  #calculate functional flow metrics for all reporting nodes under baseline conditions
  #Create boxplot figures showing the range for each FFM and reporting node


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

#directory with flow output
wd <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/"
#list files
list.files <- list.files(wd, full.names = TRUE)
hrly.files <- grep("Nodes/hourly", list.files)
flow.files <- list.files[hrly.files]
#set output directory
output.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/"


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

#test out GLEN
i=8

for(i in 1:length(flow.files)){
  #read in flow file
  data <- read.csv(flow.files[i])
  #reporting node
  node <- gsub(".csv", "", flow.files[i])
  node <- gsub("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/hourly_flow_", "", node)
    
  #format date
  #if 11101250, format date differently
  if(node == "11101250"){
    data$datetime <- as.POSIXct(data$datetime, format="%m/%d/%Y %H:%M")
    data$date <- format(data$datetime, format="%m/%d/%Y")
    unique.dates <- unique(data$date)
    
  }else{
    data$datetime <- as.POSIXct(data$datetime, format="%Y-%m-%d %H:%M:%S")
    data$date <- format(data$datetime, format="%m/%d/%Y")
    unique.dates <- unique(data$date)
  }
  ################
  
  #calc mean daily flow for predicted data
  #empty mean daily flow vector
  flow.pred <- NA
  
  for (j in 1:length(unique.dates)){
    sub.day <- data[data$date  == unique.dates[j],]
    flow.pred[j] <- mean(sub.day$flow, na.rm = TRUE)
  }
  #create new data frame with date and mean daily flow to go into FFC
  data.daily <- data.frame(cbind(unique.dates, flow.pred))
  names(data.daily) <- c("date", "flow")
  data.daily$flow <- as.numeric(data.daily$flow)
  data.daily <- na.omit(data.daily)
  #write daily output file
  fname <- paste0(wd,"daily/", node,"_flow_daily.csv")
  write.csv(data.daily, fname, row.names = FALSE)
  
  ####FFM 
  #calc FFMs and alteration for future data
  #create new directory to save ffm outputs
  dir.new <- paste0(output.dir,node)
  dir.create(dir.new)
  #find COMID for reporting node i
  COMID <- comid.node$COMID[comid.node$Name == node]
  
  #Run data through FFC online with my own gage data.future or model data.future
  #new FFC api set up
  results <- FFCProcessor$new()  # make a new object we can use to run the commands
  #allow ffc to run with min of 1 years
  results$fail_years_data <- 1
  #setup
  results$set_up(timeseries=data.daily,
                     token=mytoken,
                     comid = COMID[1])
  #set to "RGW" stream class for all --> every COMID is in this class in SOC
  results$stream_class <- "RGW"
  #then run
  results$run()
  
  results <- ffcAPIClient::evaluate_alteration(timeseries_df = data.daily, comid = COMID[1], token = mytoken)
  #reference percentiles
  ref.percentiles <- results$predicted_percentiles
  ref.percentiles$source2 <- rep("Statewide\nReference", length(ref.percentiles$p10))
  ref.percentiles.wyt <- results$predicted_wyt_percentiles
  #predicted results, SWMM baseline
  baseline.alteration.all <- results$alteration
  baseline.percentiles.all <- results$ffc_percentiles
  baseline.label <- paste0("SWMM\n")
  baseline.percentiles.all$source2 <- rep(baseline.label, length(baseline.percentiles.all$p10))
  baseline.percentiles.all$ReportingNode <- rep(node, length(baseline.percentiles.all$p10))
  baseline.results.ffm.all <- results$ffc_results
  baseline.results.ffm.all$type <- "baseline"
  baseline.drh.data <- results$drh_data.baseline
  #write outputs to dir
  write.csv(ref.percentiles, file=paste0(dir.new,"/ref.percentiles.statewide.csv"), row.names=FALSE)
  write.csv(baseline.alteration.all, file=paste0(dir.new,"/baseline.alteration.statewide.all.", node,".csv"), row.names=FALSE)
  write.csv(baseline.percentiles.all, file=paste0(dir.new,"/baseline.percentiles.all.", node,".csv"), row.names=FALSE)
  write.csv(baseline.results.ffm.all, file=paste0(dir.new,"/baseline.results.ffm.all.", node,".csv"), row.names=FALSE)
  write.csv(baseline.drh.data, file=paste0(dir.new,"/baseline.drh.data.", node,".csv"), row.names=FALSE)

  #save percentiles into percentiles.all df
  percentiles.all <- data.frame(rbind(percentiles.all, baseline.percentiles.all))
}

#remove first NA row
percentiles.all2 <- percentiles.all[2:length(percentiles.all$p10),]
#save reporting node column as class
percentiles.all2$ReportingNode <- as.character(percentiles.all2$ReportingNode)
#write percentiles.all
write.csv(percentiles.all2, file = "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/FFM_percentiles_reportingnodes_all.csv")




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
write.csv(percentiles.all.sort.rnd, file= "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Reporting-Nodes/daily/FFM/FFM_percentiles_reportingnodes_all_formattedround.csv")




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






