#Find the Q range associated with optimal depths for fishing and kayaking recreational uses
  #Kayaking: 0.9-1.5 ft
  #Fishing: 1-2 ft


# #read in flow ranges for specific reporting nodes and species-lifestage
ranges <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/MM_March04_Typha_Steelhead_Cladophora_SAS_Willow_02_05_2021_updated_KI_MM_KI2.csv")
#update node name LA20 2* with LA20_2, remove *
ranges$Node <- gsub("[*].*$", "", ranges$Node)
ranges$metric <- gsub("Use: ", "", ranges$metric)


#read in data for boxplots with flow ranges 
#GLEN data - working on an overall table with all nodes to loop through (same format)
data <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/synthesis_boxplot_example_GLEN.csv",stringsAsFactors = FALSE, encoding = "UTF-8")

#recreational nodes in Glendale, Sepulveda Basin (excluding LA1 and 2 LB estuary since negative Q values)
rec.nodes <- c("LA20_2", "LA14", "GLEN", "LA11", "F57C")

#node and reach data
reporting.node.names <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- rename(reporting.node.names, ReportingNode = SWMM.Node )


#baseline hydraulic directory
hyd.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Baseline_hydraulic-results-v4-201130/results-hydraulics_postprocessed/"
files.long <- list.files(hyd.dir, full.names=TRUE)
files.short <- list.files(hyd.dir, full.names=FALSE)



#loop to go through all rec.nodes and read in baseline hydraulic data and find Q associated with upper and lower limits
rec.flow.ranges <- data.frame(matrix(NA, 1, 16))
names(rec.flow.ranges) <- names(ranges)

for(i in 1:length(rec.nodes)){
  #find outputs associated with rec.nodes i
  ind.i <- grep(paste0(rec.nodes[i], "_predictions"), files.short)
  #find reporting reach name
  reporting.reach <- reporting.node.names$Reporting_Reach[reporting.node.names$ReportingNode == rec.nodes[i]]
  #read in csv
  baseline.hyd <- read.csv(files.long[ind.i] )
  baseline.hyd <- na.omit(baseline.hyd)
  #find recreational use range
  kaykak.depth.approx <- approx(baseline.hyd$Max.Chl.Depth..ft., baseline.hyd$Flow,  xout=c(0.93, 1.5), ties=mean) #changed to .93 to get depth at one node with min at .928
  fishing.depth.approx <- approx(baseline.hyd$Max.Chl.Depth..ft., baseline.hyd$Flow,  xout=c(1, 2), ties=mean)
  kayak.row <- c("Rec. Use - Kayak", "Rec. Use Kayak", rec.nodes[i], reporting.reach, "Main Channel", "Summer Baseflow", NA, kaykak.depth.approx$y[1], kaykak.depth.approx$y[2], NA, NA, NA, NA, NA, NA, NA)
  fishing.row <- c("Rec. Use - Fishing", "Rec. Use Fishing", rec.nodes[i], reporting.reach, "Main Channel", "Summer Baseflow", NA, fishing.depth.approx$y[1], fishing.depth.approx$y[2], NA, NA, NA, NA, NA, NA, NA)
  #save into output df
  rec.flow.ranges <- rbind(rec.flow.ranges, kayak.row, fishing.row)

}

#remove first row na
rec.flow.ranges2 <- rec.flow.ranges[2:length(rec.flow.ranges$Species),]
#add in duration and timing
#water year day for April1 - Sept 31 (rec use season)
#x <- seq(from=as.Date("2010-03-01"),to=as.Date("2010-10-01"),by="1 days")
library("EflowStats")
WYD_day.start <- get_waterYearDay("2010-04-01")
WYD.end <- get_waterYearDay("2010-12-31")
#data.frame(cbind(as.character(x), WYD_day))
duration <- WYD.end - WYD_day.start
#add to df
rec.flow.ranges2$Duration_Days <- duration
rec.flow.ranges2$Timing_Start_WYDay <- WYD_day.start
rec.flow.ranges2$Timing_End_WYDay <- WYD.end


#combine with ranges
ranges.all <- rbind(ranges, rec.flow.ranges2)
#write.csv
write.csv(ranges.all, file="C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/FlowRanges_Species_RecUses_Allnodes_03142021.csv", row.names=FALSE)
