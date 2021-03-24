#Create overall WRP iteration summary labels - seasonal WRP

library("dplyr")

######
#WRP scenario labels - original average WRP
iterations <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Results-Scenarios/iterations_labeled.csv")

#directory with seasonal WRP data for each node
seasonal.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/manuscripts/sensitivitycurves_FFMs/seasonal_WRP_by_node/"
files <- list.files(seasonal.dir, full.names = TRUE)
files.short <- list.files(seasonal.dir)
#find nodes with seasonal data
all.nodes <- sapply(strsplit(files.short,"_"), `[`, 4) #all nodes associated with the files
#find index of LA20_2 <- originally saved as LA20, need to replace those nodes with correct value
ind.la20_2 <- grep("LA20_2", files.short)
#replace names for LA20_2 from LA20
all.nodes[ind.la20_2] <- "LA20_2"
unique.nodes <- unique(all.nodes) #unique nodes only

#exclude F37B high and low compton creek since not impacted by changes in WRP
unique.nodes <- unique.nodes[unique.nodes != "F37B"]

#loop through nodes and seasonal baseline and scenario values create output for WRP and urban scenarios
#create output df iterations.WRP
iterations.WRP <- data.frame(matrix(NA,1, 5))
names(iterations.WRP) <- c("Scenario", "dry", "wet", "spr", "ReportingNode")

for(i in 1:length(unique.nodes)){
  #find files for node baseline and scenarios
  ind.nodes <- grep(paste0(unique.nodes[i], "_"), files.short )
  
  if(unique.nodes[i] == "LA20"){
    #do not use LA20_2 files for this
    ind.LA20_2 <- grep("LA20_2", files.short)
    ind.nodes <- ind.nodes[ind.nodes != ind.LA20_2]
  }
  #find the short file names to search for baseline and scenario files
  list.files.node <- files.short[ind.nodes]
  baseline.ind <- grep("baseline", list.files.node)
  scenarios.ind <- grep("scenarios", list.files.node)
  #list long files
  list.files.node.long <- files[ind.nodes]
  #find the long file name
  
  #read in scenario csv
  scen.it <- read.csv(list.files.node.long[scenarios.ind]) %>% 
    rename(Scenario = scenario)

  #baseline read csv
  baseline <- read.csv(list.files.node.long[baseline.ind], header=F)
  #add in baseline to scen.it
  scen.it[501,] <- c(0, baseline$V2[baseline$V1 == "dry"], baseline$V2[baseline$V1 == "wet"], baseline$V2[baseline$V1 == "spr"])
  #add in nodes
  scen.it$ReportingNode <- unique.nodes[i]
  
  #append to output matrix
  iterations.WRP <- rbind(iterations.WRP, scen.it)
  
}


#remove first row of NA
iteration.WRP2 <- iterations.WRP[2:length(iterations.WRP$Scenario),]

#write summary table 
write.csv(iteration.WRP2, file= "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/manuscripts/sensitivitycurves_FFMs/summary_seasonalWRP_node.csv",row.names=FALSE)



