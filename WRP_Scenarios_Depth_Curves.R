#Scenarios: Depth Curves for Rec Use/Aquatic Life
  #scenario outputs for depth --> curves for each node, % of time depth is suitable during spring/summer


#directory with post-processed hydraulics
out.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Scnearios_hydraulics_10132020/results-hydraulics_postprocessed/"
files <- list.files(out.dir, full.names = TRUE)
files.short <- list.files(out.dir)
#node names
nodes <- gsub("_predictions.csv", "", files.short)

#create at output matrix
summary.outputs <- data.frame(matrix(NA, nrow=1, ncol=3))
names(summary.outputs) <- c("Scenario", "Node", "pct.time.depth.suitable")

#loop to go through each file and post-process data
#i=10 #test LA11 in Glendale narrows
for(i in 1:length(files)){
  
  #read in csv raw hydraulics to be processed
  data <- read.csv(files[i])
  #node
  node.i <- nodes[i]
  
  temp.output <- summary.outputs[1,]
  
  #loop through each scenario
  for(j in 1:500){
    #subset to scenario j
    data.sub <- data[data$Scenario == j,]
    
    #subset date to spring/summer 5/31 to 9/7
    ind.start <- grep("5/31", data.sub$DateTime)
    ind.end <- grep("9/30", data.sub$DateTime)
    #create empty df for sub spring/summer
    spring.summer.sub <- data.sub[1,]
    spring.summer.sub[1,] <- NA
      
    for(k in 1:length(ind.start)){
      #subset to spring/summer rec period
      spring.sum <- data.sub[ind.start[k]:ind.end[k],]
      #save in spring summer sub
      spring.summer.sub <- rbind(spring.summer.sub, spring.sum)
      
    }
    
    #remove first NA
    spring.summer.sub <- spring.summer.sub[2:length(spring.summer.sub$DateTime),]

    #summarize percent of time max depth > 1 ft
    total.days <- length(spring.summer.sub$DateTime)
    days.suitable <- length(which(spring.summer.sub$Max.Chl.Dpth..ft. > 1))
    #if 0 days suitable percent is 0
    if(days.suitable == 0){
      percent <- 0
    }else{
      percent <- (days.suitable/total.days)*100
    }
    
    #save into temp output df
    temp.output[j,] <- c(j, node.i, percent)
  }
  
  #save into main output df
  summary.outputs <- rbind(summary.outputs, temp.output)
  
}

