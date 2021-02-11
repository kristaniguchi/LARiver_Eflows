#LAR Eflows Study
#Post-process the hydraulic model output. Overall:
  #change all depths < 0.1 to 0
  #if velocity is < 0, then depth should be zero
  #if [velocity, shear, or power] < 0, set value to 0
  #NEW: if depth <= 0, all other values in position should be 0
#But first, find the max depth for each channel slice, then post process depths
  #use the max channel depth to find water surface elevation (max depth + thalweg)
  #use WSE to find slice depths



#Hydraulics raw output directory
#scenarios hydraulics:
#raw.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/WRP_scenarios/" #WRP scenarios
raw.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Stormwater_Scenarios/" #stormwater scenarios
#baseline hydraulics:
#raw.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Baseline_hydraulic-results-v4-201130/"


#list files
files <- list.files(raw.dir, full.names = TRUE, pattern=".csv")
files.short <- list.files(raw.dir, pattern=".csv")

#node names from the file names, need to change based on file name structure
#current conditions hydraulics
#nodes <- gsub("hydraulic_ts_", "", files.short)
# <- gsub(".csv", "", nodes)
#scenario hydraulics node, also v4 baseline
nodes <- gsub("_predictions.csv", "", files.short)

#read in the channel geometry data used to calc max depth from WSE
geom <- read.csv(file="C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/node-cross-sections/reporting-nodes-bank-elevations.csv")
#replace name Main to MC to match hyd output name
names(geom) <- gsub("Main", "MC", names(geom))

#post-processed directory
#create new output directory within raw.dir
out.dir <- paste0(raw.dir, "results-hydraulics_postprocessed/")
#create output directory
dir.create(out.dir)

#run LA20_2 and F57C
#i <- grep("LA20_2", files) #LA20_2
#i <- grep("F57C", files)

#loop to go through each file and post-process data
for(i in 1:length(files)){
#for(i in 1:15){
    
  #read in csv raw hydraulics to be processed
  data <- read.csv(files[i])
  #replace typo in column name Dpth to Depth
  names(data) <- gsub("Dpth", "Depth", names(data))
  #find column names; will do filters based on depth, velocity, and power
  col.names <- names(data)
  #find the col indices for depth
  depth.cols <- grep("Depth", col.names)
  #depth col names
  depth.col.names <- col.names[depth.cols]
  
  #new chunk to account for reformatted output without hydraulic depth, old had hyd depth included in outputs
  #if hydraulic depth column missing (only max depth outputted), need to add in dummy hydraulic depth columns for the channel locations with data
  if(length(depth.col.names == 1)){
    #find channel positions with output data velocity
    num.positions <- length(grep("Vel", col.names))
    #if 3 channel positions, create dummy for LOB, MC, ROB
    if(num.positions == 3) {
      #create dummy hyd depth columns to mimic original output format
      data$Hydr..Depth..ft..LOB <- NA
      data$Hydr..Depth..ft..MC <- NA
      data$Hydr..Depth..ft..ROB <- NA
      
      #find column names; will do filters based on depth, velocity, and power
      col.names <- names(data)
      #find the col indices for depth
      depth.cols <- grep("Depth", col.names)
      #depth col names
      depth.col.names <- col.names[depth.cols]
      
    }
  }
  
  
  #read in the geom data for node i
  geom.i <- geom[geom$Node == nodes[i],]
  #find col names
  geom.i.colnames <- names(geom.i)
  #update the LOB, Main, ROB depths from hyd.depth to max depth using WSE and min elevation
  #find min elevation columns index
  min.col.ind <- grep("Min", geom.i.colnames)
  #determine if NA for LOB, ROB
  ind.na <- which(is.na(geom.i[,min.col.ind]))
  #if NA, remove from col ind list
  if(length(ind.na) > 0){
    #remove NA columns
    min.col.ind <- min.col.ind[-ind.na]
  }
  #find min column names
  min.col.names <- geom.i.colnames[min.col.ind]
  #find thalweg (min) elevation for entire XS
  thalweg.elev <- min(geom.i[,min.col.ind])
  #find WSE from the max channel depth and thalweg elevation
  wse.data <- thalweg.elev + data$Max.Chl.Depth..ft.
  
  #### loop through the min elevations and recalc the max depth using WSE
  for(j in 1:length(min.col.ind)){
    #find channel position
    slice <- gsub("Min.Elev..", "", min.col.names[j])
    #find index of col that needs to be replaced
    ind.replaced <- grep(slice, depth.col.names)
    col.replace <- depth.col.names[ind.replaced]
    #find max section depth
    max.depth.j <- wse.data - geom.i[,min.col.ind[j]]
    #if WSE is below the min slice depth (i.e. overbank slices), replace neg with zero
    max.depth.j[max.depth.j < 0] <- 0
    #replace hyd depth with max.depth
    ind.replaced.overall <- grep(col.replace, col.names)
    data[,ind.replaced.overall] <- max.depth.j
    #change col name to Max..Depth..ft..LOB
    names(data)[ind.replaced.overall] <- gsub("Hydr", "Max", col.replace)
  }
  
  ###depth filter####
  #if depth < 0.1 change to 0
  sub <- replace(data[,depth.cols], data[,depth.cols]<0.1, 0)
  #save updated data back into data df
  data[,depth.cols] <- sub
  
  #if velocity < 0, then depth should be zero
  #find the col indices for vel
  vel.cols <- col.names[grep("Vel", col.names)]
  depth.cols2 <- col.names[grep("Depth", col.names)]
  #if vel < 0 change the depth to 0, if depth == 0 change all values to 0 in position
  #if more than one vel columns (LOB, MC, ROB), replace, else if only one (MC), only replace MC
  if(length(vel.cols)> 1){
    #if vel <- change depth to 0
    data$Max..Depth..ft..LOB[data$Avg..Vel...ft.s..LOB < 0] <- 0
    data$Max..Depth..ft..MC[data$Avg..Vel...ft.s..MC < 0] <- 0
    data$Max..Depth..ft..ROB[data$Avg..Vel...ft.s..ROB < 0] <- 0
    #if depth == 0, change all other columns to 0 in that position
    #LOB
    data$Avg..Vel...ft.s..LOB[data$Max..Depth..ft..LOB == 0] <- 0
    data$Shear..lb.sq.ft..LOB[data$Max..Depth..ft..LOB == 0] <- 0
    data$Stream.Power..lb.ft.s..LOB[data$Max..Depth..ft..LOB == 0] <- 0
    #MC
    data$Avg..Vel...ft.s..MC[data$Max..Depth..ft..MC == 0] <- 0
    data$Shear..lb.sq.ft..MC[data$Max..Depth..ft..MC == 0] <- 0
    data$Stream.Power..lb.ft.s..MC[data$Max..Depth..ft..MC == 0] <- 0
    #ROB
    data$Avg..Vel...ft.s..ROB[data$Max..Depth..ft..ROB == 0] <- 0
    data$Shear..lb.sq.ft..ROB[data$Max..Depth..ft..ROB == 0] <- 0
    data$Stream.Power..lb.ft.s..ROB[data$Max..Depth..ft..ROB == 0] <- 0
    
  }else{
    #if vel <0 change depth to 0
    data$Max..Depth..ft..MC[data$Avg..Vel...ft.s..MC < 0] <- 0
    #MC if depth 0 change all values in MC to 0
    data$Avg..Vel...ft.s..MC[data$Max..Depth..ft..MC == 0] <- 0
    data$Shear..lb.sq.ft..MC[data$Max..Depth..ft..MC == 0] <- 0
    data$Stream.Power..lb.ft.s..MC[data$Max..Depth..ft..MC == 0] <- 0
    
  }

  #all other values, if negative, replace with 0 (velocity, shear, power)
  #index of first variable col
  ind.vars <- grep("Avg", names(data))
  variable.cols <- ind.vars[1]:length(names(data))
  #if values < 0 change to 0
  sub2 <- replace(data[,variable.cols], data[,variable.cols]<0, 0)
  #save updated data back into data df
  data[,variable.cols] <- sub2

  #write csv in outdir
  file.name <- paste0(out.dir, files.short[i])
  write.csv(data, file=file.name, row.names=FALSE)
  
}
