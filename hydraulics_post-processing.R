#LAR Eflows Study
#Post-process the hydraulic model output
  #change all depths < 0.1 to 0
  #if velocity is < 0, then depth should be zero
  #if [velocity, shear, or power] < 0, set value to 0



#Hydraulics raw output directory
#raw.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/hydraulics-results-v3-201007/"
#scenarios hydraulics:
raw.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Scnearios_hydraulics_10132020/results-hydraulics/"

files <- list.files(raw.dir, full.names = TRUE)
files.short <- list.files(raw.dir)

#post-processed directory
#out.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/post_processed_hydraulics_201008/"
#scenario output dir
out.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/Results_Hydraulics/Scnearios_hydraulics_10132020/results-hydraulics_postprocessed/"

#loop to go through each file and post-process data
for(i in 1:length(files)){
#for(i in 2:length(files)){
  
  #read in csv raw hydraulics to be processed
  data <- read.csv(files[i])
  
  #find column names and do filters based on depth, velocity, and power
  col.names <- names(data)
  
  ###depth filter####
  #find the col indices for depth
  depth.cols <- grep("Depth", col.names)
  #add last col for max Dpth
  depth.cols <- c(depth.cols, length(col.names))
  #if depth < 0.1 change to 0
  sub <- replace(data[,depth.cols], data[,depth.cols]<0.1, 0)
  #save updated data back into data df
  data[,depth.cols] <- sub
  
  #if velocity < 0, then depth should be zero
  #find the col indices for vel
  vel.cols <- col.names[grep("Vel", col.names)]
  depth.cols2 <- col.names[grep("Depth", col.names)]
  #if vel < 0 change the depth to 0
  #if more than one vel columns (LOB, MC, ROB), replace, else if only one (MC), only replace MC
  if(length(vel.cols)> 1){
    data$Hydr..Depth..ft..LOB[data$Avg..Vel...ft.s..LOB < 0] <- 0
    data$Hydr..Depth..ft..MC[data$Avg..Vel...ft.s..MC < 0] <- 0
    data$Hydr..Depth..ft..ROB[data$Avg..Vel...ft.s..ROB < 0] <- 0
  }else{
    data$Hydr..Depth..ft..MC[data$Avg..Vel...ft.s..MC < 0] <- 0
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
