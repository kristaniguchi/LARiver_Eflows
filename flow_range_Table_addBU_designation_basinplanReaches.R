#Update beneficial use designations, BU_names associated with the focal species

library("tidyverse")


#read in flow ranges for specific reporting nodes and species-lifestage
#ranges <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/FlowRanges_Species_RecUses_Allnodes_03142021.csv") 
ranges <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/FlowRanges_Species_RecUses_Allnodes_04132021.csv") 

#create species column with just first element of Species_Label
species <- sapply(strsplit(ranges$Species," "), `[`, 1) 
ranges$species <- species
#fix double space in Rio Hondo 2 - Above Spreading Grounds
ranges$Reach <- gsub("Rio Hondo  2 - Above Spreading Grounds", "Rio Hondo 2 - Above Spreading Grounds", ranges$Reach)

#Focal species and BU names lookup table
species_BU <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/Species_BU_lookuptable.csv")
#subset to only modeled species
species_BU.sub <- species_BU[species_BU$Modeled =="Y",]
#create output data frame that lists species and beneficial uses
species_BU_out <- data.frame(matrix(NA, length(species_BU.sub$species), 2))
names(species_BU_out) <- c("species", "BU_names")
#loop through to get all BU names associated with each species
for(i in 1:length(species_BU.sub$species)){
  #subset to species i and transpose row to column, filter to BUs with 1 
  row <- data.frame(t(species_BU.sub[i,])) 
  names(row) <- "column"
  row <- filter(row, column == "1")
  #list BU's that have a 1 associated with species i
  BUs <- paste0(row.names(row), collapse=", ")
  #save into output df
  species_BU_out[i,] <- c(species_BU.sub$species[i], BUs)
}

#merge species with range table
ranges <- left_join(ranges, species_BU_out, by="species")

#Reach and BU designations
reach_BU <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/ReportingReaches_MasterPlanReaches_BUDesignations.csv")


#loop through each BU_names in ranges and determine if the BU is designated or not designated, if designated, which categories (E, P, I) <- if multiple then choose E?
#create new output range df to update
ranges_output <- ranges
#blank column for designation BU (Designated or not designated)
ranges_output$Designation_BU <- NA
#blank column for designation category (Existing, Intermittent, Potential)
ranges_output$Designation_Category <- NA

#indicate the rows where it is not NA for BU_names
row.ind <- which(!is.na(ranges$BU_names))
  

for(i in 1:length(row.ind)){
  row.sub <- ranges_output[row.ind[i],]
  reach.i <- row.sub$Reach
  #split beneficial use names 
  bu.names.split <- strsplit(row.sub$BU_names, split=", ")[[1]]
  #find the designated beneficial uses for that reach
  #if LA20 (upstream of sepulveda basin, use LA River Reach 6, diff than Reach 5 but both falls on Los Angeles River Reach 10 reporting reach)
  if(row.sub$Node == "LA20"){
    reach_bu.sub <- reach_BU[reach_BU$Reach == "NODE LA20",] %>% 
      #transpose reach sub
      t() %>% 
      data.frame()
    names(reach_bu.sub) <- "Designation_Category1"
  }else{
    reach_bu.sub <- reach_BU[reach_BU$Reach == reach.i,] %>% 
      #transpose reach sub
      t() %>% 
      data.frame()
    names(reach_bu.sub) <- "Designation_Category1"
  }
  
  #find existing
  existing <- reach_bu.sub %>% 
    filter(Designation_Category1 == "E")
  #find intermittent
  intermit <- reach_bu.sub %>% 
    filter(Designation_Category1 == "I")
  #find potential
  potential <- reach_bu.sub %>% 
    filter(Designation_Category1 == "P")
  #overall designated uses
  designated.uses <- rbind(existing, intermit, potential)
  designated.uses$BU_names <- row.names(designated.uses)
  
  #if bu.names.split is in designated.uses$BU_names, then call that use as designated, else call it not designated 
  if(length(which(bu.names.split %in% designated.uses$BU_names)) > 0) {
    row.sub$Designation_BU <- "Designated"
    #if it is designated determine if it's existing, future, or intermittent
    ind.designated <- which(bu.names.split %in% designated.uses$BU_names)
    #find designation category of the match and keep it as one string
    row.sub$Designation_Category <- paste0(unique(designated.uses$Designation_Category1[ind.designated]), collapse=", ")
    #find 
  } else{
    row.sub$Designation_BU <- "Not Designated"
    row.sub$Designation_Category1 <- NA
  }
  #save the updated row into the output DF
  ranges_output[row.ind[i],] <- row.sub
  
}

#add in the waterbody to the output file
#waterbody reach subset
reach_BU.sub <- reach_BU %>% 
  select(waterbody, Reach)
ranges_output <- ranges_output %>%
  left_join(reach_BU.sub, by="Reach")
  

#write output flow ranges
write.csv(ranges_output, file="C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Reports/Flow recommendations report/FlowRanges_Species_RecUses_Allnodes_04132021.csv", row.names=FALSE) 

