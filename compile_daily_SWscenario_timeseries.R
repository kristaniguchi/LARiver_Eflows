#loop through stormdrain reduction scenario flow modeling results and extract only the daily scenarios


#stormwater directory
sw_dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/Outputs_Share/Stormwater_Stormdrain_Dryweather_Scenarios/"
#list subfolder directories to loop through
sub.dir.foldername <- list.files(sw_dir, pattern="WRP")
sub.dir.fullname <- list.files(sw_dir, pattern="WRP", full.names = TRUE)

#output model directory
output.dir <- "C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/Final_Data_Products/Physical/Modeled_flow_data/Stormwater_Stormdrain_Dryweather_Scenarios/"

#loop through sw_dir and copy daily flow timeseries, save into parent folder name in output directory
for(i in 1:length(sub.dir.foldername)){
  #list files in subdir i
  daily.dir <- list.files(sub.dir.fullname[i], pattern="daily", full.names = TRUE)
  #list all daily csv files with full path to be copied
  daily.files.list <- list.files(daily.dir, pattern="csv", full.names = TRUE)
  
  #create new output directory with name of subdir
  new.dir.name <- paste0(output.dir, sub.dir.foldername[i])
  dir.create(new.dir.name)
  
  #copy files from daily.dir and paste to new.dir.name
  file.copy(daily.files.list, new.dir.name, overwrite = TRUE, copy.mode=TRUE, recursive = FALSE, copy.date = TRUE)
  
}
