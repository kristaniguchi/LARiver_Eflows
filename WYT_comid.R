#Determine WYT for COMIDs - Reporting Nodes

library("tidyverse")
install.packages("fabricatr")
library("fabricatr")

#Reporting node description file
reporting.node.names <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/200827_draft-reporting-nodes-v4.csv")
reporting.node.names <- rename(reporting.node.names, ReportingNode = SWMM.Node )

#COMID for each reporting node
comid.node <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/SpatialData/reporting-nodes_082020/reportingnodes_COMID.csv")
unique.comid <- unique(comid.node$COMID)

#WYT based on COMID
WYT <- read.csv("L:/CA  E-flows framework_ES/Misc/socal_statewidemodel_evaluation/WYT_ALL_COMIDS.csv")

#subset based on COMID
comid.subset <- WYT[WYT$COMID %in% unique.comid,]
#filter to WY 2011-2017
comid.WYT <-  filter(comid.subset, year > 2010 & year < 2018)

#sort by year to see if same designation for each year
summary <- data.frame(aggregate(comid.WYT, by = comid.WYT[c('year', 'WYT')], length))


#missing WY 2015-2017 --> need to add it to Db to determine type
#only look at outlet F319 --> get mean annual discharge by WY and designate by terciles to get WYT
#read in flow at outlet F319 gage data

flow <- read.csv("C:/Users/KristineT/SCCWRP/LA River Eflows Study - General/Data/RawData/FlowData_from_Jordy/Flow/Processed/F319.csv", skip=1)
#calc mean daily Q
#format date
date.time <- paste(flow$Date, flow$Time)

unique.dates <- unique(flow$Date)
################

#empty mean daily flow vector
flow.daily <- NA

for (j in 1:length(unique.dates)){
  sub.day <- flow[flow$Date  == unique.dates[j],]
  flow.daily[j] <- mean(sub.day$Flow, na.rm = TRUE)
}
#create new flow frame with date and mean daily flow to go into FFC
data.daily <- data.frame(cbind(unique.dates, flow.daily))

#get water year
wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates, format="%m/%d/%Y")
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

WY <- wtr_yr(unique.dates, start_month=10)

#calc mean annual flow
av.annual <- aggregate(as.numeric(data.daily$flow.daily), by=list(WY), FUN=mean, na.action = na.omit)

#find whole COMID subset
22518274 
subset.outlet <- filter(comid.subset, COMID == 22518274) %>% 
  filter(year > 2001 & year < 2016) 


#######WTY by PRISM annual rainfall 1950-2019 at outlet of LAR Watershed###
#PRISM Monthly totals to do WYT by WY ###
prism2 <- read.csv("C:/Users/KristineT/Documents/Git/LARiver_Eflows/PRISM_ppt_stable_4km_194910_201909_33.7747_-118.2073_monthly_outlet.csv", skip=10)

#get water year
wtr_yr <- function(month, year, start_month=10) {
  # Convert dates into POSIXlt
  #dates.posix = as.POSIXlt(dates, format="%Y-%m")
  # Year offset
  offset = ifelse(month >= start_month , 1, 0)
  # Water year
  adj.year = year + offset
  # Return the water year
  adj.year
}

#get month and year to determine WY
month <- as.numeric(substr(prism2$Date, start = 6, stop = 7))
year <- as.numeric(substr(prism2$Date, start = 1, stop = 4))
#determine water year
wateryear <- wtr_yr(month, year, start_month=10)
#aggregate to get total annual precip by WY
WY.precip <- aggregate(as.numeric(prism2$ppt..inches.), by=list(wateryear), FUN=sum)
#terciles to get dry, moderate, wet year designation (WYT)
WY.precip$tercile <- split_quantile(WY.precip$x, type=3)
#set WYT names for each tercile
WY.precip$WYT <- as.character(WY.precip$tercile)
WY.precip$WYT  <- gsub("1", "DRY", WY.precip$WYT)
WY.precip$WYT  <- gsub("2", "MODERATE", WY.precip$WYT)
WY.precip$WYT  <- gsub("3", "WET", WY.precip$WYT)
#rename columns
WYT_data <- rename(WY.precip, total.precip.in = x) %>% 
  rename(WY = Group.1)
#write WYT designation
write.csv(WYT_data, file="C:/Users/KristineT/Documents/Git/LARiver_Eflows/LAR_WYT_1950_2019_ppt.csv")
