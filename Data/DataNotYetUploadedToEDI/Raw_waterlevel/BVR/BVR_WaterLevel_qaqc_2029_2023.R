# Title: Clean BVR Waterlevel
# Author: Adrienne Breef-Pilz
# Created: 21 March 2024
# Edited: 

# This script reads in the digitized water from Google Sheets
# Checks to make sure the values are in the correct range
# Create the DateTime column and flag when the time is not recorded. 

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, gsheet)

# Read in the Google Drive file

 bvr_wl <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1DDF-KZPuGBOjO2rB-owdod14N9bRs00XSZmmaASO7g8/edit#gid=0')
 
 # Sort columns and order by datetime
 
 bvr_wl <- bvr_wl[order(bvr_wl$Date),]
 
 
 # check for duplicates
 
 dups <- bvr_wl[duplicated(bvr_wl),]
 
 # add flag column, flag if datetime not recorded, and flag if observation below the staff gauge
 
 bvr <- bvr_wl%>%
   dplyr::select(Reservoir, Site, Date, Source, Obs_waterlevel_from_full_pond_ft, WaterLevel_m, Comments)%>%
   dplyr::rename(DateTime = Date)%>%
   dplyr::mutate(Time = format(DateTime,"%H:%M:%S"),
          Flag_DateTime = ifelse(Time == "00:00:00", 1, 0), # Flag if set time to noon
          Time = ifelse(Time == "00:00:00", "12:00:00",Time),
          Date = as.Date(DateTime),
          DateTime = ymd_hms(paste0(Date, "", Time)))%>% # time is in seconds put it in ymd_hms
   dplyr::mutate(
     #Flag_Obs_waterlevel_from_full_pond_ft = 0,
     Flag_Obs_waterlevel_from_full_pond_ft = ifelse(grepl("below",Comments) & is.na(Obs_waterlevel_from_full_pond_ft),
                                                    2, 0),
     WaterLevel_m = round(WaterLevel_m, digits = 2))%>%
   dplyr::select(Reservoir, Site, Source, DateTime, Obs_waterlevel_from_full_pond_ft, 
                 WaterLevel_m, Flag_DateTime, Flag_Obs_waterlevel_from_full_pond_ft)
 
 # convert the datetime to a character and save
 
 bvr$DateTime <- as.character(format(bvr$DateTime))
 
 write_csv(bvr, "./Data/DataNotYetUploadedToEDI/Raw_waterlevel/BVR/BVR_WaterLevel_2009_2023.csv")
 