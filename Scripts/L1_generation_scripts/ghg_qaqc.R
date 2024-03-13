# Title: GHG L1 generation (QA/QC) script
# By: Adrienne Breef-Pilz
# Written: 24 Nov 23, 
# Last updated: 08 Mar 24 (ABP)

# Additional notes: This script is included with this EDI package to show which QAQC has already
# been applied to generate these data along with the ghg_functions_for_L1.R which are used here.
# This script is only for internal use by the data creator team and is provided as a reference; it will not run as-is. 

# This function:
# 1. Read in the Maintenance Log and then Raw files 
# 2. Process the files if necessary
# 3. Make Flag Columns and add flags for missing values and negative values 
# 4. Take out values based on the Maintenance Log
# 5. Additional Maintenance
# 6. Save files

# The MDL file is generated from the Analytical chem lab. Make sure you have the right link if the function fails. 
# You need the one that is the reference tank


# Download/load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(lubridate,tidyverse, googledrive, readxl, gsheet)

# The place of the functions used in the function
# source("./Data/DataNotYetUploadedToEDI/Raw_GHG/ghg_functions_for_L1.R")
devtools::source_url("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_GHG/ghg_functions_for_L1.R")

##### Function to qaqc Fluoroprobe data
#'@param directory filepath to raw files 
#'@param maintenance_file filepath to maintenance log
#'@param gdrive Are the files on Google Drive. True or False
#'@param gshared_drive filepath to GOogle Drive
#'@param current_year  Current Year. Must be numeric
#'@param Air_Pressure GS location of headspace preparation conditions
#'@param vial_digitized_sheet GS location of digitiized field sheet with vial numbers
#'@param Rolling_MDL GS location of MDL Serum Vial CH4 CO2 2016 style + Rolling 18dec23
#'@param output_file filepath for output file
#'@param MDL_file filepath for MDL output file
#'@param Vial_Number_Check filepath for vial number output file

ghg_qaqc<-function(directory,
                   maintenance_file,
                   gdrive = F,
                   gshared_drive,
                   current_year,
                   Air_Pressure,
                   vial_digitized_sheet,
                   Rolling_MDL,
                   output_file,
                   MDL_file,
                   Vial_Number_Check){
  # 
  # directory = "./Data/DataNotYetUploadedToEDI/Raw_GHG/"
  # maintenance_file = "./Data/DataNotYetUploadedToEDI/Raw_GHG/GHG_Maintenance_Log.csv"
  # gdrive = T # Are the files on Google Drive. True or False
  # gshared_drive = as_id("1OMx7Bq9_8d6J-7enC9ruPYuvE43q9uKn")
  # current_year = 2024 # Current Year. Must be numeric
  # Air_Pressure = "https://docs.google.com/spreadsheets/d/1YH9MrOVROyOgm0N55WiMxq2vDexdGRgG/edit#gid=462758328"
  # vial_digitized_sheet = "https://docs.google.com/spreadsheets/d/1HoBeXWUm0_hjz2bmd-ZmS0yhgF1WvLenpvwEa8dL008/edit#gid=1256821207"
  # Rolling_MDL = "https://docs.google.com/spreadsheets/d/1AcqbdwbogWtO8QnLH1DmtZd47o323hG9/edit#gid=593287102"
  # output_file = "./Data/DataNotYetUploadedToEDI/Raw_GHG/L1_manual_GHG.csv"
  # MDL_file = "./Data/DataNotYetUploadedToEDI/Raw_GHG/MDL_GHG_file.csv"
  # Vial_Number_Check = "./Data/DataNotYetUploadedToEDI/Raw_GHG/Vial_Number_Check.csv"
  # 
  #### 1. Read in the Maintenance Log and then Raw files ####
  
  ### 1.1 Read in Maintenance file ####
  log_read <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  # For later
  # if(!is.null(current_year)){
  #   log <- log_read%>%
  #     filter(year(TIMESTAMP_start)>current_year)
  # }else{
  #   log <- log_read
  # }
  
  log <- log_read
  
  
  # Name the directory where the full output files are found. Ours are on GitHub 
  mydir <- directory
  
  # list of GHG files on Github
  rfiles <- list.files(path=paste0(mydir,"data/"),pattern="", full.names=TRUE)
  
  
  ### 1.2 Get Files off of Google Drive ####
  
  # Are the files on Google Drive? If so then download missing GHG files
  # This should be False until we figure out how to use GitHub actions and 
  # authentication
  
  if(gdrive==T){
    # Get the file info of the GHG spreadsheets from GoogleDrive
    gdrive_files<-googledrive::drive_ls(path = gshared_drive, 
                                        pattern = "GC", 
                                        type = "xlsx")
    
    
    
    # download output files and put them on GitHub
    
    for(i in 1:nrow(gdrive_files)){
      
      #extract the beginning of the file name to see if a qaqc plot has been made
      dfile<-sub("\\_full.*", "",sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(gdrive_files$name[i])))
      
      
      if(any(grepl(dfile,rfiles))==F){
        # download and put on GitHub
        
        name<-gdrive_files$name[i]
        
        googledrive::drive_download(gdrive_files$id[i], path = paste0(mydir,"data/",name))
        
      }else{
        
      }
    }
    
  }
  
  ### 2. Process the files if necessary ####
  
  ## 2.1 Collate files ### 
  
  # First make sure to include the newly downloaded files if necessary
  # list of GHG files on Github
  rfiles <- list.files(path=paste0(mydir,"data/"),pattern="", full.names=TRUE)
  
  # use purr to read in all the files using the function above
  all<-list.files(path=paste0(mydir,"data/"),pattern="", full.names=TRUE)%>%
    map_df(~ read_ghg_files(.x))
  
  # some timestamps are duplicated between files. Inspect:
  duplicate_timestamps <- all[duplicated(all$date_acquired), ]
  dups <- all %>%
    filter(date_acquired %in% duplicate_timestamps$date_acquired)
  # based on this, we should be okay to remove duplicates
  all <- all %>%
    filter(!duplicated(date_acquired))
  
  # If the headspace_ppm is NA then there was no peak and should be set to Flag 6. 
  # This will happen later but right now want to set to NO_PEAK depending on it is for CO2 or CH4. 
  # This info will live in the notes column until we get to the Flag section
  
  all <- all%>%
    mutate(
      notes = ifelse(is.na(CH4_GC_headspace_ppm) & is.na(CO2_GC_headspace_ppm),"CH4CO2_NO_PEAK", 
                     ifelse(is.na(CH4_GC_headspace_ppm), "CH4_NO_PEAK",
                            ifelse(is.na(CO2_GC_headspace_ppm), "CO2_NO_PEAK", notes))))
  
  #### 2.2 Assign Air temp and lab pressure for time of lab sampling ####
  # read in the file Bobbie created for air temp and pressure
  
  # read in the Air temp and lab pressure Google Sheet Bobbie Maintains
  temp_pres <- gsheet::gsheet2tbl(Air_Pressure)
  
  # take out the first 9 rows because we don't need them
  temp_pres<-temp_pres[9:nrow(temp_pres),1:4]
  
  # Rename the columns
  names(temp_pres)<- c("Date","lab_temp", "weather_station_bp", "notes")
  
  # Reformat the date and make the columns numeric
  temp_pres2 <- temp_pres%>%
    mutate(
      Date= as.Date(Date, format = "%d-%b-%y"),
      lab_temp=as.numeric(lab_temp),
      weather_station_bp=as.numeric(weather_station_bp))%>%
    select(-notes)
  
  
  #### 2.2 Assign the lab temp and BP based on the observations ####
  
  # set date_acquired as just a date. The dates should line up but just in case they don't 
  # let's take the closest observation
  all$date_acquired_comp <- as.Date(all$date_acquired)
  
  # find the closest date and then join the two data frames
  by <- join_by(closest(date_acquired_comp >= Date))
  fg <- full_join(all, temp_pres2, by)
  
  fg <- fg %>% 
    drop_na(date_acquired) 
  
  
  #### 2.3 Calculate the GHG concentration from ppm to umolL ####
  
  # Use the ghg_concentration function in the source script to get umolL
  
  ghg_con <- ghg_concentration(raw_file = fg)
  
  
  #### 2.4 Match up vial numbers to Sampling Locations #####
  
  # Read in the Digitized GHG vials spreadsheet from Google Drive
  
  site_info <- gsheet::gsheet2tbl(vial_digitized_sheet)
  
  # convert the date and DateTime into usable form
  site_info <- site_info %>%
    #dplyr::rename(clean_vial_number="Vial Number")%>%
    mutate(
      clean_vial_number= `Vial Number`,
      DateTime=parse_date_time(DateTime, orders = c('ymd HMS','ymd HM','ymd','mdy')),
      Date=as.Date(DateTime),
      Date_upper=Date+4)
  
  
  # samples that were take less than 3 days from the process date. Usually they are processed the next day
  # use the join_by and between to say that lab processing had to happened after the samples were collected
  # but less that 4 days after collection. That is what Date_upper is. 
  
  by<-join_by("clean_vial_number", between(date_acquired_comp ,Date, Date_upper))
  ab<-full_join(ghg_con, site_info, by)
  
  # Check out how that worked and it did!!
  
  work_check <- ab%>%
    select(Reservoir, Site, Depth_m, Date.y, date_acquired, Date_upper, vial_number,`Vial Number`)%>%
    dplyr::rename(field_date=Date.y,
                  lab_date=date_acquired,
                  upper_date=Date_upper,
                  lab_vial_number=vial_number,
                  field_vial_number=`Vial Number`)%>%
    distinct()
  
  work_check$lab_date = as.Date(as.character(with_tz(work_check$lab_date, "America/Nome")))
  
  # Make a list of observations that don't fall with in the 3 days after collection
  
  out_range <- work_check%>%
    filter(is.na(lab_vial_number)|is.na(field_vial_number))%>%
    mutate(miss = ifelse(is.na(field_date),"no_field_obs", "no_lab_obs"),
           com_date=coalesce(lab_date, field_date))%>%
    select(com_date, miss, field_date, lab_date, upper_date, field_vial_number, lab_vial_number)%>%
    dplyr::arrange(., com_date)
  
  #Warn about missing information
  
  #Missing field info
  if(nrow(out_range[out_range$miss == "no_field_obs",]) > 0){
    format_to_print_field <- out_range %>%
      filter(miss == "no_field_obs") %>%
      mutate(message = paste0("Date: ", com_date, ", Vial Number: ", lab_vial_number)) %>%
      summarize(message = paste(message, collapse = "\n"))
    
    warning("There are ", nrow(out_range[out_range$miss == "no_field_obs",]), 
            " samples without field observations. 
            These are the samples with missing information:\n",
            format_to_print_field$message)
  }
  
  #Missing lab info
  if(nrow(out_range[out_range$miss == "no_lab_obs",]) > 0){
    format_to_print_lab <- out_range %>%
      filter(miss == "no_lab_obs") %>%
      mutate(message = paste0("Date: ", com_date, ", Vial Number: ", field_vial_number)) %>%
      summarize(message = paste(message, collapse = "\n"))
    
    warning("There are ", nrow(out_range[out_range$miss == "no_lab_obs",]), 
            " samples without lab observations. 
            These are the samples with missing information:\n",
            format_to_print_lab$message)
  }
  
  
  # Make a working data frame for QAQC that goes into QAQC
  working_final_df <- ab%>%
    distinct() %>%
    mutate(field_lab_date_check = coalesce(DateTime, date_acquired),
           Notes = ifelse(is.na(Notes), "", Notes),
           notes = ifelse(is.na(notes), "", notes),
           Notes = paste0(notes, Notes)) %>% 
    select(Reservoir, Site, DateTime, Depth_m,`Vial Number`,CH4_umolL, CO2_umolL, Notes)
  
  
  ### QAQC Section ###
  
  ### 3. Make Flag Columns and add flags for missing values and negative values ####
  
  #  Flag value for methane concentration;
  #  0 = the samples and replicates are good; 
  #  1 = Sample was either not collected or was not retained due to issues with the GC; 
  #  2 = Sample was below the method detection limit; or was negative and changed to 0
  #  3 = The difference between the reps are above the limit of quantification and >30% and <50% different from each other. 
  #     Both replicates were retained but flagged; 
  #  4 = The difference between the reps are above the limit of quantification and >50% different from each other. 
  #     Both replicates were retained but flagged
  #  6 = No peak detected and set to 0
  
  
  
  #### 3.1  for loop to create flag columns ####
  for(j in colnames(working_final_df %>% select(DateTime, CH4_umolL:CO2_umolL))) { 
    #for loop to create new columns in data frame
    working_final_df[,paste0("Flag_",colnames(working_final_df[j]))] <- 0 #creates flag column + name of variable
    working_final_df[c(which(is.na(working_final_df[, j]))), paste0("Flag_",colnames(working_final_df[j]))] <- 1 #puts in flag 1 if value not collected
  }
  
  ### 3.2 Flag for No Peak ####
  # Let's add the Flag for No peaks and change the NA to 0.
  # I like to bounce back and forth between base R and tidyverse
  
  working_final_df <- working_final_df%>%
    mutate(
      Flag_CO2_umolL = ifelse(grepl("CO2_NO_PEAK|CH4CO2_NO_PEAK", Notes), 
                              6, Flag_CO2_umolL),
      Flag_CH4_umolL = ifelse(grepl("CH4_NO_PEAK|CH4CO2_NO_PEAK", Notes), 
                              6, Flag_CH4_umolL)
    )
  
  #### 3.3  Change negative values to 0 ####
  for(k in colnames(working_final_df %>% select(CH4_umolL:CO2_umolL))) { 
    #for loop to create new columns in data frame
    working_final_df[c(which(working_final_df[,k] < 0)), paste0("Flag_", colnames(working_final_df[k]))] <- 2
    working_final_df[c(which(working_final_df[,k] < 0)), k] <- 0 #replaces value with 0
    working_final_df[c(which(working_final_df[,paste0("Flag_", colnames(working_final_df[k]))] == 6)), k] <- 0
  }
  
  ### 3.4 Drop samples that don't have a depth (these are samples that did not have a match in site info) ####
  working_final_df <- working_final_df%>%
    filter(!is.na(Depth_m)) %>%
    group_by(Reservoir, Site, DateTime, Depth_m) %>% 
    mutate(Rep = row_number())%>%
    ungroup()
  
  ### 3.5 More than 2 reps per a depth ####
  # If there are three samples per a time then one of the obs is an NA and drop it
  
  # Get data frame with just the observations that have more than 2 reps
  a <- working_final_df[which(working_final_df$Rep > 2),]
  
  # label those observations that have over 2 reps and the obs is an NA
  # Note from ASL 19 Dec 2023 -> there is only one sample being flagged here.  
  # Its a sample that was run twice because there was an issue the first time. 
  # I'd strongly recommend putting this in the maintenance log to remove the 
  # first run, rather than systematically dealing with all cases where there are
  # 3 reps in the same way. Let me know if I can help with this!
  for(g in 1:nrow(a)){
    working_final_df[which(working_final_df$DateTime==ymd_hms(a$DateTime[g])& 
                             (is.na(working_final_df$CH4_umolL)|is.na(working_final_df$CO2_umolL))), "Remove"]<- "Remove"
    
  }
  
  # Now remove those rows and then re do the reps so there are only 2 per depth
  working_final_df <- working_final_df%>%
    filter(is.na(Remove))%>%
    select(-Remove)%>%
    group_by(Reservoir, Site, DateTime, Depth_m) %>% 
    mutate(Rep = row_number())%>%
    ungroup()
  
  ### 3.6 Create a DateTime Flag for non-recorded times ####
  # (i.e., 12:00) and set to noon
  # Convert time that are in 12 hours to 24 hours
  raw_df <- working_final_df %>% 
    mutate(Time = format(DateTime,"%H:%M:%S"),
           Flag_DateTime = ifelse(Time == "00:00:00", 1, 0), # Flag if set time to noon
           Time = ifelse(Time == "00:00:00", "12:00:00",Time),
           Date = as.Date(DateTime),
           DateTime = ymd_hms(paste0(Date, "", Time)),
           Hours = hour(DateTime),
           DateTime = ifelse(Hours<5, DateTime + (12*60*60), DateTime), # convert time to 24 hour time
           DateTime = as_datetime(DateTime))%>% # time is in seconds put it in ymd_hms
    select(-c(Time, Date, Hours))
  
  
  ### 4. Take out values based on the Maintenance Log ####
  
  ### 4.1 Set up the Values to be used ####
  # modify raw_df based on the information in the log
  
  for(i in 1:nrow(log)){
    
    ### get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]
    
    ### Get the Reservoir Name
    
    Reservoir <- log$Reservoir[i]
    
    ### Get the Site Number
    
    Site <- as.numeric(log$Site[i])
    
    ### Get the depth
    
    Depth <- as.numeric(log$Depth[i])
    
    ### Get the vial number
    
    vial <- as.numeric(log$vial_number[i])
    
    ### Get the Maintenance Flag
    
    flag <- log$flag[i]
    
    ### Get the new value for a column or an offset.
    
    update_value <- as.numeric(log$update_value[i])
    
    
    ### Get the names of the columns affected by maintenance
    
    colname_start <- log$start_parameter[i]
    colname_end <- log$end_parameter[i]
    
    ### if it is only one parameter parameter then only one column will be selected
    
    if(is.na(colname_start)){
      
      maintenance_cols <- colnames(raw_df%>%select(all_of(colname_end)))
      
    }else if(is.na(colname_end)){
      
      maintenance_cols <- colnames(raw_df%>%select(all_of(colname_start)))
      
    }else{
      maintenance_cols <- colnames(raw_df%>%select(colname_start:colname_end))
    }
    
    ### Get the name of the flag column
    
    flag_cols <- paste0("Flag_", maintenance_cols)
    
    ### remove any Flag columns that don't exsist because we don't have a flag column for them
    # and they get removed before publishing
    
    #flag_col = flag_col[!flag_col %in% c(COLUMN NAMES HERE)]
    
    ### Getting the start and end time vector to fix. If the end time is NA then it will put NAs
    # until the maintenance log is updated
    
    if(is.na(end)){
      # If there the maintenance is on going then the columns will be removed until
      # and end date is added
      Time <- raw_df$DateTime >= start
      
    }else if (is.na(start)){
      # If there is only an end date change columns from beginning of data frame until end date
      Time <- raw_df$DateTime <= end
      
    }else {
      
      Time <- raw_df$DateTime >= start & raw_df$DateTime <= end
    }
    
    ### 4.2 Actually remove values in the maintenance log from the data frame 
    ## This is where information in the maintenance log gets removed. 
    # UPDATE THE IF STATEMENTS BASED ON THE NECESSARY CRITERIA FROM THE MAINTENANCE LOG
    
    # replace relevant data with NAs and set flags while maintenance was in effect
    if(flag==1){ # The observations are changed to NA for maintenance or other issues found in the maintenance log
      
      if (is.na(Depth) & is.na(vial)){
        raw_df[Time, maintenance_cols] <- NA
        raw_df[Time, flag_cols] <- flag
        
      } else if (is.na(vial)){
        # Just use the time and depth for indexing
        raw_df[c(which(Time & (raw_df[,"Depth_m"] = Depth))), maintenance_cols] <- NA
        raw_df[c(which(Time & (raw_df[,"Depth_m"] = Depth))), flag_cols] <- flag
        
      } else if (is.na(Depth)){
        # Just use the time and vial number for indexing
        raw_df[c(which(Time & (raw_df[,"Vial Number"] == vial))), maintenance_cols] <- NA
        raw_df[c(which(Time & (raw_df[,"Vial Number"] == vial))), flag_cols] <- flag
        
      } else {
        
        # Use time, depth and vial number for indexing
        
        raw_df[c(which(Time & (raw_df[,"Vial Number"] == vial) & (raw_df[,"Depth_m"] == Depth))),
               maintenance_cols] <- NA
        raw_df[c(which(Time & (raw_df[,"Vial Number"] == vial) & (raw_df[,"Depth_m"] == Depth))),
               flag_cols] <- flag
        
      }
      
    } else if (flag==2){
      # This one is below minimum detection level and most likely won't be in the maintenance log
      
    } else {
      warning("Flag used not defined in the L1 script. Talk to Austin and Adrienne if you get this message")
      next
    }
    #next
  }
  
  
  #### 5. Additional Maintenance ####
  
  # clean up columns we no longer need
  ghg <- raw_df%>%
    select(Reservoir, Site, DateTime, Depth_m, Rep, CH4_umolL, CO2_umolL, Flag_DateTime, Flag_CH4_umolL, Flag_CO2_umolL)
  
  ### 5.1 Calculate the Minimum detection ####
  
  # Read in the Google Drive file of rolling MDL Serum Vial CH4 CO2 2016 style + Rolling
  # use the charting reference tank.
  
  MDL_from_ref_tank <- gsheet::gsheet2tbl(Rolling_MDL)
  
  # take out the first 42 rows because we don't need them
  # and only want the date column and the current seasonal ppm for CH4 and CO2
  MDL_from_ref_tank<-MDL_from_ref_tank[42:nrow(MDL_from_ref_tank), c(3,46:47)]
  
  # Rename the columns
  names(MDL_from_ref_tank)<- c("Date","CH4_GC_headspace_ppm", "CO2_GC_headspace_ppm")
  
  # Reformat the date, get the headspace concentrations as numeric, and only take the columns with a date
  MDL_from_ref_tank <- MDL_from_ref_tank%>%
    drop_na(Date)%>% # take out anything that doesn't have a depth
    mutate(
      Date= as.POSIXct(Date, format = "%m/%d/%y %H:%M"),
      CH4_GC_headspace_ppm = as.numeric(CH4_GC_headspace_ppm),
      CO2_GC_headspace_ppm = as.numeric(CO2_GC_headspace_ppm))
  
  
  ## Filter because we only want the last 2 years
  
  # Get end date as system time
  end_date <- Sys.Date()
  # Start date is years prior. Can change later if we want
  start_date <- end_date - years(2)
  
  # filter for only the last 2 years
  current <- MDL_from_ref_tank%>%
    filter(Date>=start_date & Date<=end_date)
  
  # Get the ppm for CH4 and CO2 of the MDL
  MDL_ppm <- current%>%
    mutate(
      # standard deviation for CH4 and
      CH4_standard_dev = sd(CH4_GC_headspace_ppm),
      CO2_standard_dev = sd(CO2_GC_headspace_ppm),
      
      # find the t value based on the number of observations
      # To find the critical t-value for a 98% confidence interval with x degrees freedom:
      CH4_tvalue = qt(1-0.02/2, (length(CH4_GC_headspace_ppm)-1)),
      CO2_tvalue = qt(1-0.02/2, (length(CO2_GC_headspace_ppm)-1)),
      
      # Get the MDL in ppm of tvalue * standard deviation
      CH4_GC_headspace_ppm = CH4_tvalue * CH4_standard_dev,
      CO2_GC_headspace_ppm = CO2_tvalue * CO2_standard_dev,
      
      # add in standard lab temperature and bp
      lab_temp = 20,
      weather_station_bp = 29.99
    )%>%
    select(-Date)%>%
    distinct()
  
  
  ### 5.12 Calculate the concentration in umol/L ####
  
  # Use the ghg_concentration function in the source script to get MDL
  
  MDL_umolL <- ghg_concentration(raw_file = MDL_ppm)
  
  # Create a table with the MDL info that gets save every time there is an update
  
  MDL_umolL <- MDL_umolL%>%
    select(CH4_GC_headspace_ppm, CO2_GC_headspace_ppm, CH4_standard_dev, CO2_standard_dev,
           CH4_tvalue, CO2_tvalue, CH4_umolL, CO2_umolL)
  
  # Assign them a name to use below
  CH4_umolL_MDL <- MDL_umolL$CH4_umolL
  CO2_umolL_MDL <- MDL_umolL$CO2_umolL
  
  # Flag with 2 if less than the MDL but don't change the flag if observation was negative and changed to 0
  
  ghg <- ghg%>%
    mutate(
      Flag_CH4_umolL = ifelse(CH4_umolL>0 & CH4_umolL<CH4_umolL_MDL & !is.na(CH4_umolL),
                              2, Flag_CH4_umolL),
      Flag_CO2_umolL = ifelse(CO2_umolL>0 & CO2_umolL<CO2_umolL_MDL & !is.na(CO2_umolL),
                              2, Flag_CO2_umolL)
    )
  
  
  ### 5.2 Calculating the difference and percent difference between the reps ####
  
  ## Separate into rep 1 and rep2
  ghgs_rep1 <- ghg %>%
    filter(Rep == 1) %>%
    dplyr::rename(CH4_umolL_rep1 = CH4_umolL,
                  CO2_umolL_rep1 = CO2_umolL,
                  Flag_DateTime_rep1 = Flag_DateTime,
                  Flag_CH4_umolL_rep1 = Flag_CH4_umolL,
                  Flag_CO2_umolL_rep1 = Flag_CO2_umolL)
  
  ghgs_rep2 <- ghg %>%
    filter(Rep == 2) %>%
    rename(CH4_umolL_rep2 = CH4_umolL,
           CO2_umolL_rep2 = CO2_umolL,
           Flag_DateTime_rep2 = Flag_DateTime,
           Flag_CH4_umolL_rep2 = Flag_CH4_umolL,
           Flag_CO2_umolL_rep2 = Flag_CO2_umolL)
  
  ghgs_reps <- left_join(ghgs_rep1,ghgs_rep2,by=c("DateTime","Site","Depth_m","Reservoir"))
  
  # Add '2' when rep 2 is NA
  ghgs_reps <- ghgs_reps %>%
    mutate(Rep.y = ifelse(is.na(Rep.y),2,Rep.y), 
           Flag_DateTime_rep2 = ifelse(is.na(Flag_DateTime_rep2), 
                                       Flag_DateTime_rep1,
                                       Flag_DateTime_rep2)) # and give it the same DateTime flag as rep1 if it is missing
  
  ## Calculate percent difference between reps
  ghgs_reps <- ghgs_reps %>%
    mutate(CH4_pdiff = round((abs(CH4_umolL_rep1-CH4_umolL_rep2)/(abs(CH4_umolL_rep1+CH4_umolL_rep2)/2))*100)) %>%
    mutate(CH4_diff = abs(CH4_umolL_rep1-CH4_umolL_rep2))
  
  ghgs_reps <- ghgs_reps %>%
    mutate(CO2_pdiff = round((abs(CO2_umolL_rep1-CO2_umolL_rep2)/(abs(CO2_umolL_rep1+CO2_umolL_rep2)/2))*100)) %>%
    mutate(CO2_diff = abs(CO2_umolL_rep1 - CO2_umolL_rep2))
  
  # Now that we calculated the differences then seperate rep 1 and rep2 and then bind them on top of each other again
  
  ghg_rep1 <- ghgs_reps %>%
    select(Reservoir, Site, DateTime,Depth_m,CH4_umolL_rep1,CO2_umolL_rep1,
           CH4_pdiff,CH4_diff,CO2_pdiff,CO2_diff, Flag_DateTime_rep1,
           Flag_CH4_umolL_rep1, Flag_CO2_umolL_rep1)%>%
    mutate(Rep = 1) %>%
    dplyr::rename(CH4_umolL = CH4_umolL_rep1,
                  CO2_umolL = CO2_umolL_rep1,
                  Flag_DateTime = Flag_DateTime_rep1,
                  Flag_CH4_umolL = Flag_CH4_umolL_rep1,
                  Flag_CO2_umolL = Flag_CO2_umolL_rep1)
  
  ghg_rep2 <- ghgs_reps %>%
    select(Reservoir, Site, DateTime,Depth_m,CH4_umolL_rep2,CO2_umolL_rep2,
           CH4_pdiff,CH4_diff,CO2_pdiff,CO2_diff, Flag_DateTime_rep2,
           Flag_CH4_umolL_rep2, Flag_CO2_umolL_rep2) %>%
    mutate(Rep = 2) %>%
    dplyr::rename(CH4_umolL = CH4_umolL_rep2,
                  CO2_umolL = CO2_umolL_rep2,
                  Flag_DateTime = Flag_DateTime_rep2,
                  Flag_CH4_umolL = Flag_CH4_umolL_rep2,
                  Flag_CO2_umolL = Flag_CO2_umolL_rep2)
  
  ghg_all <- bind_rows(ghg_rep1,ghg_rep2)
  
  ghg_all <-  ghg_all %>%
    arrange(DateTime,Reservoir,Depth_m)
  
  ### 5.21 Flag based on the difference between the reps ####
  
  # This section gives a Flag or 3 or 4 depending on how different the reps are.
  # If the data are already Flagged then we leave previous flag in
  
  ghg_all <- ghg_all %>%
    mutate(Flag_CH4_umolL = ifelse((CH4_pdiff>=50 & CH4_diff>=CH4_umolL_MDL*3 &Flag_CH4_umolL == 0) | 
                                     is.na(CH4_pdiff), 4,
                                   ifelse(CH4_pdiff<=50 & CH4_pdiff>=30 & CH4_diff>=CH4_umolL_MDL*3 &
                                            Flag_CH4_umolL == 0, 3, Flag_CH4_umolL)),
           
           Flag_CO2_umolL = ifelse((CO2_pdiff>=50 & CO2_diff>=CO2_umolL_MDL*3 & Flag_CO2_umolL == 0) |
                                     is.na(CH4_pdiff), 4,
                                   ifelse(CO2_pdiff<=50 & CO2_pdiff>=30 & CO2_diff>=CO2_umolL_MDL*3 &
                                            Flag_CO2_umolL == 0,3,Flag_CO2_umolL)))
  
  
  ### 5.3 Check that all NA values are flagged ####
  
  for(m in colnames(ghg_all%>%select(DateTime, CH4_umolL:CO2_umolL))) {
    ghg_all[c(which(is.na(ghg_all[,m]))), paste0("Flag_",colnames(ghg_all[m]))] <- 1 # puts in flag 1 if value not collected
  }
  
  
  # ## identify latest date for data on EDI (need to add one (+1) to both dates because we want to exclude all possible start_day data and include all possible data for end_day)
  # package_ID <- 'edi.551.7'
  # eml <- read_metadata(package_ID)
  # date_attribute <- xml_find_all(eml, xpath = ".//temporalCoverage/rangeOfDates/endDate/calendarDate")
  # last_edi_date <- as.Date(xml_text(date_attribute)) + lubridate::days(1)
  
  # ghg_all <- ghg_all |> filter(DateTime > last_edi_date)
  
  ### 6. Save files ####
  
  # Make final data frame with only the columns we want
  ghg_final <- ghg_all%>%
    select(Reservoir, Site, DateTime, Depth_m, Rep, CH4_umolL, CO2_umolL,
           Flag_DateTime, Flag_CH4_umolL, Flag_CO2_umolL)
  
  # Write the MDL file
  write.csv(MDL_umolL, MDL_file, row.names = F)
  
  # Write an out of range file to check if vial numbers were messed up
  write.csv(out_range, Vial_Number_Check, row.names = F)
  
  # Write the L1 file
  write.csv(ghg_final, output_file, row.names = F)
  
}

# Use the function here

ghg_qaqc(directory = "./Data/DataNotYetUploadedToEDI/Raw_GHG/",
         maintenance_file = "./Data/DataNotYetUploadedToEDI/Raw_GHG/GHG_Maintenance_Log.csv",
         gdrive = F, # Are the files on Google Drive. True or False
         gshared_drive = as_id("1OMx7Bq9_8d6J-7enC9ruPYuvE43q9uKn"),
         current_year = 2024, # Current Year. Must be numeric
         Air_Pressure = "https://docs.google.com/spreadsheets/d/1YH9MrOVROyOgm0N55WiMxq2vDexdGRgG/edit#gid=462758328",
         vial_digitized_sheet = "https://docs.google.com/spreadsheets/d/1HoBeXWUm0_hjz2bmd-ZmS0yhgF1WvLenpvwEa8dL008/edit#gid=1256821207",
         Rolling_MDL = "https://docs.google.com/spreadsheets/d/1AcqbdwbogWtO8QnLH1DmtZd47o323hG9/edit#gid=593287102",
         output_file = "./Data/DataNotYetUploadedToEDI/Raw_GHG/L1_manual_GHG.csv",
         MDL_file = "./Data/DataNotYetUploadedToEDI/Raw_GHG/MDL_GHG_file.csv",
         Vial_Number_Check = "./Data/DataNotYetUploadedToEDI/Raw_GHG/Vial_Number_Check.csv")

## Call healthcheck
#RCurl::url.exists("https://hc-ping.com/46cfa878-1fd4-4ebb-abcc-87f84071c9ab", timeout = 5)
