# Title: Metals data wrangling script
# Author: Cece Wood
# Date: 18JUL23
# Edit: 07 Mar. 24 A. Breef-Pilz

# Purpose: convert metals data from the ICP-MS lab format to the format needed
# for publication to EDI

# 1. Read in Maintenance Log and Sample ID Key
# 2. Compile the files from Jeff and add Site information
# 3. Read in the Time of sampling sheet and add to data frame
# 4. Read in MRL and add flags
# 5. Use Maintenance Log to flag or change observations
# 6. Switch observations if total and soluble samples were mixed up
# 7. Save files

# Read in packages
pacman::p_load("tidyverse", "lubridate", "gsheet", "rqdatatable", "hms")

metals_qaqc <- function(directory = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/2023/",
                        historic = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/historic_raw_2014_2019_w_unique_samp_campaign.csv",
                        sample_ID_key = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv", 
                        maintenance_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Metals_Maintenance_Log.csv",
                        sample_time = "https://docs.google.com/spreadsheets/d/1MbSN2G_NyKyXQUEzfMHmxEgZYI_s-VDVizOZM8qPpdg/edit#gid=0",
                        MRL_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/MRL_metals.txt",
                        outfile = "./Data/DataNotYetUploadedToEDI/Metals_Data/metals_L1.csv", # put Null to return the file
                        ISCO_outfile = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/ISCO_metals_L1.csv", # put Null to return the file
                        start_date = NULL,
                        end_date = NULL)
                        
{
  
  # # These are so I can run the function one step at a time and figure everything out.
  # # Leave for now while still in figuring out mode
  # directory = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/"
  # historic = "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/historic_raw_2014_2019_w_unique_samp_campaign.csv"
  # sample_ID_key = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Scripts/Metals_Sample_Depth.csv"
  # maintenance_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/Metals_Maintenance_Log.csv"
  # sample_time = "https://docs.google.com/spreadsheets/d/1MbSN2G_NyKyXQUEzfMHmxEgZYI_s-VDVizOZM8qPpdg/edit#gid=0"
  # MRL_file = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Metals_Data/MRL_metals.txt"
  # #outfile = "./Data/DataNotYetUploadedToEDI/Metals_Data/metals_L1.csv"
  # ISCO_outfile = "./Data/DataNotYetUploadedToEDI/FCR_ISCO/ISCO_metals_L1.csv"

  #### 1. Read in Maintenance Log and Sample ID Key ####
  
  # Read in Maintenance Log
  
  log <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    Sample_Date = col_date("%Y-%m-%d"),
    flag = col_integer(),
    Sample_ID = col_integer(),
    Site = col_number(),
    Depth_m = col_number()
  ))
  
  # Read in Sample ID Key 
  
  #read in metals ID, reservoir, site, depth, and total/soluble key
  metals_key <- read_csv(sample_ID_key)|> 
    dplyr::rename(Depth_m =`Sample Depth (m)`,
                  Sample_ID = Sample)

    
  
  ### 2. Read in and combine all metals files ####
  
  # make a function that reads in the files and takes the columns we want
  read_metals_files <- function(FILES){
    
  al <- read_csv(FILES, skip = 3, col_names = T, show_col_types = F)|>
    dplyr::rename(Date_ID = `...1`)|>
    select(starts_with("Date"), contains("(STDR"))|> # only select the columns that are the date column and end with (STDR) which is how the samples are labeled 
    drop_na(Date_ID) |>
    rename_with(~paste0(gsub("[[:digit:]]", "", gsub("\\s*\\([^\\)]+\\)", "", .)), "_mgL"), -1)
  
  print(FILES)
  print(al$Date_ID[1])
  
  # warning if the Date_ID column is not acutally not a Date but a names
  if(grepl('[A-Z]', al$Date_ID[1])==T){
  
    al <- NULL
    
    warning("In ", FILES, " The Date_ID column is not in the right format.",
    "Please make sure it does not contain any letters and only has the state and the site number.",
    "File is not included in the combined data frame.")
    
  }else{
    
   al <- al|>
     filter(!grepl("[A-Z]", Date_ID)) |> #filter out 
      separate(Date_ID,c("Date","Sample_ID"),sep = "  | - |-")
   
   # Another check on the Date_ID and Sample_ID column to make sure they have the date and site ID
   if(is.na(al$Sample_ID)[1]==T){
     
     al <- NULL
     
     # Since there the Sample ID doesn't exist then we don't want to add it. 
     warning("In ", FILES, " There are no sample IDs.",
     "Check the first column in the data frame.",
     "File is not included in the combined data frame.")
   }else{
    
    # Determine the order of the Date_ID columns and make sure Date and Sample are in the correct column
    if(is.na(as.Date(al$Date[1], format = "%m/%d/%Y"))){
      
      # If you try to parse the top Date in the data frame and you get an NA,
      # that means the the DateTime and Sample_ID column were switched
      
      al <- al |>
        dplyr::rename("Sample_ID" = Date,
                      "Date" = Sample_ID)
      
    }
    
    al <- al |>
      mutate(Date =parse_date_time(Date, c("mdY", "mdy")),
             Sample_ID = as.numeric(Sample_ID))|>
      select(Date, Sample_ID, Li_mgL, Na_mgL, Mg_mgL, Al_mgL, Si_mgL, K_mgL, Ca_mgL,
             Fe_mgL, Mn_mgL,Cu_mgL, Sr_mgL, Ba_mgL)|>
      modify_if(is.character, ~as.numeric(gsub(",","",.))/1000)
    
    
    
  }
  }
  return(al)
  }
  
 # List the files in the folder
  ICP2<-list.files(path=directory, pattern="ICPMS", full.names=TRUE, recursive=TRUE)
  
  # Take out the files that are in the Files_dont_follow_key folder
  ICP2 <- ICP2[grepl("\\d+[/ICPMS]", ICP2)]
  
  # use map to read in all the files using the function above
  ICP <-ICP2 |>
    #list.files(path=directory, pattern="ICPMS", full.names=TRUE, recursive=TRUE)|>
    map_df(~ read_metals_files(.x))
    #drop_na(Date) # when NA in DateTime column. Maybe a warning?
 
#set up data frame with Reservoir, Site, Depth, and filter
  # then pivot longer so we can get the mean of any samples that had to be rerun
 frame1 <- left_join(ICP, metals_key, by = c('Sample_ID'))|>
   select(-Sample_ID)|>
   distinct(Date, Reservoir, Depth_m, Site, Filter, .keep_all = TRUE) |>
   select(Reservoir, Site, Depth_m, Filter, Date, everything()) |>
   pivot_longer(cols=c(Li_mgL:Ba_mgL), names_to="element", values_to="obs")|>
   group_by(Reservoir, Site, Depth_m, Filter, Date, element)|>
    summarize(
     count = n(), # get the number of samples
     mean = mean(obs, na.rm = TRUE))|> # take the mean. Most if not all are one so is the same value
   ungroup()

 # now pivot wider so we can make the flag columns
 frame <- frame1|>
   pivot_wider(names_from = "element", values_from = c("mean", "count"))

 # take out mean from column header
 names(frame) = gsub(pattern = "mean_", replacement = "", x = names(frame))

 # reorder the columns
 frame2 <- frame|>
   select(Reservoir, Site, Depth_m, Filter, Date, Li_mgL, Na_mgL, Mg_mgL,
          Al_mgL, Si_mgL, K_mgL, Ca_mgL, Fe_mgL, Mn_mgL, Cu_mgL, Sr_mgL, Ba_mgL, everything())

 ## add a warning if observation does not have a Reservoir and Site

 # Add in the historic files from 2014_2019 plus some one off sampling campaigns. We only have Fe and Mn for that time.

 hist <- read_csv(historic)

 # bind the historic files and the current files
 frame22 <- bind_rows(frame2, hist)%>%
   select(Reservoir, Site, Depth_m, Filter, Date, Time, Li_mgL, Na_mgL, Mg_mgL,
          Al_mgL, Si_mgL, K_mgL, Ca_mgL, Fe_mgL, Mn_mgL, Cu_mgL, Sr_mgL, Ba_mgL, everything())

 # Reorder the date
 frame2 <- frame22[order(frame22$Date),]


 # Establish flag columns and add ones for missing values
 for(j in colnames(frame2|>select(Li_mgL:Ba_mgL))) {

   #for loop to create new columns in data frame
   #creates flag column + name of variable
   frame2[,paste0("Flag_",j)] <- 0

   # puts in flag 1 if value not collected
   frame2[c(which(is.na(frame2[,j]))),paste0("Flag_",j)] <- 1

   # puts in flag 7 for sample run twice and we report the mean. Use the count columns made above
   frame2[c(which(frame2[,paste0("count_",colnames(frame2[j]))]>1)),paste0("Flag_",j)] <- 7
 }

 # Now we can remove the number of observation columns
 raw_df <- frame2|>
   select(-starts_with("count_"))


   ### 5. Use Maintenance Log to flag or change observations ####

   # Filter the Maintenance Log based on observations in the data frame
   raw_df <- raw_df|>
     arrange(Date)|>
     mutate(Date = as.Date(Date))

   # Get the date the data starts
   start <- head(raw_df, n=1)$Date

   # Get the date the data ends
   end <- tail(raw_df, n=1)$Date

   # Filter out the maintenance log
   log <- log|>
     filter(Sample_Date>=start & Sample_Date<= end)



   ### 5.1 Get the information in each row of the Maintenance Log ####
   # modify raw_df based on the information in the log


   # only run if there are observations in the maintenance log
   if(nrow(log)==0){
     print('No Maintenance Events Found...')

   } else {


     for(i in 1:nrow(log)){

       ### Get the date the samples was taken
       Sample_Date <- as.Date(log$Sample_Date[i])

       ### Get the Reservoir

       Reservoir <- log$Reservoir[i]

       ### Get the Site

       Site <- log$Site[i]

       ### Get the Depth

       Depth <- log$Depth_m[i]

       ### Get the Filter status

       Filt <- log$Filter[i]


       ### Get the Maintenance Flag

       flag <- log$flag[i]


       ### Get the names of the columns affected by maintenance

       colname_start <- log$start_parameter[i]
       colname_end <- log$end_parameter[i]

       ### if it is only one parameter parameter then only one column will be selected

       if(is.na(colname_start)){

         maintenance_cols <- colnames(raw_df|>select(colname_end))

       }else if(is.na(colname_end)){

         maintenance_cols <- colnames(raw_df|>select(colname_start))

       }else{
         maintenance_cols <- colnames(raw_df|>select(c(colname_start:colname_end)))
       }

       ### Get the name of the flag column

       flag_cols <- paste0("Flag_", maintenance_cols)


       #### find the row where all of these match
       #### The first part is the list of columns in the data frame then after %in% is the value we want
       #### to find in the data frame.
       #### All give us the rows that everything is true

     All <-  which(raw_df$Date %in% Sample_Date & raw_df$Reservoir %in% Reservoir &
                     raw_df$Site %in% Site & raw_df$Depth_m %in% Depth & raw_df$Filter %in% Filt)


       ### 5.2 Actually remove values in the maintenance log from the data frame
       ## This is where information in the maintenance log gets removed.
       # UPDatetime THE IF STATEMENTS BASED ON THE NECESSARY CRITERIA FROM THE MAINTENANCE LOG

       # replace relevant data with NAs and set flags while maintenance was in effect
       if(flag==1){
         # Sample not collected. Not used in the maintenance log

       }
       else if (flag==2){
         # Instrument Malfunction. How is this one removed?
         raw_df[All, maintenance_cols] <- NA

         # Flag the sample here
         raw_df[All, flag_cols] <- flag
       }
       else if (flag ==6){
         # Sample was digested because there were particulates, so need to multiply the concentration by 2.2

         raw_df[All, maintenance_cols] <- raw_df[All, maintenance_cols] * 2.2

         # Flag the sample here
         raw_df[All, flag_cols] <- flag
       }
       else if (flag==10){
         # improper procedure, set all data columns to NA and all flag columns to 10
         raw_df[All, maintenance_cols] <- NA

         # Flag the sample here
         raw_df[All, flag_cols] <- flag
       }
       else {
         warning("Flag used in row ", i ," in the maintenance log not defined in the L1 script. Talk to Austin and Adrienne if you get this message")
       }

       next
     }
   }


   ### 4. Read in the Minimum Reporting Limits and add flags ####

   MRL <- read_csv(MRL_file)|>
     pivot_wider(names_from = 'Symbol',
                 values_from = "MRL_mgL")

   # flag minimum reporting level
   for(j in colnames(raw_df|>select(Li_mgL:Ba_mgL))) {

   # If value negative set to minimum reporting level

     # If value negative and was digested flag with both
     raw_df[c(which(raw_df[,j]<0 & raw_df[,paste0("Flag_",j)]==6)),paste0("Flag_",j)] <- 64

     # If value negative flag
     raw_df[c(which(raw_df[,j]<0 & raw_df[,paste0("Flag_",j)]!=64)),paste0("Flag_",j)] <- 4

   # get the minimum detection level
   MRL_value <- as.numeric(MRL[1,j])

   # If value is less than MRL and has been digested then flag both  and will set to MRL later
   raw_df[c(which(raw_df[,j]<=MRL_value & raw_df[,paste0("Flag_",j)]==6)),paste0("Flag_",j)] <- 63

   # If value is less than MRL the flag and will set to MRL later
   raw_df[c(which(raw_df[,j]<=MRL_value & raw_df[,paste0("Flag_",j)]!=63)),paste0("Flag_",j)] <- 3

   # replace the negative values or below MRL with the MRL
   raw_df[c(which(raw_df[,j]<=MRL_value)),j] <- MRL_value

   # Get the sd and the mean for flagging
   sd_value <- sd(as.numeric(unlist(raw_df[j])), na.rm = TRUE) # get the minimum detection level

   mean_value <- mean(as.numeric(unlist(raw_df[j])), na.rm = TRUE)

   # Flag values over 3 standard deviations above the mean for the year.
   # This will change each time we add more observations.
   # This is why we should qaqc all raw files

   raw_df[c(which(raw_df[,j]>=mean_value + (sd_value*3))),paste0("Flag_",colnames(raw_df[j]))] <- 8

   print(j)
   print("mean")
   print(mean_value)
   print("sd")
   print(sd_value)
   print("MRL value")
   print(MRL_value)

   }

   # Pivot the data wider so that there is a T_element and and S_element

  wed <- raw_df |>
   #group_by(DateTime, Reservoir, Depth_m, Site) |>
   pivot_wider(names_from = 'Filter',
                              values_from = Li_mgL:Flag_Ba_mgL,
                               names_glue = "{Filter}_{.value}")

  # rename the Flag column
  # Change the column headers so they match what is already on EDI. Added T_ because it is easier in the

  frame3 <- wed |>
    rename_with(~gsub("T_Flag", "Flag_T", gsub("S_Flag", "Flag_S",.)), -1)|>
    mutate(
      clean_site = Site,
      Site = ifelse(Site==100.1, 100, Site)
    )

   # read in the timesheet with the date and time the samples were taken.
  # For the ISCO just use the weir time. Figure out how to do this.

   time_sheet <- gsheet::gsheet2tbl(sample_time)|>
     select(Reservoir, Site,Date,Time,Depth_m)|>
     #filter(VT_Metals =="X")|> #only take obs when metals samples were collected
     mutate(
       Date = parse_date_time(Date, orders = c('ymd HMS','ymd HM','ymd','mdy')),
       Date = as.Date(Date),
       Site = as.numeric(Site),
       Depth_m = as.numeric(Depth_m))
     #select(-VT_Metals)


   # add the time the sample was collected. Use Natural join to override NAs

   raw_df <-
     natural_join(frame3,time_sheet,
                  by = c("Reservoir", "Site","Date","Depth_m"),
                  jointype = "LEFT")|>
     select(-Site)|>
     dplyr::rename(Site=clean_site)|>
     select(Reservoir, Site, Date, Time, Depth_m, starts_with("T"), starts_with("S"), starts_with("Flag"))|>
    mutate(
      Time = as.character(hms::as_hms(Time)), # convert time and flag if time is NA
      Flag_DateTime = ifelse(is.na(Time), 1, 0),
      Time = ifelse(Flag_DateTime==1, "12:00:00",Time), # set flagged time to noon
      DateTime = ymd_hms(paste0(Date," ",Time))
    )|>
     select(-c(Date, Time))


   #### 6. Switch observations if total and soluble samples were mixed up ####

   # Determine if totals and soluble samples were switched.
   # Totals plus the Minimum reporting level is less than the soluble sample then they need to be
   # switched.
   # Cece is this what you want it to be? It looks like some of the observations are very close.
    #we want to do 3 MRL for Fe, Al, and Si, give it a flag of 9, and then see what it looks like

  for(l in c('T_Fe_mgL', 'T_Al_mgL', 'T_Si_mgL')){
    raw_df[,paste0("Check_",colnames(raw_df[l]))] <- 0  #creates Check column + name of variable
    MRL_value <- as.numeric(MRL[1,gsub("T_|S_","",l)]) # get the minimum detection level
    switch_threshold <- MRL_value*3

    # Puts "SWITCHED" in the Check column if the soluble concentration is greater than the totals plus three times the MRLA;s
    raw_df[which(raw_df[,l]+switch_threshold < raw_df[,gsub("T_", "S_", l)]),paste0("Check_",colnames(raw_df[l]))] <- "SWITCHED"
  }


  ## assign rows where all three variables were switched
  raw_df$switch_all <- 0
  for (i in 1:nrow(raw_df)){
  if (raw_df[i,'Check_T_Fe_mgL'] == 'SWITCHED' &
      raw_df[i,'Check_T_Al_mgL'] == 'SWITCHED' &
      raw_df[i,'Check_T_Si_mgL'] == 'SWITCHED'){
    raw_df[i,'switch_all'] <- 1
  }
}

  for(l in colnames(raw_df|>select(starts_with(c("T_"))))) {
    raw_df[which(raw_df[,'switch_all'] == 1), c(l,gsub("T_", "S_", l)) ] <-
      raw_df[which(raw_df[,'switch_all'] == 1), c(gsub("T_", "S_", l), l)]
  }


   # for(l in colnames(raw_df|>select(starts_with(c("T_"))))) {
   #   #for loop to create new columns in data frame
   #   raw_df[,paste0("Check_",colnames(raw_df[l]))] <- 0 #creates Check column + name of variable
   #
   #   MRL_value <- as.numeric(MRL[1,gsub("T_|S_","",j)]) # get the minimum detection level
   #
   #   # Puts "SWITCHED" in the Check column if the soluble concentration is greater than the totals plus the MRL
   #   raw_df[T_Al_mgLc(which(raw_df[,l]+MRL_value<raw_df[,gsub("T_", "S_", l)])),paste0("Check_",colnames(raw_df[l]))] <- "SWITCHED"
   #
   #   # Swap the observations from the totals and solubles if the Check column is labeled "SWITCHED"
   #
   #   raw_df[c(which(raw_df[,paste0("Check_",l)]=="SWITCHED")), c(l,gsub("T_", "S_", l)) ] <-
   #     raw_df[c(which(raw_df[,paste0("Check_",l)]=="SWITCHED")), c(gsub("T_", "S_", l), l)]
   # }

  # Flag all Na in the data frame again
  for(j in colnames(raw_df|>select(starts_with("T_"),starts_with("S_")))) {
    
    # add a flag if the samples were switched
    raw_df[which(raw_df[,'switch_all'] == 1), paste0("Flag_",j)] <- 9

    # puts in flag 1 if value not collected
    raw_df[c(which(is.na(raw_df[,j]) & is.na(raw_df[paste0("Flag_",j)]))),paste0("Flag_",j)] <- 1

  }
   # Change the column headers so they match what is already on EDI. Added T_ because it is easier in the

   frame4 <- raw_df |>
     rename_with(~gsub("T_", "T", gsub("S_", "S",.)), -1)

#let's write the final csv
#note: you must edit the script each time to save the correct file name
 frame4 <- frame4 |>
   select(Reservoir, Site, DateTime, Depth_m,
          TLi_mgL, SLi_mgL, TNa_mgL, SNa_mgL,
          TMg_mgL, SMg_mgL, TAl_mgL, SAl_mgL,
          TSi_mgL, SSi_mgL, TK_mgL, SK_mgL,
          TCa_mgL, SCa_mgL, TFe_mgL, SFe_mgL,
          TMn_mgL, SMn_mgL, TCu_mgL, SCu_mgL,
          TSr_mgL, SSr_mgL, TBa_mgL, SBa_mgL,
          Flag_DateTime,
          Flag_TLi_mgL, Flag_SLi_mgL, Flag_TNa_mgL, Flag_SNa_mgL,
          Flag_TMg_mgL, Flag_SMg_mgL,
          Flag_TAl_mgL, Flag_SAl_mgL, Flag_TSi_mgL, Flag_SSi_mgL,
          Flag_TK_mgL, Flag_SK_mgL, Flag_TCa_mgL, Flag_SCa_mgL,
          Flag_TFe_mgL, Flag_SFe_mgL, Flag_TMn_mgL, Flag_SMn_mgL,
          Flag_TCu_mgL, Flag_SCu_mgL, Flag_TSr_mgL, Flag_SSr_mgL,
          Flag_TBa_mgL, Flag_SBa_mgL) |>
   arrange(DateTime, Reservoir, Site, Depth_m)

 #### 7. Save Files ####

 # Save the metals data frame
 # Remove the ISCO samples
 final <- frame4|>
   filter(Site != 100.1)


 ### identify the date subsetting for the data
 if (!is.null(start_date)){
   #force tz check
   start_date <- force_tz(as.POSIXct(start_date), tzone = "America/New_York")

   final <- final %>%
     filter(DateTime >= start_date)

 }

 if(!is.null(end_date)){
   #force tz check
   end_date <- force_tz(as.POSIXct(end_date), tzone = "America/New_York")

   final <- final %>%
     filter(DateTime <= end_date)

 }

 if(is.null(outfile)){
   return(final)

 }else{
   final$DateTime <- as.character(format(final$DateTime)) # convert DateTime to character

    # Write the L1 file
 write_csv(final, outfile)

 }


 # Save the ISCO observations
 ISCO <- frame4|>
   filter(Site == 100.1)

 # add in filter later. Right now save everything.

 ISCO$DateTime <- as.character(format(ISCO$DateTime)) # convert DateTime to character

 write_csv(ISCO, ISCO_outfile)


}
