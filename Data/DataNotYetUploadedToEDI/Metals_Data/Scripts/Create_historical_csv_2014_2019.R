# Make the raw file for Metals of observations from 2014-2019 because it is hard to track down all the information
# for these observations. We take the published file from 2022, filter observations from 2014 - 2019. We
# also include one off samples becuase their IDs are different then our normal IDs and are nearly impossible to hunt down.
# We then only save Reservoir, Site, DateTime, Fe, and Mg concentration files. The flag columns are added in the QAQC script. 
# I wanted it documented how this file was created. 

# This script only has to be run one time to create historic_raw_2014_2019_w_unique_samp_campaign.csv

# By: Adrienne Breef-Pilz

# Created: 04 March 2024

# packages 

pacman::p_load(tidyverse)

# Read in the file
metals <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/455/7/e71d70cac1650610e6a3fbbf7928007f")

# filter out the columns and times we want

df <- metals%>%
  select(Reservoir, Site, DateTime, Depth_m, TFe_mgL, TMn_mgL, SFe_mgL, SMn_mgL, Flag_DateTime)%>%
  mutate(Year= year(DateTime),
         Date = as.Date(DateTime))%>%
  filter(Year<2020|Date=="2020-10-16"| Date=="2020-10-17"| Date=="2020-12-02"| Date=="2021-06-10"| Date=="2021-06-11")%>%
  select(-Year, -Date)%>%
  pivot_longer(!c(Reservoir, Site, DateTime, Depth_m, Flag_DateTime), names_to = "elements", values_to = "observations")%>%
  mutate(Filter= substring(elements,1,1)) # put if the samples where filtered or not in their own column

# Get rid of the T or S in the elements column
df$elements <- gsub('T|S', '', df$elements)

# Take out the times that were flagged and separate Time into a new column 
dfg <- df |>
  separate(DateTime, sep=" ", c("Date", "Time"))|>
  mutate(
    Time = ifelse(Flag_DateTime==1, NA, Time)
  )




# now pivot wider so we have a Fe_mgL and a Mg_mgL columns
new <- dfg|>
  group_by(Reservoir, Site, Date, Time, Depth_m, Filter, elements)|>
  summarize(
    count = n(), # get the number of samples
    mean = mean(observations, na.rm = TRUE))|> # take the mean. Most if not all are one so is the same value
  ungroup()|>
  pivot_wider(names_from = elements, values_from = c(mean, count))|>
  ungroup()

# take out mean from column header
names(new) = gsub(pattern = "mean_", replacement = "", x = names(new))


# save the Date and Time column in the correct format
new$Date <- as.character(format(new$Date))

new$Time <- as.character(format(new$Time))

# write the csv

write_csv(new, "./Data/DataNotYetUploadedToEDI/Metals_Data/Raw_Data/historic_raw_2014_2019_w_unique_samp_campaign.csv")
