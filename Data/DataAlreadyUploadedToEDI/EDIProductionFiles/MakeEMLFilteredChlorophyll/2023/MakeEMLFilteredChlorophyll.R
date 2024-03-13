##MakeEMLFilteredChlorophyll
##Author: Mary Lofton
##Modified by Whitney Woelmer and Jacob Wynne 
##Date: 24Jun2020
##Modified by Katie Hoffman on 10 Jan 2024

#This script is to stage and publish data to EDI

#good site for step-by-step instructions
#https://ediorg.github.io/EMLassemblyline/articles/overview.html
#and links therein

library(devtools)
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFilteredChlorophyll/2023"

# (install and) Load EMLassemblyline #####
#install.packages('devtools')

#devtools::install_github("EDIorg/EMLassemblyline")
#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour
library(EMLassemblyline)


#Step 1: Create a directory for your dataset

#Step 2: Move your dataset to the directory

#Step 3: Identify an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset
#right now the only supported option is "table"; happily, this is what 
#we have!

#Step 5: Import the core metadata templates

#for our application, we will need to generate all types of metadata
#files except for taxonomic coverage, as we have both continuous and
#categorical variables and want to report our geographic location

# View documentation for these functions
?template_core_metadata
?template_table_attributes
?template_categorical_variables #don't run this till later
?template_geographic_coverage

# Import templates for our dataset licensed under CCBY, with 1 table.
#these already exist

# template_core_metadata(path = folder,
#                  license = "CCBY",
#                  file.type = ".txt",
#                  write.file = TRUE)
# 
# template_table_attributes(path = folder,
#                        data.path = folder,
#                        data.table = c("manual_chlorophyll_2014_2023.csv","site_descriptions.csv"),
#                        write.file = TRUE)


#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
# template_geographic_coverage(path = folder,
#                           data.path = folder,
#                           data.table = "manual_chlorophyll_2014_2023.csv",
#                           empty = TRUE,
#                           write.file = TRUE)

#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics
# updated for 2019 to include info on RC sites

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information
#I put the notes about FCR manipulations and pubs documenting it in this file
#KKH removed pubs and FCR manipulations. Information to that effect is in methods.txt

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#not sure if this is still true...let's find out! :-)
#see the LabKeywords.txt file for keywords that are mandatory for all Carey Lab data products

#Step 11: Personnel
#copy-paste this information in from your metadata document
#Cayelan needs to be listed several times; she has to be listed separately for her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)
# Updated this for 2019 to include HLW and WMW


#Step 12: Attributes
#grab attribute names and definitions from your metadata word document
#for units....
# View and search the standard units dictionary
view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical

#Step 13: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 14: Categorical variables
# Run this function for your dataset
#THIS WILL ONLY WORK once you have filled out the attributes_chemistry.txt and
#identified which variables are categorical
#exists already
# template_categorical_variables(path = folder,
#                                data.path = folder,
#                                write.file = TRUE)

#open the created value IN A SPREADSHEET EDITOR and add a definition for each category

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file (or geographic_coverage.txt file) that is Carey Lab specific into your working directory
#already existss

## Step 16: Obtain a package.id. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords.

# Select Tools --> Data Package Identifier Reservations and click 
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations" 
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below

#Step 17: Make EML
# View documentation for this function
?make_eml

# Run this function
make_eml(
  path = folder,
  data.path = folder,
  eml.path = folder,
  dataset.title = "Filtered chlorophyll a time series for Beaverdam Reservoir, Carvins Cove Reservoir, Claytor Lake, Falling Creek Reservoir, Gatewood Reservoir, Smith Mountain Lake, Spring Hollow Reservoir in southwestern Virginia, and Lake Sunapee in Sunapee, New Hampshire, USA during 2014-2023",
  temporal.coverage = c("2014-04-18", "2023-12-04"),
  maintenance.description = 'ongoing',
  data.table = c("FiltChla_2014_2023.csv", 'site_descriptions.csv'),
  data.table.name = c("FiltChla_2014_2023", 'site_descriptions'), 
  data.table.description = c("Filtered chlorophyll a data at multiple sites","Sampling site descriptions with latitude and longitude" ),
  other.entity = c('FiltChla_qaqc_2023_2023.R', 'FiltChla_inspection_2014_2023.Rmd', 'FiltChla_maintenancelog_2014_2023.csv'),
  other.entity.name = c("FiltChla_qaqc_2023_2023", 'FiltChla_inspection_2014_2023', 'FiltChla_maintenancelog_2014_2023'),
  other.entity.description = c('Script used to collate and flag data for publication', 'Markdown file used to visualize data for QA/QC', 
                               'Maintenance Log through 2023'), 
  user.id = 'ccarey',
  user.domain = 'EDI',
  #package.id = 'edi.52.23') #THIS IS FOR STAGING
  package.id = 'edi.555.4') # ONLY USE THIS FOR ACTUAL PUBLISHING 

# make_eml(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_ManualDischarge/2021",
#          dataset.title = "Manually-collected discharge data for multiple inflow tributaries entering Falling Creek Reservoir, Beaverdam Reservoir, and Carvin's Cove Reservoir, Vinton and Roanoke, Virginia, USA from 2019-2021",
#          data.table = "ManualDischarge_2019_2021.csv",
#          data.table.description = "Reservoir Continuum Manual Discharge Data",
#          temporal.coverage = c("2019-02-08", "2021-11-19"),
#          maintenance.description = "ongoing",
#          user.id =  "ccarey",
#          other.entity = c('Collate_QAQC_ManualDischarge_2021.R', 
#                           'SOP for Manual Reservoir Continuum Discharge Data Collection and Calculation.pdf',
#                           'calculate_discharge_flowmate_data_Jan2022.R',
#                           'CCR_VolumetricFlow_discharge_calc_example.xlsx'),
#          other.entity.description = c('Script used to collate and QAQC data for publication', 
#                                       'SOPs for discharge data collection and calculation using flowmeter, salt injection, velocity float, and bucket volumetric methods',
#                                       'Script used to calculate discharge using flowmate velocity data',
#                                       'Example spreadsheet which demonstrates the bucket volumetric calculation') ,
#          package.id = "edi.716.4", #### this is the one that I need to change!!!
#          user.domain = 'EDI')


## Step 8: Check your data product! ####
# Return to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file (e.g., edi.270.1.xml), check "I want to 
# manually upload the data by selecting files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked 
# for errors. If there are no errors, your data product is now published! 
# If there were errors, click the link to see what they were, then fix errors 
# in the xml file. 
# Note that each revision results in the xml file increasing one value 
# (e.g., edi.270.1, edi.270.2, etc). Re-upload your fixed files to complete the 
# evaluation check again, until you receive a message with no errors.

## Step 9: PUBLISH YOUR DATA! ####
# DO NOT REQUEST A NEW PACKAGE ID FOR UPDATING
# SIMPLY INCREASE THE LAST DIGIT OF THE PREVIOUS PACKAGE ID BY 1 TO UPDATE THE CURRENT PUBLICATION
# DIRECTIONS ON HOW TO GET A NEW ID ARE HERE, BUT DO NOT USE THEM FOR ALREADY PUBLISHED DATASETS BEING UPDATED (E.G. CHEMISTRY, CATWALK, CTD, ETC.)
# NEVER ASSIGN this identifier to a staging environment package.
# Go to the EDI Production environment (https://portal.edirepository.org/nis/home.jsp)
# and login using the ccarey (permanent) credentials. 

# Select Tools --> Data Package Identifier Reservations and click "Reserve Next 
# Available Identifier". A new value will appear in the "Current data package 
# identifier reservations" table (e.g., edi.518)
# This will be your PUBLISHED package.id

# In the make_eml command below, change the package.id to match your 
# PUBLISHED package id. This id should end in .1 (e.g., edi.518.1)

# ALL OTHER entries in the make_eml() command should match what you ran above,
# in step 7

make_eml(
  path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFilteredChlorophyll",
  data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFilteredChlorophyll",
  eml.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFilteredChlorophyll",
  dataset.title = "Filtered chlorophyll a time series for Beaverdam Reservoir, Carvins Cove Reservoir, Claytor Lake, Falling Creek Reservoir, Gatewood Reservoir, Smith Mountain Lake, and Spring Hollow Reservoir in southwestern Virginia, USA during 2014-2019",
  temporal.coverage = c("2014-04-18", "2019-10-04"),
  maintenance.description = 'ongoing',
  data.table = "chla_master_df_dt.csv",
  data.table.description = "Reservoir chlorophyll a dataset",
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.555.1') #DO NOT REQUEST A NEW PACKAGE ID, SIMPLY INCREASE THE LAST DIGIT HERE BY 1 TO UPDATE THE CURRENT PUBLICATION

# Once your xml file with your PUBLISHED package.id is Done, return to the 
# EDI Production environment (https://portal.edirepository.org/nis/home.jsp)

# Select Tools --> Preview Your Metadata, then upload your metadata (.xml) file 
# associated with your PUBLISHED package.id. Look through the rendered 
# metadata one more time to check for mistakes (author order, bounding box, etc.)

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file associated with your PUBLISHED package.id 
# (e.g., edi.518.1.xml), check "I want to manually upload the data by selecting 
# files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked for 
# errors. Since you checked for and fixed errors in the staging environment, this 
# should run without errors, and your data product is now published! 

# Click the package.id hyperlink to view your final product! HOORAY!