### MakeEMLInflow_2019
### Updated script to publish 2019 Inflow data
### Following: MakeEMLInflow.R and MakeEMLChemistry.R
### 17Dec19 R. Corrigan and A. Hounshell
### Updated: 07 Feb 2020, A. Hounshell
### Updated: 09 Mar 2020, A. Hounshell
### Updated: 11 Jan 2021, A. Hounshell
### Updated: 6 Nov 2021, A. Hounshell
### Updated: 13 Jan 2021, W. Woelmer
### Updated: 29 Jun 2022, A. Hounshell

# (install and) Load EMLassemblyline #####
# install.packages('devtools')

# devtools::install_github("EDIorg/EDIutils")
# devtools::install_github("EDIorg/taxonomyCleanr")
# devtools::install_github("EDIorg/EMLassemblyline")

#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour
library(devtools)
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

folder <- "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_Inflow/2024_Jan/"

#Step 1: Create a directory for your dataset
#in this case, our directory is Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/Jan2021

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
#?template_core_metadata
#?template_table_attributes
#?template_categorical_variables #don't run this till later
#?template_geographic_coverage


# Import templates for our dataset licensed under CCBY, with 1 table.
#template_core_metadata(path = folder,
#                       license = "CCBY",
#                       file.type = ".txt",
#                       write.file = TRUE)

template_table_attributes(path = folder,
                          data.path = folder,
                          data.table = c("Inflow_2013_2023.csv", "Inflow_maintenancelog_2013_2023.csv",
                                         "Inflow_gaugeheight_2019_2023.csv", "Inflow_ratingcurve_2013_2023.csv"),
                          write.file = TRUE)


#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
#template_geographic_coverage(path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_Inflow/2022_Jan",
#                             data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_Inflow/2022_Jan",
#                             data.table = c("Inflow_2013_2021.csv", "2020_WeirWaterLevel.csv", "2021_WeirWaterLevel.csv"),
#                             empty = TRUE,
#                             write.file = TRUE)

#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into methods.txt
#if you want to check your methods for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information
# Add additional information for authorship contribution statement!

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#not sure if this is still true...let's find out! :-)
#see the LabKeywords.txt file for keywords that are mandatory for all Carey Lab data products

#Step 11: Personnel
#copy-paste this information in from your metadata document
#Cayelan needs to be listed several times; she has to be listed separately for her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)

#Step 12: Attributes
#grab attribute names and definitions from your metadata word document
#for units....
# View and search the standard units dictionary
view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical

#Step 14: Categorical variables
# Run this function for your dataset
#THIS WILL ONLY WORK once you have filled out the attributes_chemistry.txt and
#identified which variables are categorical
template_categorical_variables(path = folder,
                               data.path = folder,
                               write.file = TRUE)
# NO CATVARS!

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file (or geographic_coverage.txt file) that is Carey Lab specific into your working directory

#Step 16: Custom units
# Copy and pased custom units .txt file from prior year

## Step 17: Obtain a package.id FROM STAGING ENVIRONMENT. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Data Package Identifier Reservations and click 
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations" 
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below

## Step XXX: Make EML metadata file using the EMLassemblyline::make_eml() command ####
# For modules that contain only zip folders, modify and run the following 
# ** double-check that all files are closed before running this command! **

## Make EML for staging environment
## NOTE: Will need to check geographic coordinates!!!
make_eml(
  path = folder,
  data.path = folder,
  eml.path = folder,
  dataset.title = "Discharge time series for the primary inflow tributary entering Falling Creek Reservoir, Vinton, Virginia, USA 2013-2023",
  data.table = c("Inflow_2013_2023.csv", "Inflow_maintenancelog_2013_2023.csv",
                 "Inflow_gaugeheight_2019_2023.csv", "Inflow_ratingcurve_2013_2023.csv"),
  #data.table.name = c("Inflow_2013_2022.csv", "Inflow_Maintenance_RatingCurveLog_2013_2022"), 
  data.table.description = c("FCR inflow dataset from 2013 to 2023",
                             "Maintenance for the weir along with time periods for the rating curves",
                             'Observed water level from the staff gauge used for the rating curve',
                             'Rating curve information: slope, intercept, low pressure, and high pressure'),
  other.entity = c("Inflow_qaqc_2013_2023.R","Inflow_inspection_2013_2023.Rmd", 'Plot_function.R'),
  #other.entity.name = c("Inflow_QAQC_function_2013_2022","Inflow_QAQC_Plots_2013_2022","Inflow_GaugeHeight_2013_2022"),
  other.entity.description =c("Automated function for QAQC and calculating flow. Also known as L1 function",
                                      'Visual inspection script to check the data before writing to csv',
                                      "Function to create qaqc plots that can use plotly or not"),
  temporal.coverage = c("2013-05-15", "2023-12-31"),
  maintenance.description = 'ongoing',
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.923.15')




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

## Step 17: Obtain a package.id. ####
# INFLOW IS A REVISION: already have an identifier: 202.X (x = revision number)
## NOTE: Will need to check geographic coordinates!!!
make_eml(
  path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_Inflow/2022_Jan",
  data.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_Inflow/2022_Jan",
  eml.path = "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_Inflow/2022_Jan",
  dataset.title = "Discharge time series for the primary inflow tributary entering Falling Creek Reservoir, Vinton, Virginia, USA 2013-2021",
  temporal.coverage = c("2013-05-15", "2021-12-31"),
  maintenance.description = 'ongoing',
  data.table = c("Inflow_2013_2021.csv","2020_WeirWaterLevel.csv","2021_WeirWaterLevel.csv"),
  data.table.description = c("FCR inflow dataset","2020 Weir Water Level","2021 Weir Water Level"),
  other.entity= 'Inflow_Aggregation_EDI_2021.R',
  other.entity.description = "Script for compiling and QAQC of data",
  user.id = 'ccarey',
  user.domain = 'EDI',
  package.id = 'edi.202.8')

## Step 18: Upload revision to EDI
# Go to EDI website: https://portal.edirepository.org/nis/home.jsp and login with Carey Lab ID
# Click: Tools then Evaluate/Upload Data Packages
# Under EML Metadata File, select 'Choose File'
# Select the .xml file of the last revision (i.e., edi.202.4)
# Under Data Upload Options, select 'I want to manually upload the data by selecting...'
# Click 'Upload'
# Select text files and R file associated with the upload
# Then click 'Upload': if everything works, there will be no errors and the dataset will be uploaded!
# Check to make sure everything looks okay on EDI Website

