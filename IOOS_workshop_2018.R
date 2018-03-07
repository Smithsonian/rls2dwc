###################################################################################
#                                                                                ##
# Aligning Carrie Bow RLS 2015 to Darwin Core                                    ##
# Data are current as of 2018-02-08                                              ##
# Data source: Tennenbaum Marine Observatories Network - Smithsonian             ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2018-03-06                                                        ##
#                                                                                ##
###################################################################################

# SUMMARY:


# Required Files (check that script is loading latest version):
# 2015_CB_RLS.csv

# Associated Scripts:
# none

# TO DO

# need to clear up non-unique occurrence values that result. (about 80)


###################################################################################
# TABLE OF CONTENTS                                                               #
#                                                                                 #
# RECENT CHANGES TO SCRIPT                                                        #
# LOAD PACKAGES                                                                   #
# READ IN AND PREPARE DATA                                                        #
# MANIPULATE DATA                                                                 #   
#                                                                                 #
###################################################################################

###################################################################################
# RECENT CHANGES TO SCRIPT                                                        #
###################################################################################

# 2018-03-06 corrected problem with duplicate occurrences
# 2018-02-14 added check for unique occurrences
# 2018-02-08 Script created.  

###################################################################################
# LOAD PACKAGES                                                                   #
###################################################################################

library(tidyverse)
library(worrms)

###################################################################################
# READ IN AND PREPARE DATA                                                        #
###################################################################################

RLS_data <- read_csv("2015_CB_RLS.csv")

# replace NAs with 0 
RLS_data[is.na(RLS_data)] <- 0

# gather data by size class
RLS_data_gather <- RLS_data %>%
  gather(Length, Abundance, 21:48)
#remove non-occurrences
RLS_data_occur <- RLS_data_gather %>%
  filter(!Abundance == 0)

###################################################################################
# MANIPULATE DATA                                                                 #
###################################################################################

# Change columns to match DarwinCore

# Create eventDate column
RLS_data_occur$Date <- as.Date(RLS_data_occur$Date,
                       format = "%m/%d/%y")
RLS_data_occur <- RLS_data_occur %>%
  unite(eventDate, Date, Time, sep = "T")

# Rename directly translatable columns
# Rename Diver to identifiedBy
names(RLS_data_occur)[names(RLS_data_occur) == 'Diver'] <- 'identifiedBy'
# Rename Latitude to decimalLatitude
names(RLS_data_occur)[names(RLS_data_occur) == 'Latitude'] <- 'decimalLatitude'
# Rename Longitude to decimalLongitude
names(RLS_data_occur)[names(RLS_data_occur) == 'Longitude'] <- 'decimalLongitude'
names(RLS_data_occur)[names(RLS_data_occur) == 'Species'] <- 'scientificName'
names(RLS_data_occur)[names(RLS_data_occur) == 'Depth'] <- 'maximumDepthInMeters'
names(RLS_data_occur)[names(RLS_data_occur) == 'Length'] <- 'measurementValue'

# duplicate maximumDepthInMeters to minimumDepthInMeters
RLS_data_occur$minimumDepthInMeters <- RLS_data_occur$maximumDepthInMeters

# add new columns
RLS_data_occur$basisOfRecord <- rep("HumanObservation")
RLS_data_occur$occurrenceStatus <- rep("present")
RLS_data_occur$coordinateUncertaintyInMeters <- rep("50")
RLS_data_occur$geodeticDatum <- rep("EPSG:4326 WGS84")


# extract species vector to check against WoRMS and get LSID
species <- unique(RLS_data_occur$scientificName)
species <- as.character(species)
WoRMS_rec <- wm_records_names(name = species)
WoRMS_rec_df <- do.call("rbind", WoRMS_rec)
WoRMS_rec_accept <- WoRMS_rec_df %>%
  filter(status == "accepted")

# join lsid to main file
names(WoRMS_rec_accept)[names(WoRMS_rec_accept)=="scientificname"] <- "scientificName"
RLS_data_occur <- left_join(RLS_data_occur, WoRMS_rec_accept, by="scientificName")


# identify species not retrieved
unique(RLS_data_occur$scientificName[!(RLS_data_occur$scientificName %in% WoRMS_rec_accept$scientificName)])


# rename new WoRMS-derived columns to match DwC
names(RLS_data_occur)[names(RLS_data_occur) == 'lsid'] <- 'scientificNameID'
names(RLS_data_occur)[names(RLS_data_occur) == 'valid_authority'] <- 'scientificNameAuthorship'

# create eventID and parentEventID columns
RLS_data_occur$event <- rep("MarineGEO_RLS")
RLS_data_occur <- RLS_data_occur %>%
  unite(eventID, event, Site.No., eventDate, sep = "_", remove = FALSE)
# THIS IS ENTERED MANUALLY
RLS_data_occur$parentEventID <- rep("MarineGEO_CB_2015-09")

# create occurrenceID column
RLS_data_occur <- RLS_data_occur %>%
  unite(occurrenceID, eventID, Code, Block, measurementValue, identifiedBy, sep = "_", remove = FALSE)

# aggragate any split rows
RLS_data_occur$Abundance <- as.numeric(RLS_data_occur$Abundance)
RLS_data_agg <- RLS_data_occur %>%
  group_by(occurrenceID) %>%
  summarise(sum(Abundance))
RLS_data_join <- left_join(RLS_data_agg, RLS_data_occur, by="occurrenceID")
RLS_data_occur <- RLS_data_join %>%
  select(-Abundance) %>%
  distinct(occurrenceID, .keep_all = TRUE)
names(RLS_data_occur)[names(RLS_data_occur) == 'sum(Abundance)'] <- 'Abundance'


# EXTENDED MEASUREMENT OR FACT FIELDS

# create new columns
RLS_data_occur$measurementUnit <- rep("Centimeter")
RLS_data_occur$measurementUnitID <- rep ("http://vocab.nerc.ac.uk/collection/P06/current/ULCM/")
RLS_data_occur$measurementType <- rep("body length")
RLS_data_occur$measurementTypeID <- rep("http://vocab.nerc.ac.uk/collection/P01/current/OBSINDLX/")

# separate out each measurement to rejoin in long form
length <- RLS_data_occur %>%
  select(eventID, occurrenceID, measurementType, measurementTypeID, measurementUnit, measurementUnitID, measurementValue) 

count <- RLS_data_occur %>%
  select(eventID, occurrenceID, Abundance)
names(count)[names(count) == 'Abundance'] <- 'measurementValue'
count$measurementType <- rep("count of individuals")
count$measurementTypeID <- rep("http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/")
count$measurementValue <- as.character(count$measurementValue)

visibility <- RLS_data_occur %>%
  select(eventID, vis)
names(visibility)[names(visibility) == 'vis'] <- 'measurementValue'
visibility$measurementValue <- as.character(visibility$measurementValue)
visibility$measurementType <- rep("underwater visibility")
visibility$measurementTypeID <- rep("http://vocab.nerc.ac.uk/collection/P14/current/GVAR0923/")
visibility$measurementUnit <- rep("Meter")
visibility$measurementUnitID <- rep("http://vocab.nerc.ac.uk/collection/P06/current/ULAA/")


###################################################################################
# OUTPUT DwC FILES                                                                #
###################################################################################

Event <- RLS_data_occur %>%
  select(parentEventID, eventID, eventDate, decimalLatitude, decimalLongitude, minimumDepthInMeters, maximumDepthInMeters, coordinateUncertaintyInMeters) %>%
  distinct()

write_csv(Event, "Event.csv")

Occurrence <- RLS_data_occur %>%
  select(eventID, occurrenceID, scientificName, scientificNameAuthorship, scientificNameID, kingdom, occurrenceStatus, basisOfRecord)

# check for unique occurrences for all rows (result should == 0)
length(Occurrence$occurrenceID) - length(unique(Occurrence$occurrenceID))

# if not 0, identify duplicates:
dupes <- Occurrence$occurrenceID
dupes[dupes %in% unique(dupes[duplicated(dupes)])]

write_csv(Occurrence, "Occurrence.csv")

EMoF <- length %>%
  full_join(count) %>%
  full_join(visibility) %>%
  distinct()

write_csv(EMoF, "EMoF.csv")

#####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#
