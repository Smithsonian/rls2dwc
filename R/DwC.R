# Darwin core alignment

#' Add unique ids for events and species occurrence
#'
#' @param df long dataframe with species names verified
#' @return dataframe with unqiue ids for events and species occurrences
makeIDs <- function(df){
  ids <- df %>%
    tidyr::unite(parentEventID, SiteID, DateTime, remove=FALSE) %>%
    tidyr::unite(eventID, parentEventID, Method, Block, remove=FALSE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(occurrenceID = uuid::UUIDgenerate())
  return(ids)
  }


#' Make the Darwin Core occurence table
#'
#' @param df dataframe with unique IDs for occurrences
#' @return darwin core compatible occurence table
occurence <- function(df){
  Occurrence <- df %>%
    dplyr::rename(ScientificName=scientificname, scientificNameAuthorship=valid_authority, scientificNameID=lsid, taxonRank=rank) %>%
    dplyr::mutate(occurrenceStatus="present", basisOfRecord="HumanObservation") %>%
    dplyr::select(eventID, occurrenceID, ScientificName, scientificNameAuthorship, scientificNameID, kingdom, taxonRank, occurrenceStatus, basisOfRecord)
  return(Occurrence)
  }


#' Make the Darwin Core Event table
#'
#' @param df dataframe with unique IDs for events
#' @return darwin core compatible event table
event <- function(df){
  Event <- df %>%
    dplyr::rename(eventDate=DateTime) %>%
    dplyr::mutate(coordinateUncertaintyInMeters=50, samplingProtocol="Reef Life Survey", sampleSizeValue=50,
                  sampleSizeUnit="metre", eventRemarks=paste("Visibility", vis, "meters", "direction", Direction)) %>%
    dplyr::select(parentEventID, eventID, eventDate, decimalLatitude, decimalLongitude, minimumDepthInMeters,
                  maximumDepthInMeters, coordinateUncertaintyInMeters, samplingProtocol, eventRemarks) %>%
    dplyr::distinct()
  return(Event)
}


#' Make the Darwin Core extended MeasurementOrFact table
#'
#' @param df dataframe with unique IDs for events
#' @return darwin core compatible eMoF table
emof <- function(df){
  fish_lengths <- df %>% dplyr::filter(!is.na(measurementValue)) %>%
    dplyr::select(eventID, occurrenceID, measurementType, measurementValue, measurementUnit) %>%
    dplyr::mutate(measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                  measurementTypeID="http://vocab.nerc.ac.uk/collection/P01/current/OBSINDLX/") %>%
    dplyr::mutate(measurementValue=as.numeric(measurementValue))

  fish_abundance <- df %>%
    dplyr::select(eventID, occurrenceID, IndividualCount) %>%
    dplyr::rename(measurementValue=IndividualCount) %>%
    dplyr::mutate(measurementType="count of individuals",
                  measurementTypeID="http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/")


  measures <- dplyr::bind_rows(fish_lengths, fish_abundance)

  return(measures)
}
