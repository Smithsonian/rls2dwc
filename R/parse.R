# functions to parse data from the excel workbook

#' Load a RLS workbook to a R data frame
#'
#' Imports the data tab for a reef life survey workbook into a r dataframe
#'
#' @param xlsx path to the reef life survey excel workbook
#' @param sheetname name of the sheet (tab) with the data
#' @return r dataframe
readRLS <- function(xlsx, sheetname="DATA") {
  filesource <- basename(xlsx) # extracts the filename and extension from the path to append to the record

  # load rls data from spreadsheet to R dataframe
  rls_data <- readxl::read_excel(xlsx, sheet=sheetname)

  # first row contains some auxillirary header info
  header <- rls_data[1,] # not being used but let's retain it just in case we need it in the future
  justdata <- rls_data[-1,]

  # urg messy way to deal with date time
  # readxlsx brings in the time as a posixct field time with a bad date (ie year 1899), so this messy step
  # extracts the time as a character, the date as a character, pastes them together and then uses lubridate
  # to turn it back into a corrected POSIXct using local time
  fixtime <- justdata %>% dplyr::mutate(TimeONLY=strftime(Time, format="%H:%M:%S", tz="GMT")) %>%
    dplyr::mutate(DateONLY=strftime(Date, format("%Y-%m-%d"))) %>% dplyr::mutate(dtChar=paste(DateONLY, TimeONLY)) %>%
    dplyr::mutate(DateTime=lubridate::ymd_hms(dtChar)) %>% dplyr::select(-c(TimeONLY, DateONLY, Time, Date, dtChar))


  # RLS workbook equations fill down so there is alot of empty rows at the bottom of the workbook.
  # We will filter out any rows of data that has a Total=0
  df <- fixtime %>% dplyr::filter(Total>0)

  # adds the filesource name to the attribute table
  df <- df %>% dplyr::mutate(source=filesource) # adds name of the source file

  df <- renameRLS(df)

  return(df)
}


#' Internal function to rename columns to match Darwin Core terms
#'
#' @param df RLS dataframe imported using readRLS function
#' @return dataframe with column names renamed
renameRLS <- function(df){
  renamed <- df %>%
    dplyr::rename(SiteID = 'Site No.',
                  SiteName='Site Name',
                  decimalLatitude='Latitude',
                  decimalLongitude='Longitude',
                  identifiedBy='Diver',
                  maximumDepthInMeters='Depth',
                  scientificname='Species'
                  ) %>%
    dplyr::mutate(minimumDepthInMeters=maximumDepthInMeters)

  return(renamed)
}


#' Parse unique site names from RLS datasheet
#'
#'
#' @param df RLS dataframe
#' @return r dataframe with unqiue site names, site ids, latitudes, longitudes
uniqueSites <- function(df){
  sites <- df %>% dplyr::select(c(SiteID, SiteName, decimalLatitude, decimalLongitude)) %>%
    dplyr::distinct()  # pulls distinct locations only
  return(sites)
}
