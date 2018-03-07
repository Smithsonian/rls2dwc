# read RLS spreadsheet data into a long format for archiving
RLSextractor <- function(rls_spreadsheet_path, sheetname){
  # example RLSextractor("../RLS/2017/RLS Hawaii DATA ENTRY RW MAY17.xlsx", "DATA")
  filesource <- basename(rls_spreadsheet_path) # extracts the filename and extension from the path to append to the record

  # load rls data from spreadsheet to R dataframe
  rls_data <- read_excel(rls_spreadsheet_path, sheet=sheetname)

  # first row contains some auxillirary header info
  header1 <- rls_data[1,] # not being used but let's retain it just in case we need it in the future
  notheader1 <- rls_data[-1,]

  # standardize the names
  colsrenamed <- notheader1 %>% rename("SiteNo"="Site No.", "SiteName"="Site Name", "decimalLatitude"="Latitude", "decimalLongitude"="Longitude",
                                       "PQs"="P-Qs", "CommonName"="Common name", "SpeciesCode"="Code")
  # urg messy way to deal with date time
  # readxlsx brings in the time as a posixct field time with a bad date (ie year 1899), so this messy step
  # extracts the time as a character, the date as a character, pastes them together and then uses lubridate
  # to turn it back into a corrected POSIXct
  fixtime <- colsrenamed %>% mutate(TimeONLY=strftime(Time, format="%H:%M:%S", tz="GMT")) %>%
    mutate(DateONLY=strftime(Date, format("%Y-%m-%d"))) %>% mutate(dtChar=paste(DateONLY, TimeONLY)) %>%
    mutate(DateTime=ymd_hms(dtChar)) %>% select(-c(TimeONLY, DateONLY, Time, Date, dtChar))

  # gather the measurements, calling "inverts" a measurement initially but then replacing Inverts with NAs
  long <- fixtime %>% filter(Total>0) %>% gather(Inverts:'400', key="measurementValue", value="IndividualCount", na.rm = FALSE) %>%
    filter(IndividualCount>0) %>% # removes the zero in the inverts columns
    select(-c(Total)) %>% # drops the total since it's no longer relevant
    mutate(measurementValue=replace(measurementValue, which(measurementValue=="Inverts"), NA)) %>%  # replaces inverts with NAs
    #mutate(measurementType=case_when(measurementValue>0 ~ "length", TRUE~NA), measurementUnit="cm")
    mutate(measurementType=ifelse(measurementValue>0,"length", NA) , measurementUnit=ifelse(measurementValue>0, "cm", NA)) %>%
    mutate(source=filesource) # adds name of the source file

  return(long)
}
