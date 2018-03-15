# gather the abundance measurements

#' Gathers the fish length/abundance measurements to a long tidy r dataframe
#'
#'
#' @param df RLS dataframe imported using the readRLS function
#' @return r dataframe
gather_measures <- function(df){
  # gather the measurements, calling "inverts" a measurement initially but then replacing Inverts with NAs
  long <- df %>% dplyr::filter(Total>0) %>% tidyr::gather('Inverts':'400', key="measurementValue", value="IndividualCount", na.rm = FALSE) %>%
    dplyr::filter(IndividualCount>0) %>% # removes the zero in the inverts columns
    dplyr::select(-c(Total)) %>% # drops the total since it's no longer relevant
    dplyr::mutate(measurementValue=replace(measurementValue, which(measurementValue=="Inverts"), NA)) %>%  # replaces inverts with NAs
    dplyr::mutate(measurementType=ifelse(measurementValue>0,"length", NA) , measurementUnit=ifelse(measurementValue>0, "cm", NA))
  return(long)
}
