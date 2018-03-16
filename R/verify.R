# verify scientific names using the World Register of Marine Species (WoRMS)

#' Verify scientific names using the World Register of Marine Species (WoRMS)
#'
#'
#' @param df dataframe with scientific names to validate (col should be named scientificname)
#' @param allowUnmatched boolean to allow unmatched records (default is false)
#' @return dataframe with matches joined to the rows
verify_sciName <- function(df, allowUnmatched=FALSE){

  species_list <- df %>% dplyr::select(scientificname) %>%
    dplyr::distinct() %>% dplyr::pull(scientificname)

  worms_rec <-worrms::wm_records_names(name =species_list)

  worms_rec_df <- worms_rec %>% dplyr::bind_rows() %>%
    dplyr::filter(status=="accepted")

  joined <- dplyr::left_join(ex, worms_rec_df, by="scientificname")

  # print to console the unmatched records
  unmatched_records <- joined %>% dplyr::filter(is.na(AphiaID)) %>% dplyr::select(scientificname) %>% dplyr::distinct()
  if(nrow(unmatched_records)>0 & !allowUnmatched){
    print("The following scientific names do not have a match in WoRMS...")
    print(unmatched_records)
    stop(paste("Unknown species fix manually and try again:", unmatched_records))
    }
  return(joined)
}


#' Replaces all occurnaces of a scientific name with a new value
#'
#' Intended use is to make your data complient with WoRMS
#'
#' @param df dataframe with species names to validate (col should be named scientificname)
#' @param old scientific name to replace
#' @param new the new scientific name to use
#' @return dataframe
replace_sciName <- function(df, old, new){
  t <- df %>% dplyr::mutate(scientificname = dplyr::if_else(scientificname==old, new, scientificname))
  return(t)
}
