# RLS2DWC - Reef Life Survey to Darwin Core R package

Converts a Reef life Survey spreadsheet to a R data frame and aligns the terms with [Darwin Core terminology](http://rs.tdwg.org/dwc/terms/index.htm). The functions convert the reef life survey observations to a long tidy format and validates scientific names against the [World Register of Marine Species](http://www.marinespecies.org). Exports the reef life survey data into several tables including Event, Occurrence and MeasurementOrFact


## Installation

```{r}
install.packages("devtools")
devtools::install_github("Smithsonian/rls2dwc")
```

## Example

See link to example markdown for a full example of processing a Reef Life Survey dataset.



## Workflow

### Load RLS spreadsheet 

Loads a reef life survey excel datasheet as a R dataframe. Provide the path to the excel spreadsheet and the name of the tab that contains the data (usually called "DATA"). The function will load the spreadsheet, clean up the extra header info in the second row, combine the date and time field into one column, filter out empty data rows (Total=0), and add a source column using the filename provided.  

```{r}
rlsdata <- readRLS('example_data.xlsx', 'DATA')
```


### Extract locations

Extracts the location data (SiteID, SiteName, decimalLatitude, decimalLongitude) from a reef life survey data set

```{r}
locations <- uniqueSites(rlsdata)
```

### Validate Species

Validate the scientific names using WoRMs and joins the results back to the original data

```{r}
verify_sciName(rlsdata, dryrun=TRUE) # verify all unique scientific names against worms and report ones that don't have matches

# fix the scientific names
rlsdata <- replace_sciName(rlsdata, "Acanthurus sp", "Acanthurus") %>% 
    replace_sciName(rlsdata, "Scaridae sp.", "Scaridae") %>% 
    replace_sciName(rlsdata, "Abudefduf sp.", "Abudefduf")


# verify scientific names again 
v <- verify_sciName(rlsdata, dryrun=FALSE) #  non-matched names retained
```


### Gather and make Unique IDs

Turns dataframe into a long format by gathering the abundances by size class.

```{r}
# gathers observations into a long format (one size class per row) 
RLSlong <- gather_measures(v)

# add unique IDs for the events and the occurrences
RLSlong_ids <- makeIDs(RLSlong)

```

### Create Darwin Core tables
Generate the Event, Occurence and MeasurementOrFact tables.

```{r}
event_table <- event(RLSlong_ids)
occurence_table <- occurence(RLSlong_ids)
measurementOrFact_table <- emof(RLSlong_ids)
```

