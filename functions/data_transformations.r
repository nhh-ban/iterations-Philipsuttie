
# Load necessary libraries
library(tibble)
library(purrr)

# Define the function to transform metadata to a data frame
transform_metadata_to_df <- function(stations_metadata) {
  # Flatten the list structure and extract the necessary information
  metadata_df <- stations_metadata$trafficRegistrationPoints %>%
    map_df(~ {
      latestData <- .x$latestData
      list(
        id = .x$id,
        name = .x$name,
        latestData = as.POSIXct(latestData$volumeByHour, tz = "UTC"),
        lat = .x$location$coordinates$latLon$lat,
        lon = .x$location$coordinates$latLon$lon
      )
    })
  
  return(metadata_df)
}



