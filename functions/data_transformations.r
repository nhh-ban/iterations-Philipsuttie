
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


# Load necessary libraries
library(anytime)
library(lubridate)

# Function to convert a date-time to ISO8601 format with an offset
to_iso8601 <- function(datetime, offset) {
  iso_date <- as.character(iso8601(datetime))
  iso_date <- as_datetime(iso_date)
  adjusted_date <- iso_date + days(offset)
  iso_string <- format(adjusted_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  return(iso_string)
}


# Save the vol_qry function to a file
writeLines('
library(glue)

vol_qry <- function(id, from, to) {
  query <- glue(
    \'{{
      trafficData(trafficRegistrationPointId: "{id}") {{
        volume {{
          byHour(from: "{from}", to: "{to}") {{
            edges {{
              node {{
                from
                to
                total {{
                  volumeNumbers {{
                    volume
                  }}
                }}
              }}
            }}
          }}
        }}
      }}
    }}\',
    id = id,
    from = from,
    to = to
  )
  return(query)
}
', con = 'iterations-Philipsuttie/gql-queries/vol_qry.r')


b <- GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)


# Define the function to transform Statens Vegvesen API data to a data frame
transform_volumes <- function(api_data) {
  # Extract the necessary information and flatten the list structure
  volumes_df <- api_data$trafficData$volume$byHour$edges %>%
    map_df(~ {
      node <- .x$node
      list(
        from = as.POSIXct(node$from, tz = "UTC"),
        to = as.POSIXct(node$to, tz = "UTC"),
        volume = node$total$volumeNumbers$volume
      )
    })
  
  return(volumes_df)
}





