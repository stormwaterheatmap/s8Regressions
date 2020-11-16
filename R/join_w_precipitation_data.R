library(tidyverse)
library(magrittr)
library(lubridate)
library(daymetr)

# load s8 data downloaded from eim.
# Filter to get individual sampling events
data <- readRDS("data/S8_data_eim_raw.rds") %>%
  select(1:11, 18, 39:40) %>%
  unique()

# all lowercase col names
colnames(data) %<>% tolower

# format dates
data$field_collection_start_date <- as.Date(data$field_collection_start_date)
data$field_collection_end_date <- as.Date(data$field_collection_end_date)

# remove non-outfall locations and drop the parameter column to get list of
# unique events
data <- data %>%
  filter(
    !result_parameter_name %in% c(
      "Solids",
      "Precipitation",
      "Storm Event Flow Volume",
      "Sample Event Flow Volume",
      "Flow"
    )
  ) %>%
  select(-result_parameter_name) %>%
  unique() %>%
  add_column(
    yday = yday(.$field_collection_start_date),
    year = year(.$field_collection_start_date)
  )

# save site list and xy data to file
siteList <- unique(
  tibble(
    site = data$location_id,
    lat = data$calculated_latitude_decimal_degrees_nad83harn,
    lon = data$calculated_longitude_decimal_degrees_nad83harn
  )
)
write.csv(siteList, "siteList.csv", row.names = FALSE)

# Download the nearest rainfall gage from daymet
daymet_p <- download_daymet_batch(file_location = "siteList.csv", start = 2009,
                                  end = 2013, simplify = T) #

# rename columns
colnames(daymet_p)[colnames(daymet_p) == "site"] <- "location_id"

# get just the precip data
p <- filter(daymet_p, measurement == "prcp..mm.day.")

# calculate two day, three day, and seven day cummulative precip
p$two.day <- zoo::rollsum(p$value, 2, fill = 0)
p$three.day <- zoo::rollsum(p$value, 3, fill = 0)
p$seven.day <- zoo::rollsum(p$value, 7, fill = 0)

# Match em
s8data.w.Precip <- plyr::join(data, p, by = c("location_id", "year", "yday"))

# Classify according to antecedant moisture condition
s8data.w.Precip$AMC <- cut(s8data.w.Precip$three.day,
                           breaks = c(-Inf, 12.7, 27.9, Inf),
                           labels = c("AMC_1", "AMC_2", "AMC_3"))
