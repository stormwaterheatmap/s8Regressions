library(tidyverse)
library(magrittr)
library(lubridate)
library(daymetr)

# load s8 data downloaded from eim.
# Filter to get individual sampling events
data <- readRDS("data/S8_data_eim_raw.rds") %>%
  select(1:11, 18, 22:23, 39:40) %>%
  unique()

# all lowercase col names
colnames(data) %<>% tolower

# format dates
data$field_collection_start_date <- as.Date.character(data$field_collection_start_date,format="%m/%d/%Y")
data$field_collection_end_date <- as.Date(data$field_collection_end_date,format="%m/%d/%Y")

#add year and Julian day
data <- data   %>%
  add_column(
  yday = yday(.$field_collection_start_date),
  year = year(.$field_collection_start_date)
)


# remove non-outfall locations and drop the parameter column to get list of
# unique events
events <- data %>%
  filter(
    !result_parameter_name %in% c(
      "Solids",
      "Precipitation",
      "Storm Event Flow Volume",
      "Sample Event Flow Volume",
      "Flow"
    )
  ) %>%
  select(-result_parameter_name, result_value, result_value_units) %>%
  unique()


# save site list and xy data to file
siteList <- unique(
  tibble(
    site = events$location_id,
    lat = events$calculated_latitude_decimal_degrees_nad83harn,
    lon = events$calculated_longitude_decimal_degrees_nad83harn
  )
)
write.csv(siteList, "siteList.csv", row.names = FALSE)

# Download the nearest rainfall gage from daymet
daymet_p <- download_daymet_batch(file_location = "siteList.csv", start = 2009,
                                  end = 2013, simplify = T) #

# rename columns
colnames(daymet_p)[colnames(daymet_p) == "site"] <- "location_id"

# get just the precip data
p <- daymet_p %>%
  filter(measurement == "prcp..mm.day.") %>%
  select(-c(tile,altitude))

# calculate two day, three day, and seven day cummulative precip
p$two.day <- zoo::rollsum(p$value, 2, fill = 0)
p$three.day <- zoo::rollsum(p$value, 3, fill = 0)
p$seven.day <- zoo::rollsum(p$value, 7, fill = 0)

# Match em
s8data.w.Precip <- plyr::join_all(list(data, p), by = c("location_id", "year", "yday"),
                                  type = "left",match="all")

# Classify according to antecedant moisture condition
# Use AMC from TR-55 example here: https://www.researchgate.net/figure/Antecedent-Moisture-Conditions_tbl1_236851811
s8data.w.Precip$AMC <- cut(s8data.w.Precip$three.day,
                           breaks = c(-Inf, 12.7, 27.9, Inf),
                           labels = c("AMC_1", "AMC_2", "AMC_3"))


#quick function to display the results
plot_fun <- function(parameter, precip_col) {
  df <- s8data.w.Precip %>%
    filter(result_parameter_name == parameter) %>%
    filter(result_value_units == "ug/L")

  ggplot(df, aes_string(x = precip_col, y = "result_value")) + geom_point() +
    facet_wrap( ~ location_id) + geom_smooth(method = "glm") +
    scale_y_log10()+xlab(paste(precip_col,"rainfall, (mm)"))+ggtitle(paste(
      parameter,'ug/L'))

}

#plot for Copper concentration vs. 7 day rainfall depth
plot_fun("Copper","seven.day")

