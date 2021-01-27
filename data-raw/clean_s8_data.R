

# Get and prepare monitoring data

# The stormwater outfall data is available from the Department of Ecology
# at https://data.wa.gov/Natural-Resources-Environment/Municipal-Stormwater-Permit-Outfall-Data/d958-q2ci.

# A .csv file is saved in ```WatershedRegression/data/S8_data.csv```

library(tidyverse)


all.S8.data <- read.csv("data/S8_data.csv", stringsAsFactors = FALSE)

# get antecedant days
library(readr)
Antecedant_days <- read_csv("data/Antecedant_days.csv",
    col_types = cols(ADD = col_number()),
    na = "NA"
  ) %>%
  na.omit() %>%
  dplyr::select(c(access_id, ADD))

clean_data <- function(df) {

  # filter out rejected data
  filtered.df <- (filter(df, !result_data_qualifier %in% "REJ")) %>%
    # filter out replicates
    filter(!sample_replicate_flag %in% "Y")

  # change nondetect warnings to detects
  warnings <- filtered.df$nondetect_flag == "WARNING"
  filtered.df$nondetect_flag[warnings] <- FALSE

  # Change NA to detect
  filtered.df$nondetect_flag[is.na(filtered.df$nondetect_flag)] <- FALSE

  # Change season to factor
  filtered.df$season <- as.factor(filtered.df$season)

  # Change access id to numeric
  filtered.df$access_id <- as.numeric(filtered.df$access_id)
  return(filtered.df)
}


add.antecedant.days <- function(df) {
  df <- merge(df, Antecedant_days, on.x = "access_id", on.y = "access_id")
  df$ADD <- as.numeric(df$ADD)
  return(df)
}

select_columns <- function(df) {
  # select columns used in analysis
  df <- df %>%
    dplyr::select(
      access_id,
      study_name,
      location_id,
      parameter,
      type,
      season,
      new_result_value,
      nondetect_flag,
      study_id,
      access_id,
      field_collection_end_date,
      field_collection_start_date,
      type,
      ADD
    )

  # rename some columns
  colnames(df)[colnames(df) == "location_id"] <- "Location"
  colnames(df)[colnames(df) == "new_result_value"] <-"concentration"
  df$nondetect_flag <- as.logical(df$nondetect_flag)
  df$concentration <- as.numeric(df$concentration)
  return(df)
}

select_params <- function(df) {
  # Parameters for this anlysis as a list:
  params <- c(
    "Zinc - Water - Total",
    "Copper - Water - Total",
    "Nitrite-Nitrate - Water - Dissolved",
    "Lead - Water - Total",
    "Total Phosphorus - Water - Total",
    "Total Suspended Solids - Water - Total",
    "Total Phthalate - Water - Total",
    "Total PAH - Water - Total",
    "CPAH - Water - Total"
  )
  df <- df %>% dplyr::filter(parameter %in% params)
  return(df)
}

s8data <- all.S8.data %>%
  clean_data() %>%
  #add.antecedant.days() %>%
  select_params()
  #select_columns()


