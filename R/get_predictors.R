## Header ---------------------------
##
## Script name:get_predictors.R
##
## Abstract: This script is used to get landscape predictors from Google Earth Engine
## for use in stormwaterheatmap regressions.
##
## Author: Christian Nilsen, Geosyntec Consultants
## Email: cnilsen@geosyntec.com
##
## Date Created: 2021-01-24
##
## Copyright (c) Geosyntec Consultants, 2021
##
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 3.0
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## For a copy of the GNU General Public License
##  see <https://www.gnu.org/licenses/>.
##


# Installation ------------------------------------------------------------

## Uncomment to install the first time (only need to do this once)
##
## Install the rgee package from GitHub
# install.packages("devtools")
# devtools::install_github("r-spatial/rgee")
#
##
## rgee depends on reticulate because it has some Python dependencies (i.e. numpy and ee), run as follows to install them:
# rgee::ee_install()
## If you are a Windows user reticulate requires miniconda/anaconda.
## The use of rgee::ee_install() is not mandatory, you can count on with your own custom installation.
##
## After install rgee, you might use the function below for checking the status of rgee.
ee_check() # Check non-R dependencies

#rgee::ee_install_upgrade()


# Libraries ---------------------------------------------------------------
library(rgee)
library(mapview)
library(purrr)
library(tidyverse)
library(data.table)



# initialize earth engine api:
ee$Initialize()

# Data  -------------------------------------------------------------------
watersheds <- ee$FeatureCollection("users/stormwaterheatmap/revised_s8_watersheds_v4")$select(c("Location_N"), c("Location")) # $filter(ee$Filter$neq('Location_N', 'POSOUTFALL_60')
## Get Predictor images from earth engine
## trees
tree_cover <- ee$Image("USGS/NLCD/NLCD2016")$select("percent_tree_cover")

## traffic
traffic <- ee$Image(0)$blend(ee$Image("users/cnilsen/traffic_raw"))$rename('traffic')

## population density
population <- ee$Image("users/stormwaterheatmap/population_per_ha")

## pm 2.5
pm25 <- (ee$Image("users/cnilsen/pm25clipped")$rename("pm25"))

## imperviousness:
tnc_landcover <- ee$Image("users/jrobertson2000/psLandCover_1m_finPS_roofs")#get land cover:
impervious <- tnc_landcover$eq(6)$Or(tnc_landcover$eq(7))$rename("impervious")# impervious is coded as 6; roofs are coded as 7

no2 <- ee$Image("users/stormwaterheatmap/SURFACE_NO2_010x010_2010")$rename("NO_2")

# age of development
age_of_development <- ee$Image(
  "JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1"
)$select("built")




# CO Emissions ------------------------------------------------------------
##Sector Code	Description
# airport	Airport sector (taxi/takeoff to 3000’)
# cement	Cement production sector
# cmv	Commercial Marine Vessel sector
# commercial	Commercial sector
# elec_prod 	Electricity production sector
# industrial	Industrial sector
# nonroad	Nonroad sector (e.g. snowmobiles, ATVs)
# onroad	Onroad sector
# railroad	Railroad sector
# residential	Residential sector
# total	Total emissions

Vulcan_total = ee$Image("users/stormwaterheatmap/Vulcan_total")$reduce('mean')$rename("CO_emissions_total")
Vulcan_cement = ee$Image("users/stormwaterheatmap/Vulcan_cement")$reduce('mean')$rename("CO_emissions_cement")
Vulcan_elec_prod = ee$Image("users/stormwaterheatmap/Vulcan_elec_prod")$reduce('mean')$rename("CO_emissions__elec_prod")
Vulcan_airport = ee$Image("users/stormwaterheatmap/Vulcan_airport")$reduce('mean')$rename("CO_emissions_airport")
Vulcan_cmv = ee$Image("users/stormwaterheatmap/Vulcan_cmv")$reduce('mean')$rename("CO_emissions_cmv")
Vulcan_commercial = ee$Image("users/stormwaterheatmap/Vulcan_commercial")$reduce('mean')$rename("CO_emissions_commercial")
Vulcan_residential = ee$Image("users/stormwaterheatmap/Vulcan_residential")$reduce('mean')$rename("CO_emissions_residential")
Vulcan_industrial = ee$Image("users/stormwaterheatmap/Vulcan_industrial")$reduce('mean')$rename("CO_emissions_industrial")
Vulcan_nonroad = ee$Image("users/stormwaterheatmap/Vulcan_nonroad")$reduce('mean')$rename("CO_emissions_nonroad")
Vulcan_onroad = ee$Image("users/stormwaterheatmap/Vulcan_onroad")$reduce('mean')$rename("CO_emissions_onroad")
Vulcan_rail= ee$Image("users/stormwaterheatmap/Vulcan_rail")$reduce('mean')$rename("CO_emissions_rail")




# State wide emissions ----------------------------------------------------



no2 <- ee$Image("users/stormwaterheatmap/SURFACE_NO2_010x010_2010")$rename("NO_2")
#state_emissions_data = ee$FeatureCollection("users/stormwaterheatmap/ecy_emissions")
PugetSound = ee$FeatureCollection("users/stormwaterheatmap/tables/PugetSound")
table2 = ee$FeatureCollection("users/stormwaterheatmap/revised_s8_watersheds_v4")
#PS_emissions = state_emissions_data$filterBounds(PugetSound)

v4_pm25 = ee$Image("users/stormwaterheatmap/V4NA03_PM25_NA_201001_201012-RH35-NoNegs")$rename("PM25_NA")

  NH4p = ee$Image("users/stormwaterheatmap/NH4p")$rename("NH4p")
sa = ee$Image("users/stormwaterheatmap/surface_area")$rename("SA")
nitp = ee$Image("users/stormwaterheatmap/NITp_2010")$rename("NITp")
so4 = ee$Image("users/stormwaterheatmap/so4p2010")$rename("so4p2010")

names = ee$List(c(
  "PM25",
  "NH4p",
  "SA",
  "NITP",
  "SO4"
))

## Other available data:
# landuse <- ee$Image("users/stormwaterheatmap/gen_landuse")

# Make Map ----------------------------------------------------------------
map_image <- so4
map_viz <- list(min = 0, max = 1, palette = list("blue", "green","yellow","white"), opacity = 0.5)
Map$centerObject(eeObject = watersheds, zoom = 7)
Map$addLayer(map_image) +
  Map$addLayer(watersheds)

# Reduce Predictors -------------------------------------------------------
## combine predictors in one dataset (one band each)
predictors <- ee$Image(0)$blend(
  ee$Image$cat(impervious,# no2, tree_cover, traffic, population, age_of_development, #pm25,
               Vulcan_total,#Vulcan_commercial,Vulcan_residential,Vulcan_onroad,Vulcan_nonroad,


               v4_pm25,
               NH4p,
                 sa,
               nitp,
                 so4

  )
)

# predictors <- ee$Image(0)$blend(ee$Image$cat(Vulcan_total,
#
#                        #Vulcan_elec_prod,
#                        #  Vulcan_airport,
#                       # Vulcan_cmv,
#                        Vulcan_commercial,
#                        Vulcan_residential,
#
#                        Vulcan_nonroad,
#                        Vulcan_onroad)#,
#                        #Vulcan_rail)
# )
## calculate mean stats from earth engine
ee_stats <- predictors$reduceRegions(
  tileScale  = 2,
  collection = watersheds,
  scale = 60,
  reducer = ee$Reducer$mean()
)

# evaluate ee object - pulls data from server to client
ee_df <- ee_stats$getInfo()

# wrangle the data
df_predictors <- ee_df$features %>%
  map("properties") %>%
  rbindlist(fill=TRUE)


# Results -----------------------------------------------------------------
View(df_predictors)

# pivot for charting
df_long <- df_predictors %>%
  pivot_longer(cols = -c(Location_N))

ggplot(df_long) +
  geom_col(aes(x = Location_N, y = value), fill = "cadetBlue", position = "dodge") +
  facet_wrap(~name, scales = "free")



# join data ---------------------------------------------------------------
source(here::here('data-raw','clean_s8_data.R'))
joined <- df_predictors %>% mutate(
  Location = Location_N
) %>% left_join(s8data, by = "Location") %>% as.data.frame()

View(joined)


# Correlation -------------------------------------------------------------
library(PerformanceAnalytics)
params = joined$parameter %>% unique() %>% sort()

i=12

p = params[i]
p
df.coc <- joined %>%
  filter(parameter == p) %>%
  mutate(log_concentration = log(concentration)) %>%
  #select(log_concentration,everything()) %>%
  mutate_at(vars(starts_with('CO_emissions')), log) %>%
  dplyr::mutate_if(is.numeric, scale)

M = df.coc %>% select_if(is.numeric) %>% dplyr::select(-access_id,-concentration)%>% dplyr::select(c(log_concentration, everything()))

chart.Correlation(M, histogram=TRUE, pch=19)

#

