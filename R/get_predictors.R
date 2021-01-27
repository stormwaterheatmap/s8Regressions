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




# Libraries ---------------------------------------------------------------
library(rgee)
library(mapview)
library(purrr)
library(tidyverse)
library(data.table)



# initialize earth engine api:
ee$Initialize()


# Data  -------------------------------------------------------------------
watersheds <- ee$FeatureCollection("users/cnilsen/s8_watersheds")$select(c("Location_N"), c("Location")) # $filter(ee$Filter$neq('Location_N', 'POSOUTFALL_60')

## Get Predictor images from earth engine
## trees
tree_cover <- ee$Image("USGS/NLCD/NLCD2016")$select("percent_tree_cover")

## traffic
traffic <- ee$Image(0)$blend(ee$Image("users/cnilsen/traffic_raw"))

## population density
population <- ee$Image("users/stormwaterheatmap/population_per_ha")

## pm 2.5
pm25 <- (ee$Image("users/cnilsen/pm25clipped")$rename("pm25"))

## imperviousness:
tnc_landcover <- ee$Image("users/jrobertson2000/psLandCover_1m_finPS_roofs")#get land cover:
impervious <- tnc_landcover$eq(6)$Or(tnc_landcover$eq(7))$rename("impervious")# impervious is coded as 6; roofs are coded as 7

# age of development
age_of_development <- ee$Image(
  "JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1"
)$select("built")

## Other available data:
# landuse <- ee$Image("users/stormwaterheatmap/gen_landuse")

# Make Map ----------------------------------------------------------------
map_image <- treeCover
map_viz <- list(min = 0, max = 100, palette = list("white", "green"), opacity = 0.5)
Map$centerObject(eeObject = watersheds, zoom = 7)
Map$addLayer(map_image, visParams = map_viz) +
  Map$addLayer(watersheds)

# Reduce Predictors -------------------------------------------------------
## combine predictors in one dataset (one band each)
predictors <- ee$Image(0)$blend(
  ee$Image$cat(impervious, tree_cover, traffic, population, age_of_development, pm25)
)

## calculate mean stats from earth engine
ee_stats <- predictors$reduceRegions(
  collection = watersheds,
  scale = 30,
  reducer = ee$Reducer$mean()
)

# evaluate ee object - pulls data from server to client
ee_df <- ee_stats$getInfo()

# wrangle the data
df_predictors <- ee_df$features %>%
  map("properties") %>%
  rbindlist()


# Results -----------------------------------------------------------------
View(df_predictors)

# pivot for charting
df_long <- df_predictors %>%
  pivot_longer(cols = -c(Location_N))

ggplot(df_long) +
  geom_col(aes(x = Location_N, y = value), fill = "cadetBlue", position = "dodge") +
  facet_wrap(~name, scales = "free")
