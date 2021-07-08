library(tidync)
require(sf)
require(tidyverse)
require(raster)
file <- here::here('data-raw','Vulcan_v3_US_annual_1km_total_mn.nc4')
library(tidync)
library(ncdfgeom)

tmax = stack(file)
co_data <- tidync(file)
aa <- co_data %>% activate("D3,D2")


# ncdf4 -------------------------------------------------------------------
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

nc_data <- nc_open(file)
# Save the print(nc) dump to a text file
{
  sink('vulcan_metadata.txt')
  print(nc_data)
  sink()
}
summary(nc_data)
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
co.array <- ncvar_get(nc_data, "carbon_emissions") # store the data in a 3-dimensional array

head(lat,1)
nc_close(file)
# raster ------------------------------------------------------------------
rast <- raster(file,varname = "carbon_emissions")

tmpin <- stack(file)
nlayers(tmpin)

print(co_data)

d.slice <- co_data %>% hyper_filter(x = x >579000 & x < 1603056,

                                                                        y = y >511000 & y <  1350425.0625)
print(d.slice$grid)


dim(d.slice[[1]])
aa <- (d.slice[[6]])


co_data_out <- d.slice %>% hyper_array()
aa <- co_data_out$carbon_emissions

aaa <- raster(aa)

DSM_HARV,
     breaks = c(300, 350, 400, 450),
     col = terrain.colors(3),

co_data_out <- d.slice %>% hyper_tibble()
src_crs <- co_data %>% activate(carbon_emissions)
crs.tib <- d.slice %>% hyper_tibble()

#get geometry
stack.nc = stack(file)
brick.nc = brick(file)
raster.nc <- raster(file)

sla.df = raster::as.data.frame(raster.nc, xy = TRUE)
xvals <- sla.df$x
yvals <- sla.df$y
plot(xvals,yvals)




oisstfile <- system.file("nc/reduced.nc", package = "stars")
oisst <- tidync(oisstfile)
print(oisst)
print(co_data)
time_ex <- oisst %>% activate("D3") %>% hyper_array()
(oisst_data <- oisst %>% hyper_array())
