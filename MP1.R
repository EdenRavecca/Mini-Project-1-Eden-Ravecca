
library(tidyverse)
library(sf)
library(terra)
library(tidycensus)
library(raster)
library(prism)
# library(sp)

#################################.... Load PRFA data ....#################################
# Occurrences of PRFA in 2016
# filter occurrences in states of interest, keep relevant columns, rename columns

falcon_csv <- read.csv("/opt/data/MP/Ravecca_Eden_MP/FalconData/PRFA_Data.csv")
states <- c( "Idaho", "Montana", "Wyoming", "Utah", "Nevada", "Arizona", "New Mexico", 
             "Colorado", "California", "Oregon", "Washington" ) # to filter states of interest at once
west_prfa <- falcon_csv %>% 
  dplyr::filter(., stateProvince %in% states) %>% # keep western states
  dplyr::select(., c(occurrenceID, locality, stateProvince, decimalLatitude, decimalLongitude, eventDate,
                     day, month, year)) %>% # keep relevant columns
  dplyr::filter(., year == "2016") %>% # keep only 2016 observations
  dplyr::rename(., c(state = stateProvince, lat = decimalLatitude, lon = decimalLongitude, date = eventDate, ID
                     = occurrenceID)) # change names to make sense

unique(west_prfa$year) # check

################################.... Load raster data ....################################
# Land cover data for 2016

nlcd_data <- raster::raster("/opt/data/MP/Ravecca_Eden_MP/NLCD/nlcd_2016_land_cover_l48.img") 
nlcd_data_terr <- terra::rast("/opt/data/MP/Ravecca_Eden_MP/NLCD/nlcd_2016_land_cover_l48.img") 

crs(nlcd_data_terr) # what is the CRS

################################.... Load states data ....################################
# Poly for all Western States of interest (Rocky Mountain states and west to pacific)

states_data <- st_read("/opt/data/MP/Ravecca_Eden_MP/StatesPoly/tl_2012_us_state/tl_2012_us_state.shp")
colnames(states_data)
w_states_shp <- states_data %>%
  dplyr::select(., c(STUSPS, NAME, geometry)) %>% # keep relevant columns
  dplyr::filter(., NAME %in% states) %>% # keep western states of interest
  dplyr::rename(., c(States = NAME, State_Abbreviation = STUSPS))
plot(w_states_shp) # check
all(st_is_valid(w_states_shp)) # make sure geometries are valid; TRUE
# st_make_valid(w_states_shp)
w_states_reproj <-  st_transform(w_states_shp, crs = crs(nlcd_data_terr)) # reproject states polys to match NLCD data
st_crs(w_states_reproj) # check

plot(nlcd_data_terr)
plot(w_states_reproj, add = TRUE)

##############################.... Load population data ....#############################
# Population by County for Western States

census_api_key("ccd5de8eb03adae16092bc5e1c6c40cb104f1e97", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")
options(tigris_use_cache = TRUE)
reg.pop <- tidycensus::get_estimates(geography = "county",
                                     product = "population",
                                     state = c( "ID", "MT", "WY", "UT", "NV", "AZ", "NM", "CO", "OR", "WA", "CA" ), 
                                     year = 2016,
                                     key = key,
                                     geometry = TRUE,
                                     time_series = T) %>%
                                     st_transform(., st_crs(w_states_reproj))

w.cntypop <- filter(reg.pop, variable == "POP") # keep population value only (remove density)
unique(w.cntypop$variable)
head(w.cntypop)
all(st_is_valid(w.cntypop))
st_crs(w.cntypop) == st_crs(w_states_reproj)
plot(w.cntypop)

#################################.... Crop Rasters ....################################

w.states.vect <- as(w_states_reproj, "SpatVector") # turn states shapefiles into SpatVector for cropping (in Terra)
# terra::crop kept making R crash.
# str(w.states.vect)
# raster::extent(nlcd_data)
# raster::extent(w_states_reproj)
terra::ext(w.states.vect)
terra::ext(w_states_reproj)
# terra::crs(w.states.vect)
# 
# r <- rast()
# e <- ext(r)
# as.vector(e)
# as.character(e)
# ext(r) <- c(-2361583.22552255, -504621.51727754, 991813.443897916, 3177418.32418301)
# r
# er <- ext(r)
# terra::ext(er)
# e <- terra::ext(-2361583.22552255, 504621.51727754, 991813.443897916, 3177418.32418301)
# sp_states <- as_Spatial(w_states_reproj)
# raster::extent(sp_states)
# str(sp_states)

nlcd.crop <- terra::crop(nlcd_data_terr, w.states.vect)
# terra::crop kept making R crash. Working with Kyle currently to try and find a solution.

###############################.... Load climate data ....#############################
# PRISM annual rainfall data for 2016
# https://prism.oregonstate.edu/normals/
# https://prism.oregonstate.edu/fetchData.php # metadata
# https://prism.oregonstate.edu/documents/PRISM_datasets_aug2013.pdf
# precip units in mm

prism_set_dl_dir("/opt/data/MP/Ravecca_Eden_MP/PRISM_data") # where to put downloaded files
ann_rf <- get_prism_annual(type = "ppt", years = 2016, keepZip = FALSE) # which prism data to download
prism_archive_ls() # view downloaded files
pd_to_file(prism_archive_ls()) # path for other packages
pd_get_name(prism_archive_ls()) # description of downloaded files
ppt_2016 <- prism_archive_subset("ppt", "annual", years = 2016) # get raster of ppt 2016

###############################.... Climate data Raster....############################

pd_image(ppt_2016) # make raster plot

ppt_rastpath <- pd_to_file(ppt_2016) # path to data for loading with raster
ppt_2016_rast <- raster(ppt_rastpath)
plot(ppt_2016_rast)
crs(ppt_2016_rast)
# ppt_2016_reproj <- projectRaster(ppt_2016_rast, nlcd_data_terr)

save.image("IP_MiniProj.RData")
load("IP_MiniProj.RData")
