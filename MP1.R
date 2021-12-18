

library(usethis) #you may need to install this using install.packages('usethis')
use_git_config(user.name = "edenravecca", user.email = "edenravecca@u.boisestate.edu") #your info here
library(gitcreds) #may need to install this too
gitcreds_set() #should prompt you for your pat - paste it here # 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

rm ( list = ls() )
gc()

library(tidyverse)
library(sf)
library(terra)
library(tidycensus)
library(prism)

################################.... Load raster data ....################################
# Land cover data for 2016, NLCD, using Terra

nlcd_data_terr <- terra::rast("/opt/data/MP/Ravecca_Eden_MP/NLCD/nlcd_2016_land_cover_l48_20210604.img") 
crs(nlcd_data_terr) # what is the CRS
plot(nlcd_data_terr)
nlcd_data_terr2 <- terra::rast("/opt/data/MP/Ravecca_Eden_MP/NLCD/nlcd_2016_land_cover_l48_20210604.img") 


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

prfa.sf <- st_as_sf(west_prfa, coords = c("lon", "lat"), crs = 4326) # convert prfa gps points to sf object
prfa <- st_transform(prfa.sf, crs= crs(nlcd_data_terr))

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
head(w.cntypop)
head(reg.pop)

###############################.... Load climate data ....#############################
# https://www.fs.fed.us/rm/boise/AWAE/projects/NFS-regional-climate-change-maps/downloads/NationalForestClimateChangeMapsMetadata.pdf

precip_hist_terr <- terra::rast("/opt/data/MP/Ravecca_Eden_MP/NFCC_data/Ppt_annual_historical/Ppt_annual_historical.tif") 
plot(precip_hist_terr)
res(precip_hist_terr)
ext(precip_hist_terr)
crs(precip_hist_terr)
x <- rast(extent= ext(nlcd_data_terr), crs= crs(nlcd_data_terr), resolution= 771)
ppt_hist_reproj <- terra::project(precip_hist_terr, x) # project to NLCD crs
plot(ppt_hist_reproj)
crs(nlcd_data_terr) == crs(ppt_hist_reproj) # check crs, origin, res, ext
origin(nlcd_data_terr) == origin(x)
res(nlcd_data_terr) == res(x)
ext(nlcd_data_terr)
ext(x)

#################################.... Crop Rasters ....################################

w.states.vect <- as(w_states_reproj, "SpatVector") # turn states shapefiles into SpatVector for cropping (in Terra)
ext(w.states.vect)
ext(w_states_reproj)
plot(ppt_hist_reproj)
plot(w.states.vect, add= TRUE)
nlcd.fullres.crop <- terra::crop(nlcd_data_terr2, w.states.vect) # crop NLCD full res SpatRaster to States poly
print(nlcd.fullres.crop)
plot(nlcd.fullres.crop)
nlcd.crop <- terra::crop(nlcd_771km, w.states.vect) # crop NLCD SpatRaster to States poly
ppt.crop <- terra::crop(ppt_hist_reproj, w.states.vect) # crop Precip. SpatRaster to States poly
plot(ppt.crop)
plot(w.states.vect, add= TRUE)

plot(nlcd.crop)
res(nlcd.crop) == res(ppt.crop)
ext(nlcd.crop) == ext(ppt.crop)
crs(nlcd.crop) == crs(ppt.crop)
origin(nlcd.crop) == origin(ppt.crop)

#########################.... Aggregate & Resample Rasters ....#########################

nlcd.cats <- terra::cats(nlcd_data_terr2, layer=1) # define nlcd categories

nlcd_modeval <- terra::aggregate(nlcd.fullres.crop, fact=26, fun=modal) # aggregate to 780 m resolution and assign 
# modal value from NLCD classes
print(nlcd_modeval)
plot(nlcd_modeval, type="classes")

m <- rast(extent= ext(w.states.vect), crs= crs(nlcd.fullres.crop), resolution= 771) # blank raster for template
res(m)
crs(nlcd_data_terr) == crs(m)
ext(ppt.crop) == ext(nlcd.fullres.crop)

nlcd_771_mode <- terra::resample(nlcd_modeval, m, method= "near") # resample to 771 m resolution and assign NLCD 
# value based on "nearest neighbor"
print(nlcd_771_mode)
plot(nlcd_771_mode, type="classes")
ext(nlcd_771_mode)
res(nlcd_771_mode)
crs(nlcd_771_mode) == crs(nlcd_data_terr) # check crs, origin, res, ext

levels(nlcd_771_mode) <- nlcd.cats
is.factor(nlcd_771_mode)
print(nlcd_771_mode)
head(nlcd_771_mode)
unique(nlcd_771_mode$category) # which nlcd land cover types are present
plot(nlcd_771_mode)

##########################.... Create Population Summary ....##########################

cp.summary <- w.cntypop %>% 
  group_by(NAME) %>% 
  summarize(., avg_pop = mean(value)) %>%
  separate(NAME,c("county","state"),sep=",")

################################.... Spatial Joins ....################################

head(prfa)
head(cp.summary)
st_crs(prfa) == st_crs(cp.summary)
cp_prfa <- st_join(prfa, cp.summary, by = "state", left=TRUE)

head(cp_prfa)
str(cp_prfa)

cp_prfa.vect <- as(cp_prfa, "SpatVector") # turn county population & prfa data into SpatVector for rasterize (in Terra)
head(cp_prfa.vect)

################################.... Extract Values ....###############################

ppt_values <- terra::extract(ppt.crop, cp_prfa.vect, fun= mean, method= "bilinear", na.rm=T)
head(ppt_values)

lc_values <- terra::extract(nlcd_771_mode, cp_prfa.vect, fun= max, method= "simple", na.rm=T)
head(cp_prfa.vect)
head(lc_values)

ppt_lc_val <- left_join(ppt_values, lc_values, by= "ID")

##################################.... Final Save ....#################################

save.image("IP_MiniProj.RData")
load("IP_MiniProj.RData")






























# nlcd_data <- raster::raster("/opt/data/MP/Ravecca_Eden_MP/NLCD/nlcd_2016_land_cover_l48_20210604.img") 

# PRISM annual rainfall data for 2016
# https://prism.oregonstate.edu/normals/
# https://prism.oregonstate.edu/fetchData.php # metadata
# https://prism.oregonstate.edu/documents/PRISM_datasets_aug2013.pdf
# precip units in mm

# prism_set_dl_dir("/opt/data/MP/Ravecca_Eden_MP/PRISM_data") # where to put downloaded files
# ann_rf <- get_prism_annual(type = "ppt", years = 2016, keepZip = FALSE) # which prism data to download
# prism_archive_ls() # view downloaded files
# pd_to_file(prism_archive_ls()) # path for other packages
# pd_get_name(prism_archive_ls()) # description of downloaded files
# ppt_2016 <- prism_archive_subset("ppt", "annual", years = 2016) # get raster of ppt 2016

# ppt_allyrs <- raster::raster("/opt/data/MP/Ravecca_Eden_MP/PRISM_datadl/PRISM_ppt_30yr_normal_4kmM3_annual_bil/PRISM_ppt_30yr_normal_4kmM3_annual_bil.bil")
# head(ppt_allyrs)
# head(nlcd_data)
# head(nlcd_data_terr)
# plot(ppt_allyrs)
# ppt_norm_terr <- terra::rast(ppt_allyrs) # convert from RasterLayer to SpatRaster
# head(ppt_norm_terr)
# ppt_0nan <- ppt_norm_terr %>% replace(is.na(.), 0)
# head(ppt_0nan)
# plot(ppt_0nan)
# res(ppt_0nan)
# ext(ppt_0nan)
# crs(ppt_0nan)
# ppt_0nan_reproj <- terra::project(ppt_0nan, x) # project to NLCD crs
# plot(ppt_0nan_reproj)

###############################.... Climate data Raster....############################

# pd_image(ppt_2016) # make raster plot
# crs(ppt_2016_rast)
# 
# ppt_rastpath <- pd_to_file(ppt_2016) # path to data for loading with raster
# ppt_2016_rast <- raster(ppt_rastpath)
# plot(ppt_2016_rast)
# ppt_2016_terr <- terra::rast(ppt_2016_rast) # convert from RasterLayer to SpatRaster
# crs(ppt_2016_terr)
# ext(ppt_2016_terr)
# plot(ppt_2016_terr)
# terra::res(ppt_2016_terr)
# raster::res(ppt_2016_rast)
# terra::res(nlcd_data_terr)
# 
# ext(nlcd_data_terr)
# res(nlcd_data_terr)
# terra::crs(nlcd_data_terr)
# ext(x)
# res(x)
# crs(x)
# x <- rast(extent= ext(nlcd_data_terr), crs= crs(nlcd_data_terr), resolution= 771)
# ppt_2016_reproj <- terra::project(ppt_2016_terr, x) # project to NLCD crs
# # projected.rast <- terra::project(ppt_2016_terr, crs= crs(nlcd_data_terr))
# x2 <- rast(extent= ext(-2493045, 2342655, 177285, 3310005), crs= crs(nlcd_data_terr), resolution= 4000)
# x2 <- runif(ncell(nlcd_data_terr), 0, 1)
# ppt_2016_reproj2 <- terra::project(ppt_2016_terr, x2) # project to NLCD crs
# plot(ppt_2016_reproj2)
# 
# ext(ppt_2016_reproj)
# res(ppt_2016_reproj)
# crs(ppt_2016_reproj)
# # need to resample first?
# # crs(ppt_2016_terr)
# plot(ppt_2016_reproj)

# ppt_2016_reproj3 <- terra::project(ppt_2016_terr, x2) # project to NLCD crs
# plot(ppt_2016_reproj3)
# ext(nlcd_4km)
# res(nlcd_4km)
# crs(nlcd_4km)
# x2 <- rast(extent= ext(nlcd_4km), crs= crs(nlcd_4km), resolution= 4000)
# x2 <- runif(ncell(nlcd_data_terr), 0, 1)

# nlcd_terr_res <- terra::resample(ppt_2016_terr, nlcd_data_terr, method= "near")

# cls <- c("Unclassified", "Open Water", "Perennial Ice/Snow", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", 
#          "Developed High Intensity", "Barren Land", "Deciduous Forest", "Evergreen Forest", "Mixed Forest",
#          "Dwarf Scrub", "Shrub/Scrub", "Grassland/Herbaceous", "Sedge/Herbaceous", "Lichens", "Moss", "Pasture/Hay",
#          "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands")
# levels(nlcd_data_terr2) <- cls


# nlcd_771km <- terra::resample(nlcd_data_terr, x, method= "near")
# dim(nlcd_data_terr)
# dim(nlcd_771km)
# res(nlcd_771km) == res(ppt_hist_reproj)
# origin(nlcd_771km) == origin(ppt_hist_reproj)
# crs(nlcd_771km) == crs(ppt_hist_reproj)
# ext(nlcd_771km) == ext(ppt_hist_reproj)
# 
# is.factor(nlcd_data_terr2)
# print(nlcd_data_terr2)

# r <- rast(extent= ext(ppt.crop), crs= crs(ppt.crop), resolution= 771)
# Pop_terr <- terra::rasterize(cntypop.vect, r, field= "avg_pop", fun= "max")
# plot(Pop_terr, type= "continuous")
# plot(w_states_reproj, col= NA, add=TRUE)
# plot(cp.summary, col= NA, add=TRUE)
# head(cntypop.vect)
# head(Pop_terr)



