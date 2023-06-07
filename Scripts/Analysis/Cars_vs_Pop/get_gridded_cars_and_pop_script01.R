######################################################################
#                                                                    #
#      Evaluate relationship between No. people and No. of cars      #
#                                                                    #
#            - Part 1: Get gridded cars and population               #
#                   at various grid resolutions -                    #
#                                                                    #
######################################################################


# The following script computes the average no. of cars and no. of people
# at various grid resolutions. Average no. of cars/people can be computed at
# different temporal resolutions, but here we focus on a monthly resolution.

# The overall goal is to evaluate the relationship between gridded cars and population,
# and to use this relationship to predict IDPs during the conflict year (i.e., 2022).

# This script represents the first part of the car~pop evaluation.
# Here, for each city a spatial grid is built, whereby WorldPop and cars
# will be counted for each individual grid cell and given city-time stamp.
# The output of the script is a data frame containing the gridded pop and cars.
# This output is then used in the predicting_IDPs_script02.R script, the second
# part of the car~pop evaluation.


## Please update input & output folder paths accordingly.


## ATTENTION: original worldpop file was too large to be uploaded on the github repository
## The folder contains an aggregated file version in order for the user to run the below script.
## However, the user is needs to download the original file via worldpop data, or request it to me (Marie)
## such that the outputs are as in the original paper. 
## For more details, refer to the README file under GIS/Population/WorldPop/Shapefile/Numbers/Uncontrained)


## Code written by: Marie-Christine Rufener < macrufener@gmail.com >
## Last update: June 2023

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~~
library(sf); sf_use_s2(FALSE)
library(raster)
library(exactextractr)
library(dplyr)
library(mapview)
library(svMisc)
library(stringr)


#~~~~~~~~~~~~~~~
# Default inputs
#~~~~~~~~~~~~~~~

## Grid resolution
GRID_SPACING <- c(500, 1000, 2000, 3000, 5000, 10000, 15000)[2] # 500m, 1, 2, 3, 5, 10, 15 km; default is 1 km


## Worldpop data
DINPUT <- c("Count", "Density")[1] #Define whether to use population density or population count; default is 'count'


## Car geolocations
CARS <- c("original", "filtered") [2] #original = original car detection file, filtered = car detection file filtered by OSM layers (see script: OSM_Filtering.R); default is filtered


## Confidence threshold (based on small vehicles only - object class 18)
THRESHOLD <- c("TH_15", "TH_45") [2] # 0.15 and 0.45 (less and more conservative thresholds); default is 0.45



#~~~~~~~~~
# Set WD
#~~~~~~~~~
## Set main working directory
#setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine")
setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GitHub/IDP_UKR/") 



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~
# 1) Load data
#~~~~~~~~~~~~~~~



# 1.1) Load aggregated data 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To  retrieve only the images that were kept after post-processing pipeline
## List all files (from all the thresholds)
files <- list.files(path = 'Data/Cars/Car_aggregated/POP_CLOUD_filtered/',
                    pattern = '.csv',
                    full.names = TRUE,
                    recursive = T)


## Read the files
dat <- list()
for(i in seq_along(files)) {
  dat[[i]] <- read.csv(files[i])
  dat[[i]]$Threshold <- basename(dirname(files))[i]
}


## Bind the two data files into single data frame
dat <- do.call('rbind', dat)


## Keep only 0.45 threshold images & image id
IMG <- dat %>% 
  filter(Threshold == "th_045_carclass_18") %>%
  .[,c('Image', 'City', 'Date')]






# 1.2) AOI polygons of the cities
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## List all files in the folders
fp <- list.files(path = "GIS/SatelliteImage/Img_AOI",
                 recursive = TRUE,
                 pattern = "\\.shp$",
                 full.names = TRUE)


## Load the files & set them into a list
aoipoly <- list()
for(i in seq_along(fp)) {
  aoipoly[[i]] <- st_read(fp[i])
  names(aoipoly)[[i]] <- basename(dirname(fp))[i]
  aoipoly[[i]]$City <- as.factor(basename(dirname(fp))[i])
  aoipoly[[i]] <- st_transform(aoipoly[[i]],crs = 6381) # Project to CRS with units set in meters (to make the grid in the units of meters)
}

#lapply(imgpoly, function(x) st_crs(x)$epsg) #Checks the proj-string representation


## Combine all polygons into a single shapefile 
aoipolyall <- do.call(what = sf:::rbind.sf, args=aoipoly)


## Remove any non-matching cities
setdiff(aoipolyall$City, unique(IMG$City))

levels(aoipolyall$City)[levels(aoipolyall$City) == "Velyki_Kopani"] <- "Velyki-Kopani"
levels(aoipolyall$City)[levels(aoipolyall$City) == "Bila_Tserkva"] <- "Bila-Tserkva"
levels(aoipolyall$City)[levels(aoipolyall$City) == "Zaporizhia"] <- "Zaporizhzhia"

rmv <- setdiff(aoipolyall$City, unique(IMG$City))


aoipolyall <- filter(aoipolyall, City != rmv)





# 1.3) Satellite image polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## List all files in the folders -- full path names
fp <- list.files(path = "GIS/SatelliteImage/Img_coverage",
                 recursive = TRUE,
                 pattern = "\\.geojson$",
                 full.names = TRUE)


## List all files in the folders -- only partial path name
fp2 <- list.files(path = "GIS/SatelliteImage/Img_coverage/",
                  recursive = TRUE,
                  pattern = "\\.geojson$",
                  full.names = F)



## Remove b_s1/s2 images (older image version not in use anymore)
idxbs <- grep("b_s", fp)

if(length(idxbs) != 0){
  fp <- fp[-idxbs]
  fp2 <- fp2[-idxbs]
}


## Now load the shapefiles specific to each satellite image
imgpoly <- list()
for(i in seq_along(fp)) {
  imgpoly[[i]] <- st_read(fp[i])
  names(imgpoly)[[i]] <- basename(dirname(fp))[i]
  #imgpoly[[i]] <- st_transform(imgpoly[[i]], crs = 6381) # Project to CRS with units set in meters (to make the grid in the units of meters)
}



## Keep only relevant columns 
for(i in seq_along(imgpoly)){
  imgpoly[[i]] <- imgpoly[[i]]['geometry']
}


## Combine polygons of stitched images into multipolygon category
imgpoly2 <- list() #Make empty list to store results
fname <- strsplit(fp2, "/") #Split partial path names to retrieve city-date in forloop

for(i in seq_along(imgpoly)){
  
  DIM <- dim(imgpoly[[i]])[1]
  
  if(DIM > 1){
    imgpoly2[[i]] <- st_combine(imgpoly[[i]])
    
    ## Convert sfc to sf object
    imgpoly2[[i]] <- st_as_sf(imgpoly2[[i]])
    st_geometry(imgpoly2[[i]])  <- 'geometry' #Retrieve original geometry name 
    
  } else{
    imgpoly2[[i]] <- imgpoly[[i]]
  }
  
  ## Append City-Date column 
  imgpoly2[[i]]$City <- fname[[i]][1]
  imgpoly2[[i]]$Date <- fname[[i]][2]
  
}


## Combine all polygons into single data frame
imgpolyall <- do.call(what = sf:::rbind.sf, args=imgpoly2)




# 1.4) Load worldpop raster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CAUTION - See comment above at the start of this script!



if(DINPUT == "Count"){
  
  if(isTRUE(Sys.info()['effective_user'] == 'marie-christinerufener')){
    #worldpop <- raster("GIS/Population/WorldPop/Shapefile/Numbers/Constrained/v2/100m/ukr_pop_2020_100m_constrained_v2.tif") # 100m resolution
    worldpop <- raster("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GIS/Population/WorldPop/Shapefile/Numbers/Unconstrained/100m_resoultion/ukr_ppp_2019_UNadj.tif") # 100m resolution
    
  } else{
    worldpop <- raster("GIS/Population/WorldPop/Shapefile/Numbers/Uncontrained/100m_resolution/ukr_ppp_2019_UNadj.tif") # 100m resolution
    stop('CAUTION - Aggregated data is being loaded! For the original data, please refer to the comment at the start of this script.')
  }
  
  
} else if(DINPUT == "Density"){
  worldpop <- raster("GIS/Population/WorldPop/Shapefile/Density/ukr_pd_2019_1km_UNadj.tif") # 1km resolution
}



# 1.4) Adm-0 (Oblast)
#~~~~~~~~~~~~~~~~~~~~~~~
## Only used for plotting - not for analytical purposes
ukr <- st_read("GIS/Administrative_divisions/Ukraine/Adm_0/Without_CrimeaSevastopol/ukr_adm0_filtered.shp") %>%
  st_transform(crs = 6381) #Transform to UTM in order to make the grid



# 1.5) Load car coordinates data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# There is a separate csv file for each satellite image (city-date)
# Here, all individual csv files will be loaded and then bound into a single data frame


## List all files

### With full path
if(CARS == "original"){
  
  if(THRESHOLD == 'TH_15'){
    
    fp <- list.files("Data/Cars/Car_coords/th_015_oct22_carclass_18/original/",
                     full.names = TRUE,
                     pattern = ".csv" )
    
  } else if(THRESHOLD == 'TH_45'){
    
    fp <- list.files("Data/Cars/Car_coords/th_045_carclass_18/original/",
                     full.names = TRUE,
                     pattern = ".csv" )
    
  }
  
} else if(CARS == "filtered"){
  
  if(THRESHOLD == 'TH_15'){
    
    fp <- list.files("Data/Cars/Car_coords/th_015_oct22_carclass_18/OSM_filtered/",
                     full.names = TRUE,
                     pattern = ".csv" )
    
  } else if(THRESHOLD == 'TH_45'){
    
    fp <- list.files("Data/Cars/Car_coords/th_045_carclass_18/OSM_filtered/",
                     full.names = TRUE,
                     pattern = ".csv" )
  }
  
}



### Only file names
if(CARS == "original"){
  
  if(THRESHOLD == 'TH_15'){
    
    fp2 <- list.files("Data/Cars/Car_coords/th_015_oct22_carclass_18/original/",
                      full.names = FALSE,
                      pattern = ".csv" )
    
  } else if(THRESHOLD == 'TH_45'){
    
    fp2 <- list.files("Data/Cars/Car_coords/th_045_carclass_18/original/",
                      full.names = FALSE,
                      pattern = ".csv" )
    
  }
  
} else if(CARS == "filtered"){
  
  if(THRESHOLD == 'TH_15'){
    
    fp2 <- list.files("Data/Cars/Car_coords/th_015_oct22_carclass_18/OSM_filtered/",
                      full.names = FALSE,
                      pattern = ".csv" )
    
  } else if(THRESHOLD == 'TH_45'){
    
    fp2 <- list.files("Data/Cars/Car_coords/th_045_carclass_18/OSM_filtered/",
                      full.names = FALSE,
                      pattern = ".csv" )
  }
  
}


#### Remove b_s1/s2 images (older image version not in use anymore)
idxbs <- grep("b_s", fp)

if(length(idxbs) != 0){
  fp <- fp[-idxbs]
  fp2 <- fp2[-idxbs]
}



## Read all files and retrieve city & date information
### Get list of city names and dates

df <- strsplit(fp2, split="_")
cars <- list()

for(i in seq_along(fp2)){
  
  if(length(df[[i]]) == 7){ #For cities composed by single name
    city <- df[[i]][3]
    date <- paste(df[[i]][4],df[[i]][5],df[[i]][6], sep="_")
  } else { #For cities with composite names (e.g., Bila-Tserkva, Velyki-Kopani)
    city <- paste(df[[i]][3], df[[i]][4], sep = "-")
    date <- paste(df[[i]][5],df[[i]][6],df[[i]][7], sep="_")
  }
  
  
  cars[[i]] <- read.csv(fp[i])
  
  cars[[i]]$City <- city
  cars[[i]]$Date <- date
  
}

cars2 <- do.call("rbind", cars)


## Make Month & Year columns
cars2$Month <- substr(cars2$Date,1,3)
cars2$Year <- substr(cars2$Date,5,8)

levels(factor(cars2$Month))
levels(factor(cars2$Year))



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Make the spatial grid 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Because there are images that do not cover 100% of the AOI,
# we have to build a grid that is specific to each imagery extent.
# However, we have to assure that the grid ID remains constant for images
# belonging to the same city. The reason for this is that when we aggregate the
# data to calculate monthly car averages, we have to make sure that we have
# the same grid ID for months containing >1 image & whose areas are likely to overlap.
# In such case, we calculate the average car count for the given grid id.

# To assure this, we first build spatial grids for the AOI of each city.
# Then, we take the city-specific grid and crop it further to the extent of the city-specific image.
# This preserves the grid ID across images belonging to the same city (hence, AOI)


#st_crs(ukr)$units_gdal #Checks the CRS units

spgrid_aoi <- list() #List to store the grids


# 2.1) General grid 
#~~~~~~~~~~~~~~~~~~~~~~
## Spatial grid based on AOI polygon 
# for(i in seq_along(aoipoly)){
#   spgrid_aoi[[i]] <- aoipoly[[i]] %>%
#     st_make_grid(square = T, cellsize = c(GRID_SPACING, GRID_SPACING)) %>%
#     st_intersection(aoipoly[[i]]) %>% #Crop grid to AOI polygons
#     st_sf() %>%
#     mutate(grid_id = 1:nrow(.), #get grid id
#            area = as.numeric(st_area(.)))  %>% # Calculate the area of each grid cell 
#     filter(area == 1000000) %>% # filter out grid cells that were cut by the AOI polygon (ie, cell size < 10e6 m2 (1x1km))
#     st_transform(crs = 4326) #Transform back to wgs - https://epsg.io/32618
# }


for(i in seq_along(aoipoly)){
  spgrid_aoi[[i]] <- aoipoly[[i]] %>%
    st_make_grid(square = T, cellsize = c(GRID_SPACING, GRID_SPACING)) %>%
    st_intersection(aoipoly[[i]]) %>% #Crop grid to AOI polygons
    st_sf() %>%
    mutate(grid_id = 1:nrow(.))  %>% # Calculate the area of each grid cell 
    st_transform(crs = 4326) #Transform back to wgs - https://epsg.io/32618
}

names(spgrid_aoi) <- names(aoipoly) #Set City names to list elements



# 2.2) Crop AOI-based grid to image polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setdiff(names(spgrid_aoi), unique(imgpolyall$City)) # Oops, standardize name

imgpolyall$City <- as.factor(imgpolyall$City)
levels(imgpolyall$City)[levels(imgpolyall$City) == "Zaporizhzhia"] <- "Zaporizhia"

setdiff(names(spgrid_aoi), unique(imgpolyall$City)) # Sanity check


spgrid_img <- list() # Make empty list to store the results


## Intersect City-specific grid to city-specific image
for(i in 1:nrow(imgpolyall)){
  
  ## Get input city
  CITY <- paste(imgpolyall$City[i])
  
  ## Get grid for input city
  idx <- which(names(spgrid_aoi) == CITY)
  grid <- spgrid_aoi[[idx]]
  
  
  ## Intersect grid with image extent
  spgrid_img[[i]] <- imgpolyall[i,] %>%
    st_intersection(grid) #Crop grid to Image polygon extent
  
  
  ## Calculate grid area
  spgrid_img[[i]] <- spgrid_img[[i]] %>% 
    st_transform(crs = 6381) %>% 
    mutate(area = as.numeric(st_area(geometry)))
  
  
  ## Now filter out grid cells with area < 1km2 (there are cells that were cropped by the imagery extent)
  spgrid_img[[i]] <- spgrid_img[[i]] %>%
    mutate(area = round(area)) %>%
    #filter(area == 1000000)
    filter(area >= 990000) #There are some cells that are slightly smaller than 1km, but they can still be accounted for
  
  ## Tranform back to WGS84
  spgrid_img[[i]] <- spgrid_img[[i]] %>%
    st_transform(crs = 4326) #Transform back to wgs - https://epsg.io/32618
}


names(spgrid_img) <- paste(imgpolyall$City, imgpolyall$Date, sep ="_") #Set City-Date names to list elements


## Quick visualization
mapview(spgrid_aoi[[27]], alpha.regions = 0) + #Kiev
  mapview(spgrid_img[[435]], lwd = 2) + #Kiev
  #mapview(spgrid_img[[449]], lwd = 2) + #Kiev
  mapview(spgrid_img[[457]], lwd = 2)  #Kiev




# 2.3) Subset image-specific grid list and cars list 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## After the post-processing pipeline, we kept only a fraction
## of the original satellite images. We therefore need to
## subset the car and image-specific grid list to the city-date stamps
## that remained after the post-processing pipeline.
## To do so, we will use the aggregated data uploaded in section 1.1, 
## and get the city-date stamp from there to check which ones we will keep.


### First standardize some City names
cars2$City <- as.factor(cars2$City)
levels(cars2$City)[levels(cars2$City) == "Bila-Tserkva"] <- "Bila_Tserkva"
levels(cars2$City)[levels(cars2$City) == "Velyki-Kopani"] <- "Velyki_Kopani"
levels(cars2$City)[levels(cars2$City) == "Zaporizhzhia"] <- "Zaporizhia"


IMG$City <- as.factor(IMG$City)
levels(IMG$City)[levels(IMG$City) == "Bila-Tserkva"] <- "Bila_Tserkva"
levels(IMG$City)[levels(IMG$City) == "Velyki-Kopani"] <- "Velyki_Kopani"
levels(IMG$City)[levels(IMG$City) == "Zaporizhzhia"] <- "Zaporizhia"

setdiff(levels(cars2$City), levels(IMG$City))



### Update CityDate column
cars2$CityDate <- factor(paste(cars2$City, cars2$Date, sep="_"))
IMG$Image <- factor(paste(IMG$City, IMG$Date, sep="_"))


### Now subset the car list data
idx <- intersect(levels(cars2$CityDate), levels(factor(IMG$Image)))
cars2 <- filter(cars2, CityDate %in% idx)



### And then the image-grid list
idx2 <- which(names(spgrid_img) %in% levels(as.factor(IMG$Image)))
spgrid_img <- spgrid_img[idx2]



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Count No. people & cars per grid cell
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We will calculate the total number of people and then cars at the grid cell level


# 3.1) Extract total population counts/average density for spatial grid
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note: For count data, we calculate the total no. person per grid cell.
# For density data, we calculate the average density per grid cell.

crs(worldpop)
worldpop_grid <- list() #Set empty list to store the results


if(DINPUT == "Count"){
  
  for(i in seq_along(spgrid_img)){
    print(i)
    worldpop_grid[[i]] <- exact_extract(worldpop, spgrid_img[[i]], 'sum', append_cols=("grid_id"))
    colnames(worldpop_grid[[i]]) <- c("grid_id", "Npop")
  }
} else if(DINPUT == "Density"){
  for(i in seq_along(spgrid_list)){
    worldpop_grid[[i]] <- exact_extract(worldpop, spgrid_img[[i]], 'mean', append_cols=("grid_id"))
    colnames(worldpop_grid[[i]]) <- c("grid_id", "Dpop")
  }
}

names(worldpop_grid) <- names(spgrid_img) #Set City names to list elements


## Append City column to the grids
for(i in seq_along(worldpop_grid)){
  worldpop_grid[[i]]$City <- paste(names(worldpop_grid)[i])
}


## Merge population count data to spatial grid data -- For plotting
worldpop_sf <- list()
for(i in seq_along(spgrid_img)){
  worldpop_sf[[i]] <- merge(worldpop_grid[[i]], spgrid_img[[i]], by="grid_id")
}

worldpop_sf <- lapply(worldpop_sf, st_as_sf) ## Set it to sf object - for plotting
names(worldpop_sf) <- names(spgrid_img)


### Quick visualization
mapview(spgrid_aoi[[27]], alpha.regions = 0) + #Kiev
  mapview(worldpop_sf[[245]], zcol = 'Npop')



# 3.2) Extract number of cars for spatial grid
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# In order to evaluate the relationship on a month-year basis
# We will calculate the average No. of cars for a given city within a given grid cell.
# To do so, we first calculate the total No. of cars for each grid cell for a given image (city-date),
# and then calculate the average no. of cars (aggregation done on city, year, month, gird cell level)


#st_crs(spgrid)$proj4string


## Set cars data frame to sf object
cars2 <- st_as_sf(cars2, coords = c("Longitude", "Latitude"),
                  crs = st_crs(spgrid_img[[1]])$proj4string) #take an arbitrary grid to set CRS -- all grids have the same CRS


## Split data by City-Date
cars2$CityDate <- factor(cars2$CityDate)

cars_list <- split(cars2, cars2$CityDate) #Now split the data



## Calculate total no. of cars
cars_grid_list <- list() #Make empty list to store the results

for(i in seq_along(cars_list)){
  print(i)
  
  ind <- paste(unique(cars_list[[i]]$CityDate))
  
  cars_grid_list[[i]] <- spgrid_img[[ind]]
  cars_grid_list[[i]]$Ncars <- lengths(st_intersects(spgrid_img[[ind]], cars_list[[i]]))
  
  cars_grid_list[[i]]$City <- unique(cars_list[[i]]$City)
  cars_grid_list[[i]]$Year <- unique(cars_list[[i]]$Year)
  cars_grid_list[[i]]$Month <- unique(cars_list[[i]]$Month)
  cars_grid_list[[i]]$CityDate <- names(cars_list)[i]
  
  
  cat("Finished counts for:  ", names(cars_list)[i], "\n",sep="")  
  
}


### Quick visualization
mapview(spgrid_aoi[[27]], alpha.regions = 0) + #Kiev
  mapview(worldpop_sf[[245]], zcol = 'Npop') +
  mapview(cars_grid_list[[245]], zcol = 'Ncars')



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Merge population to cars data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To have all information together

## Drop geometry (for faster calculations)
worldpop_sf2 <- list()
for(i in seq_along(worldpop_sf)){
  worldpop_sf2[[i]] <- st_drop_geometry(worldpop_sf[[i]])
}


cars_grid_list2 <- list()
for(i in seq_along(cars_grid_list)){
  cars_grid_list2[[i]] <- st_drop_geometry(cars_grid_list[[i]])
}

## Bind list into single data frame
popdf <- do.call('rbind', args=worldpop_sf2)
cardf <- do.call('rbind', args=cars_grid_list2)


## Merge the two data
colnames(popdf)[3:4] <- c('CityDate','City')
setdiff(cardf$CityDate, popdf$CityDate) #Sanity check

popcar <- merge(cardf, popdf, by = c('CityDate','grid_id'))

dim(popcar)[1] == dim(cardf)[1] # Sanity check - needs to have the same dimension


## remove obsolete columns
popcar[,c('City.y', 'Date.y')] <- NULL

## Rename replicated columns
colnames(popcar)[3:4] <- c('City', 'Date')


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~
# 5) Aggregate data 
#~~~~~~~~~~~~~~~~~~~~~~

# To evaluate the relationship between Cars vs. People,
# data will be aggregated on a monthly basis. This means 
# that for cars, we will calculate the average no. of cars
# for a given grid cell belonging to a given City-Month-Year.

# Note that any desired temporal resolution could be chosen for the aggregation.



# 5.1) Calculate average car abundance for given temporal resolution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Averages are calculated for a given city within a given month of a given year for each grid cell


## First drop empty factor levels
popcar[,c('CityDate','grid_id', 'City', 'Date', 'Year', 'Month')] <- lapply(popcar[,c('CityDate','grid_id', 'City', 'Date', 'Year', 'Month')], factor)



popcar_aggr <- popcar %>% 
  #group_by(Year, Month, grid_id) %>%
  group_by(City, Year, Month, grid_id) %>%
  summarize(Ncars_avg = mean(Ncars, na.rm=T),
            Ncars_median = median(Ncars, na.rm=T),
            Ncars_sd = sd(Ncars, na.rm=T),
            Npop = mean(Npop, na.rm = T),
            City = paste(unique(City)),
            Year = paste(unique(Year)),
            Month = paste(unique(Month)),
            grid_id = paste(unique(grid_id))) %>% 
  data.frame()


## Insert column to identify confidence threshold (might be useful sometime later)
popcar_aggr$Threshold <- THRESHOLD


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6) Append data back to spatial grid 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## To perform the two previous steps, we had to drop the
## geometry from the spatial data objects, such that calculations could be performed faster.
## Now we have to retrieve this information again, if at some point we want to visualize the results.

popcar_aggr_grid <- list()

for(i in seq_along(spgrid_aoi)){
  
  CITY <- names(spgrid_aoi)[i]
  grid <- spgrid_aoi[[i]]
  
  tmp <- filter(popcar_aggr, City == CITY)
  
  popcar_aggr_grid[[i]] <- merge(grid, tmp, by = "grid_id")
  
}

names(popcar_aggr_grid) <- names(spgrid_aoi)


## Quick visualization
popcar_aggr_grid[[14]] %>% #Mariupol
  filter(Year == '2022' & Month == 'mar') %>%
  mapview(zcol = 'Ncars_avg') 
#mapview(zcol = 'Npop') 


popcar_aggr_grid[[27]] %>% #Kyiv
  filter(Year == '2019' & Month == 'mar') %>%
  #mapview(zcol = 'Ncars_avg') 
  mapview(zcol = 'Npop') 



# m <- popcar_aggr_grid[[27]] %>% #Kyiv
#   filter(Year == '2019' & Month == 'mar') %>%
#   mapview(zcol = 'Ncars_avg') 
# 
# setwd("~/Desktop")
# mapshot(m, url = paste0(getwd(), "/ukraine_ncars_map_march_2019.html"))
# mapshot(m, file = paste0(getwd(), "/ukraine_ncars_map_march_2019.png"))



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~~~~~~
# 7) Save the results
#~~~~~~~~~~~~~~~~~~~~~~

# We could save only the popcar_aggr_grid list through the saveRDS command.
# However, it is prudent to keep some intermediate files such as the
# City-specific and Image-specific grids, in case we want to return to them at some point.
# Thus, we will save the full environment.

# To save the environment, we will keep only the most relevant objects in order
# to reduce the final file size.



### Output file name
#OUTFILE <- paste("Npop_cars_", GRID_SPACING, "_m", sep="")
OUTFILE <- paste("Npop_cars_", GRID_SPACING, "_m_updated", sep="")


### output directory
if(CARS == "original"){
  
  if(THRESHOLD == 'TH_15'){
    #OUTDIR <- "Imagery_EDA/WorldPop_vs_Ncars/Confidence_Threshold/TH15/carclass_18/original/"
    OUTDIR <- "Data/GriddedPopCar/TH15/carclass_18/original/"
    
    
    
  } else if(THRESHOLD == 'TH_45'){
    #OUTDIR <- "Imagery_EDA/WorldPop_vs_Ncars/Confidence_Threshold/TH45/carclass_18/original/"
    OUTDIR <- "Data/GriddedPopCar/TH45/carclass_18/original/"
  }
  
} else if(CARS == "filtered"){
  if(THRESHOLD == 'TH_15'){
    #OUTDIR <- "Imagery_EDA/WorldPop_vs_Ncars/Confidence_Threshold/TH15/carclass_18/osm_filtered/"
    OUTDIR <- "Data/GriddedPopCar/TH15/carclass_18/osm_filtered/"
    
  } else if(THRESHOLD == 'TH_45'){
    #OUTDIR <- "Imagery_EDA/WorldPop_vs_Ncars/Confidence_Threshold/TH15/carclass_45/osm_filtered/"
    OUTDIR <- "Data/GriddedPopCar/TH45/carclass_18/osm_filtered/"
  }
}


### Make output file
OUT <- paste(OUTDIR, OUTFILE, ".RData",sep="")


### Remove objects from environment that won't be used 
rm(list = setdiff(ls(), c('popcar_aggr_grid','spgrid_aoi', 'spgrid_img', 'OUT', 'popcar2')))


save.image(file = OUT)


