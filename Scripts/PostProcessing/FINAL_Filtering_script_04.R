#############################################
#                                           #
#          Generate final data              #
#                                           #
#############################################

# In this script, we will generate the final dataset that will be used
# for the data analysis.

# We will append imagery related information to the OSM-cleaned aggregated car data.
# In addition, we will remove images that are below the population threshold.
# Finally, we will remove images with zero car counts that are due to cloud obstructions (images fully covered by dense cloud layers)


## Code written by: Marie-Christine Rufener < macrufener@gmail.com > 
## Last update: March 2023


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~
library(dplyr)
library(lubridate)


#~~~~~~~~~~~~~~~
# Default inputs
#~~~~~~~~~~~~~~~
## Confidence threshold (based on small vehicles only - object class 18)
THRESHOLD <- c("TH_15", "TH_45") [2] # 0.15 and 0.45 (less and more conservative thresholds); default is 0.45


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Load the data files
#~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine") ## Set appropriate WD


# 1.1) Load OSM-filtered aggregated car data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(THRESHOLD == 'TH_15'){
  cars <- read.csv('Data/Cars/Car_aggregated/OSM_filtered/th_015_carclass_18/aggregated_cars.csv')
} else if(THRESHOLD == 'TH_45'){
  cars <- read.csv('Data/Cars/Car_aggregated/OSM_filtered/th_045_carclass_18/aggregated_cars.csv')
}


# 1.2) Load Population coverage data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## This data was geneated by the POP_Filtering.R script.
## It classifies each satellite image with respect to its population coverage.
popcov <- read.csv('Data/Population/Coverage/Population_coverage.csv')


# 1.3) Load image feature data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Retrieve imagery-related features (off-Nadir angle, cloud coverage, etc.)
imgf <- read.csv("Data/Satellite_Images/ImageFeatures/image_features_oct22.csv") ##Off-nadir, cloud cover, etc.
imgf2 <- read.csv("Data/Satellite_Images/ImageFeatures/Downloaded_imagery_summary_oct22.csv") ## image coverage, AOI coverage, etc.
imgf3 <- read.csv("Data/Satellite_Images/ImageFeatures/list_cities.csv") # ADM info + data collection method



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Merge the different data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.1) Merge population coverage to car data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Standardize city names
popcov$City <- as.factor(popcov$City) ## Set to factor

setdiff(popcov$City, cars$City)
levels(popcov$City)[levels(popcov$City) == "Bila_Tserkva"] <- "Bila-Tserkva"
levels(popcov$City)[levels(popcov$City) == "Velyki_Kopani"] <- "Velyki-Kopani"

setdiff(popcov$City, cars$City) # Sanity check

## Create image identifier
popcov$Image <- factor(paste(popcov$City, popcov$Date, sep="_"))

## merge the data
cars <- merge(cars, popcov[,c('Npop_img', 'Npop_aoi', 'Proportion', 'Pop_threshold', 'Image')], by='Image')



# 2.2) Merge image features to car data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### For imgf data ###
#####################

## Standardize city names
imgf$city <- as.factor(imgf$city) ## Set to factor

setdiff(imgf$city, cars$City)
levels(imgf$city)[levels(imgf$city) == "Bila_Tserkva"] <- "Bila-Tserkva"
levels(imgf$city)[levels(imgf$city) == "Velyki_Kopani"] <- "Velyki-Kopani"

setdiff(imgf$city, cars$City) # Sanity check

## Create image identifier
imgf$Image <- factor(paste(imgf$city, imgf$date, sep='_'))

## merge the data
cars <- merge(cars, imgf[,c('cloudCover', 'offNadirAngle', 'sunElevation', 'sunAzimuth', 'legacyId', 'acquisitionDate', 'Image')], by = 'Image')




### For imgf2 data ###
#######################

## Standardize city names
colnames(imgf2)[1:2] <- c('City', 'Date')
imgf2$City <- as.factor(imgf2$City) ## Set to factor


setdiff(imgf2$City, cars$City)
levels(imgf2$City)[levels(imgf2$City) == "Bila_Tserkva"] <- "Bila-Tserkva"
levels(imgf2$City)[levels(imgf2$City) == "Velyki_Kopani"] <- "Velyki-Kopani"

setdiff(imgf2$City, cars$City) # Sanity check

## Create image identifier
imgf2$Image <- factor(paste(imgf2$City, imgf2$Date, sep='_'))


## merge the data
cars <- merge(cars, imgf2[,c('AOI_area_sqkm', 'AOI_area_covered_sqkm', 'AOI_area_covered_perc', 'Perc_area_covered_by_feature', 'N_features_in_img', 'Image_resolution', 'Image')], by = 'Image')


### For imgf3 data ###
#######################

## Standardize city names
imgf3$City <- as.factor(imgf3$City) ## Set to factor


setdiff(imgf3$City, cars$City)
levels(imgf3$City)[levels(imgf3$City) == "Bila Tserkva"] <- "Bila-Tserkva"
levels(imgf3$City)[levels(imgf3$City) == "Velyki Kopani"] <- "Velyki-Kopani"
levels(imgf3$City)[levels(imgf3$City) == "Kyiv city"] <- "Kyiv"

setdiff(imgf3$City, cars$City) # Sanity check


## merge the data
cars <- merge(cars, imgf3[,c('Oblast', 'Raion', 'City', 'Selection_reason')], by = 'City')




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Add additional information & reorganize data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 3.1) Retrieve weighted average features for stiched images
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For the weights, we will use the AOI covered area from each image
stiched <- subset(cars, N_features_in_img > 1)

# NOTE: IMPROVE THE STEPS BELOW
clouds <- strsplit(stiched$cloudCover, ","); clouds <- lapply(clouds, as.numeric)
nadir <- strsplit(stiched$offNadirAngle, ","); nadir <- lapply(nadir, as.numeric)
sunelevation <- strsplit(stiched$sunElevation, ","); sunelevation <- lapply(sunelevation, as.numeric)
sunazimuth <- strsplit(stiched$sunAzimuth, ","); sunazimuth <- lapply(sunazimuth, as.numeric)
acquisitionDate <- strsplit(stiched$acquisitionDate, ",")


weights <- strsplit(stiched$Perc_area_covered_by_feature, ","); weights <- lapply(weights, as.numeric)

features <- NULL
for(i in seq_along(clouds)){
  cl = weighted.mean(clouds[[i]], weights[[i]])
  nad = weighted.mean(nadir[[i]], weights[[i]])
  sune = weighted.mean(sunelevation[[i]], weights[[i]])
  suna = weighted.mean(sunazimuth[[i]], weights[[i]])
  DateTime = acquisitionDate[[i]][1] #Tkae first element for proxy
  
  features = rbind(features, data.frame(cl, nad, sune, suna, DateTime))
  
}

features$Image <- paste(factor(stiched$Image))


## Now merge the info back to the original df
cars <- full_join(cars,features, by = "Image")


## Reset row values where n stiched images = 1
idx <- which(is.na(cars$cl))

cars[idx, "cl"] <- as.numeric(paste(cars[idx, "cloudCover"] ))
cars[idx, "nad"] <- as.numeric(paste(cars[idx, "offNadirAngle"] ))
cars[idx, "sune"] <- as.numeric(paste(cars[idx, "sunElevation"] ))
cars[idx, "suna"] <- as.numeric(paste(cars[idx, "sunAzimuth"] ))
cars[idx, "DateTime"] <- paste(cars[idx, "acquisitionDate"] )


## Let's do some cleanup
cars[,c("cloudCover", "offNadirAngle", "sunElevation","sunAzimuth")] <- NULL
colnames(cars)[20:23] <- c("cloudCover", "offNadirAngle", "sunElevation","sunAzimuth")


# 3.2) Get time-realted info
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cars$DateTime <- ymd_hms(cars$DateTime) #Transform to Date-Time data

cars$Year <- year(cars$DateTime)
cars$Month <- month(cars$DateTime)
cars$Day <- day(cars$DateTime)
cars$Hour <- hour(cars$DateTime)
cars$WeekDay <- weekdays(cars$DateTime)


# 3.3) Reorganize data
#~~~~~~~~~~~~~~~~~~~~~~~~
colnames(cars)

cars <- cars[,c('Image', 'legacyId','City', 'Oblast', 'Raion', 'Date', 'acquisitionDate',
                'DateTime','Year', 'Month', 'Day', 'Hour', 'WeekDay',
                'AOI_area_sqkm', 'AOI_area_covered_sqkm', 'AOI_area_covered_perc', 'Perc_area_covered_by_feature', 'N_features_in_img', 'Selection_reason',
                'Image_resolution', 'cloudCover', 'offNadirAngle', 'sunElevation', 'sunAzimuth', 
                'Npop_aoi', 'Npop_img','Proportion', 'Pop_threshold', 'Ncars')]


#~~~~~~~~~~~~~~~~~~~
# 4) Filter data
#~~~~~~~~~~~~~~~~~~~

# We will remove any image below the population threshold.
# Also, images in which no cars were detected, and which were due to complete cloud obstruction, will also be removed.


# 4.1) Population filtering
#~~~~~~~~~~~~~~~~~~~~~~~~~~
cars2 <- filter(cars, Pop_threshold == 'Above') #Keep only images above population coverage threshold


# 4.2) Zero-car counts
#~~~~~~~~~~~~~~~~~~~~~
zero <- which(cars2$Ncars == 0) #Identify images with zero car counts
images <- cars2[zero,'Image'] 
images # When checking the corresponding satellite images, we can see that these were all fully (or almost fully) obstructed by clouds

cars2 <- cars2[-zero,]



