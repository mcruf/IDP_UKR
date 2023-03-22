##################################################
#                                                #
#         Aggregate car detections data          #
#                                                #
##################################################


# The following script aims aggregates the geolocated car detection file,
# such that we have the the total number of cars per image.
# It is essentially meant to be applied once the geolocated car detection files
# have been cleaned by the OSM_Filtering_script_01.R pipeline - ie, removal of false-positives.



## Code written by: Marie-Christine Rufener < macrufener@gmail.com >
## Last update: March 2023



#~~~~~~~~~~~~~~~~~~
## Load libraries
#~~~~~~~~~~~~~~~~~~
library(dplyr)
# library(ggplot2)
# library(viridis)
# library(ggsci)
# library(ggpubr)


#~~~~~~~~~~~~~~~
# Default inputs
#~~~~~~~~~~~~~~~


## Car geolocations
CARS <- c("original", "filtered") [2] #original = original car detection file, filtered = car detection file filtered by OSM layers (see script: OSM_Filtering.R); default is filtered


## Confidence threshold (based on small vehicles only - object class 18)
THRESHOLD <- c("TH_15", "TH_45") [2] # 0.15 and 0.45 (less and more conservative thresholds); default is 0.45


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>><>


## NOTE: please adapt folder paths according to your needs


#~~~~~~~~~~~~~~~
# 1) Load data
#~~~~~~~~~~~~~~~
# There is a separate csv file for each satelltile image (city-date)
# Here, all individual csv files will be loaded and then bound into a single data frame

setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine")


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


#~~~~~~~~~~~~~~~~~~~~
# 2) Aggregate data
#~~~~~~~~~~~~~~~~~~~~
## Get total no. of cars per City-Date (i.e, image)


# 2.1) Make ID column
#~~~~~~~~~~~~~~~~~~~~~~
cars2$Image <- factor(paste(cars2$City, cars2$Date, sep = "_"))
nlevels(cars2$Image) #No. of images; Originally we have 1009 images - 43 images apparently did not have any cars detected (could arise either from the more conservative threshold (0.45) and/or from the OSM filtering)


# 2.2) Aggregate data
#~~~~~~~~~~~~~~~~~~~~~
dat <- cars2 %>% 
       group_by(Image) %>% #Group by image id
       summarize(Ncars = n(), #Count no. of cars = no. of rows within the image id (1 row = 1 car)
                 City = unique(City),
                 Date = unique(Date))  %>%
      data.frame()

nlevels(cars2$Image) == dim(dat)[1] ## Sanity check; should be TRUE



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Retrieve images with zero-car detections
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# It is important to distinguish true zeros (i.e., 'clean' images with no car detections), from
# false zeros (i.e., images fully obstructed by dense cloud layers).

# In order to distinguish these types of zeros, we will have to retrieve images with zero car detections
# from the general aggregated database that is provided by Ferda.
# This data base essentially provides an output similar to the data in 2.2, with the difference
# images with zero cars are kept in the aggregation process. 

# obtain_metadata_ukraine3.py -> Ferda's script that generate's this database; it is defined upon the user inputs (i.e., confidence threshold and vehicle classes).


# 3.1) Load the original aggregated data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## FIXME: Include data in the respective folders!
if(THRESHOLD == 'TH_15'){
  original <- read.csv('Data/Cars/Car_aggregated/th_015_carclass_18/')
} else if(THRESHOLD == 'TH_45'){
  original <- read.csv('Data/Cars/Car_aggregated/th_045_carclass_18/')
}

## Remove b_s1/s2 images (older image version not in use anymore)
idxbs <- grep("b_s", original$Label)

if(length(idxbs) != 0){
  original <- original[-idxbs,]
}


# 3.2) Identify missing Images
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Drop unused factor levels
original[, c('City', 'Label')] <- lapply(original[, c('City', 'Label')] , factor) 

## Standardize city names
setdiff(original$City, dat$City)

levels(original$City)[levels(original$City) == 'Bila_Tserkva'] <- 'Bila-Tserkva'
levels(original$City)[levels(original$City) == 'Velyki_Kopani'] <- 'Velyki-Kopani'

setdiff(original$City, dat$City) #Sanity check

## Make common ID column
original$Image <- factor(paste(original$City, original$Label, sep="_"))


## Append missing images
zero_img <- setdiff(original$Image, dat$Image) #Get missing images
original <- filter(original, Image %in% zero_img) #Filter original data only for missing data

### Recreate data frame with necessary infomartion (needs to be same structure as the aggregated data (dat) above)
missing <- data.frame(Image = factor(original$Image),
                      Ncars = rep(0, dim(original)[1]),
                      City = factor(original$City),
                      Date = factor(original$Label))

### Now bind the missing info to aggregated data
dat <- rbind(dat, missing)


#~~~~~~~~~~~~~~~~~~~~~
# 4) Save the output
#~~~~~~~~~~~~~~~~~~~~~
if(CARS == 'original' & THRESHOLD == 'TH_15'){
  OUTDIR <- file.path('Data', 'Cars','Car_aggregated', 'Original','th_015_carclass_18')
} else if(CARS == 'original' & THRESHOLD == 'TH_45'){
  OUTDIR <- file.path('Data', 'Cars','Car_aggregated', 'Original','th_045_carclass_18')
} else if(CARS == 'filtered' & THRESHOLD == 'TH_15'){
  OUTDIR <- file.path('Data', 'Cars','Car_aggregated', 'OSM_filtered','th_015_carclass_18')
} else if(CARS == 'filtered' & THRESHOLD == 'TH_45'){
  OUTDIR <- file.path('Data', 'Cars','Car_aggregated', 'OSM_filtered','th_045_carclass_18')
}

OUTFILE <- paste(OUTDIR, 'aggregated_cars.R', sep='/')
write.csv(dat, OUTFILE, row.names = FALSE)
