#######################################################################
#                                                                     #
#      Evaluate relationship between No. people and No. of cars       #
#                                                                     #
#           - Part 2a: Predict IDPs from cars: ratio method -         #
#                                                                     #
#######################################################################


# This is the second script from the car~pop relationship evaluation.
# It relies on the output from the get_gridded_cars_and_pop_script01.R.
# We will used gridded pop/car data retrieved at a 1x1 km spatial granularity,
# as it marked the optimal granularity at which the relationship is most preserved.
# The relationship at larger granularity (e.g., 5km) becomes more fuzzy, especially for
# cities with a small extent.

# The rationale behind this script is to infer the car~population relationship 
# from the baseline year (2019), and use the estimated parameters to predict
# population during the conflict year (2022) from cars observed in the same year.

# To do so, we will use two different methods:
# A) Ratio method: at the grid cell level, we calculate the pop/car ratio for the baseline
# and use the ratio as a multiplier for the same cell in 2022 to calculate the population
# B) GAM method: a GAM model is used to fit the pop~car relationship in 2019. The fitted model
# is then used to predict the 2022 population based on cars observed in the same year

## The current script applies the ratio method.


## Please update input & output folder paths accordingly 

## Code written by: Marie-Christine Rufener < macrufener@gmail.com >
## Last update: June 2023


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~~
library(ggplot2)
library(viridis)
library(dplyr)
library(scales)
library(ggsci)
library(sf)
library(mgcv)
library(mgcViz)
library(ggpubr)
library(mapview)




#~~~~~~~~~~~~~~~~~
# Default inputs
#~~~~~~~~~~~~~~~~~

## Car geolocations
CARS <- c("original", "filtered") [2] #original = original car detection file, filtered = car detection file filtered by OSM layers (see script: OSM_Filtering.R); default is filtered

## Confidence threshold (based on small vehicles only - object class 18)
THRESHOLD <- c("TH_15", "TH_45") [2] # 0.15 and 0.45 (less and more conservative thresholds); default is 0.45


## Standard working directory
#setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine")
setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GitHub/IDP_UKR/") 



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>><>

#~~~~~~~~~~~~~~~~~~~~~~
# 1) Read results file
#~~~~~~~~~~~~~~~~~~~~~~

# Define folder path to which results should be loaded
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(CARS == "original"){
  
  if(THRESHOLD == 'TH_15'){
    #sewd("Imagery_EDA/WorldPop_vs_Ncars/Confidence_Threshold/TH15/carclass_18/original/")
    sewd("Data/GriddedPopCar/TH15/carclass_18/original/")
    
    
  } else if(THRESHOLD == 'TH_45'){
    #setwd("Imagery_EDA/WorldPop_vs_Ncars/Confidence_Threshold/TH45/carclass_18/original/")
    setwd("Data/GriddedPopCar/TH45/carclass_18/original/")
    
  }
  
} else if(CARS == "filtered"){
  if(THRESHOLD == 'TH_15'){
    #setwd("Imagery_EDA/WorldPop_vs_Ncars/Confidence_Threshold/TH15/carclass_18/osm_filtered/")
    setwd("Data/GriddedPopCar/TH15/carclass_18/osm_filtered/")
    
  } else if(THRESHOLD == 'TH_45'){
    #setwd("Imagery_EDA/WorldPop_vs_Ncars/Confidence_Threshold/TH45/carclass_18/osm_filtered/")
    setwd("Data/GriddedPopCar/TH45/carclass_18/osm_filtered/")
    
  }
}


# Load the result file
#~~~~~~~~~~~~~~~~~~~~~~~
#load("Npop_cars_1000_m.RData")
# popcar <- popcar2; rm('popcar2')

load("Npop_cars_1000_m_updated.RData")



#~~~~~~~~~~~~~~~~~~~~~~
# 3) Reshaping data
#~~~~~~~~~~~~~~~~~~~~~~
# Subset data for baseline (2019) and conflict years (2022), and
# get months for which we have data for in each year.
# We also subset for the first covid year (2020) in order to
# verify that the population decrease/increase in war period make sense.


YEARS <- c('2019', '2020', '2022') #Define baseline year and years of interest


# 3.1) Get city names
#~~~~~~~~~~~~~~~~~~~~~~~~~
CITY <- unique(popcar$City)
cities <- c()

for(i in seq_along(CITY)){
  
  cit <- paste(CITY[i])
  
  #Filter data for given city
  tmp <- filter(popcar, City == cit & Year %in% YEARS) %>% 
    droplevels() 
  
  #Get number of years
  nyears <- nlevels(tmp$Year)
  years <- levels(tmp$Year)
  
  #Skip loop iteration for cities with data for only one year
  if(nyears == 1) next
  
  
  #Skip loop iteration for cities with no data for 2019  (main year of interest)
  if(isFALSE(any(years %in% "2019"))) next
  
  #Skip loop iteration for cities with no data for 2022 (second year of interest)
  if(isFALSE(any(years %in% "2022"))) next
  
  
  tab <- as.data.frame.matrix(table(tmp$Month, tmp$Year))
  tab[tab>1] <- 1 #transform to binary
  
  #if(isTRUE(any(rowSums(tab) >= 2))){ #Any row with sum >= 2 indicates matching months between both years
  if(isTRUE(any(rowSums(tab) >= 1))){ #Any row with sum >= 1 indicates that data is present for at least one of the months between the evaluated years
    
    
    cities[i] <- cit
  }
  
}

cities <- cities[!is.na(cities)] #Remove NAs




# 3.2) Get common months for city of interest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Correct city name (for plotting purposes)
cities[which(cities == 'Bila_Tserkva')] <- 'Bila-Tserkva' 
cities[which(cities == 'Velyki_Kopani')] <- 'Velyki-Kopani' 


names(popcar_aggr_grid)[which(names(popcar_aggr_grid) == 'Bila_Tserkva')] <- 'Bila-Tserkva' 
names(popcar_aggr_grid)[which(names(popcar_aggr_grid) == 'Velyki_Kopani')] <- 'Velyki-Kopani' 



####### ---> START LOOP HERE <---- #######


## Select city of interest
datall <- list()
logfile <- NULL

for(city in seq_along(cities)){
  
  int <- cities[city]
  
  print(paste('Calculating for:', int, sep = " "))
  
  
  ## Filter data for given city
  dat <- popcar_aggr_grid[[int]] %>%
    filter(Year %in% YEARS)
  
  
  ## Get common months between baseline and year(s) of interest
  dat_list <- split(dat, dat$Year)
  
  if(length(dat_list) == 2){ #Data for only 2019 & 2022
    comn1922 <- intersect(levels(factor(dat_list[[1]]$Month)),factor(dat_list[[2]]$Month)) # Between 2019 & 2022
  } else if(length(dat_list) == 3){ #Data for 2019, 2021 & 2022
    comn1920 <- intersect(levels(factor(dat_list[[1]]$Month)),factor(dat_list[[2]]$Month)) # Between 2019 & 2020
    comn1922 <- intersect(levels(factor(dat_list[[1]]$Month)),factor(dat_list[[3]]$Month)) # Between 2019 & 2022
  }
  
  ## Rework some columns
  for(i in seq_along(dat_list)){
    dat_list[[i]]$Month <- factor(dat_list[[i]]$Month)
    
    ## Round averaged values
    dat_list[[i]]$Ncars_avg <-  round(dat_list[[i]]$Ncars_avg)
    dat_list[[i]]$Npop <-  round(dat_list[[i]]$Npop)
  }
  
  
  # 3.3) Calculate average No. of cars & people per grid-cell for reference year
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The reference year may contain information from multiple months, each of which can cover a slightly 
  # different extent of the given city's AOI.
  # For robustness, for a given city we will calculate the average no. of cars per & people per grid cell
  # for the reference year (2019), and use the averaged values to estimate the pop~car relationship from the reference year.
  # This will then be used to predict the population during covid (2020) & conflict (2022) years
  
  
  ### First, split year-specific data on month
  if(length(dat_list) == 2){ #Data for only 2019 & 2022
    dat19 <- split(dat_list[[1]], dat_list[[1]]$Month)
    dat22 <- split(dat_list[[2]], dat_list[[2]]$Month)
  } else if(length(dat_list) == 3){ #Data for 2019, 2021 & 2022
    dat19 <- split(dat_list[[1]], dat_list[[1]]$Month)
    dat20 <- split(dat_list[[2]], dat_list[[2]]$Month)
    dat22 <- split(dat_list[[3]], dat_list[[3]]$Month)
  }
  
  ### Now subset for matching grid cells - DEPRECATED!
  # for(i in seq_along(comn)){
  #   
  #   tmp <- intersect(dat19[[i]]$grid_id, dat22[[i]]$grid_id) #Get matching grid cells
  #   
  #   dat19[[i]] <- filter(dat19[[i]], grid_id %in% tmp)
  #   dat22[[i]] <- filter(dat22[[i]], grid_id %in% tmp)
  # }
  
  
  ### Bind 2019 data into a single data frame
  dat19b <- do.call('rbind', dat19)
  dat19b$Month <- factor(dat19b$Month, levels = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))
  dat19b$grid_id <- as.factor(dat19b$grid_id) #Set grid_id to factor
  
  
  ### Now calculate averages 
  dat19c <- dat19b %>%
    group_by(grid_id) %>%
    summarize(City = paste(unique(City)),
              Year = paste(unique(Year)),
              Threshold = paste(unique(Threshold)),
              Ncars_avg = mean(Ncars_avg, na.rm=T),
              Ncars_med = median(Ncars_avg, na.rm=T),
              Npop = mean(Npop, na.rm = T))
  
  ### To see the progress
  # plot(dat19c$Ncars_avg~dat19c$Ncars_med) #To check whether mean or median should be used
  # 
  # mapview(dat19c, zcol = 'Ncars_avg') +
  #   mapview(dat19c, zcol = 'Ncars_med') +
  #   mapview(dat19c, zcol = 'Npop')
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4) Inferring IDPs from cars
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Approach A - Calculate the ratio between No. of cars & People at the grid cell level for the reference year.
  ## Take this ratio and multiply it for the number of cars for the same grid cell for the covid & war years.
  ## This approach essentially assumes that there is a linear and constant relationship between the different years.
  
  
  # 4.1) Calculate the pop~car ratio for the reference year
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dat19c$Ratio <- dat19c$Npop / dat19c$Ncars_avg
  
  ## Overcome Inf issue
  ### Happens whenever a grid cell has no cars detected, but a positive population
  ### We will replace it by the median ratio value
  idx <- which(is.infinite(dat19c$Ratio))
  replace <- median(dat19c[-idx,'Ratio']$Ratio)
  dat19c[idx, 'Ratio'] <- replace
  
  
  ## Overcome NaN issue
  ### Happens when there is zero cars and zero population
  idx2 <- which(is.na(dat19c$Ratio))
  dat19c[idx2, 'Ratio'] <- 0
  
  
  
  # 4.2) Look-up for matching grid cells between reference year and years of interest
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Between reference and covid year (when applicable)
  if(length(dat_list) == 3){
    
    for(i in seq_along(dat20)){
      tmp <- intersect(dat19c$grid_id, dat20[[i]]$grid_id) #Get matching grid cells
      dat20[[i]] <- filter(dat20[[i]], grid_id %in% tmp) #Filter for matching grid cells in reference year
      dat20[[i]]$grid_id <- as.factor(dat20[[i]]$grid_id) #Drop-out empty factor levels 
    }
    
  }
  
  ## Between reference and conflict year
  for(i in seq_along(dat22)){
    tmp <- intersect(dat19c$grid_id, dat22[[i]]$grid_id) #Get matching grid cells
    dat22[[i]] <- filter(dat22[[i]], grid_id %in% tmp) #Filter for matching grid cells in reference year
    dat22[[i]]$grid_id <- as.factor(dat22[[i]]$grid_id) #Drop-out empty factor levels 
  }
  
  
  
  # 4.3) Infer no. of people
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Infer no. of people from baseline car~pop ratio
  
  ### Covid year (when applicable) 
  if(length(dat_list) == 3){
    
    dat20b <- list()
    
    for(i in seq_along(dat20)){
      
      ## First, drop geometry to allow merging
      tmp <- st_drop_geometry(dat20[[i]]) 
      
      ## Merge month-specific 2020 data to pre-war data by grid id
      dat20b[[i]] <- merge(tmp, dat19c, by = c('grid_id', 'City', 'Threshold'))
      
      ## Correct some names for better interpretability
      colnames(dat20b[[i]]) <- c('grid_id', 'City', 'Threshold', 'Year_conflict', 'Month',
                                 'Ncars_avg_conflict', 'Ncars_med_conflict','Ncars_sd_conflict',
                                 'Npop_conflict', 'Year_prewar', 'Ncars_avg_prewar','Ncars_med_prewar',
                                 'Npop_prewar', 'geometry', 'Ratio')
      
      ## Multiply cars from 2020 by 2019 pop~car ratio
      dat20b[[i]]$Npop_war <- dat20b[[i]]$Ratio * dat20b[[i]]$Ncars_avg_conflict
      
      ## Get relative change at the grid cell level
      dat20b[[i]]$Relative_change <- ((dat20b[[i]]$Npop_war - dat20b[[i]]$Npop_prewar) / dat20b[[i]]$Npop_prewar)*100
      
      ## Set data frame back to sf object (for plotting)
      dat20b[[i]] <- st_as_sf(dat20b[[i]])
      
      
    }
    
  }
  
  
  ## Conflict year 
  dat22b <- list()
  
  for(i in seq_along(dat22)){
    
    ## First, drop geometry to allow merging
    tmp <- st_drop_geometry(dat22[[i]]) 
    
    ## Merge month-specific 2022 data to pre-war data by grid id
    dat22b[[i]] <- merge(tmp, dat19c, by = c('grid_id', 'City', 'Threshold'))
    
    ## Correct some names for better interpretability
    colnames(dat22b[[i]]) <- c('grid_id', 'City', 'Threshold', 'Year_conflict', 'Month',
                               'Ncars_avg_conflict', 'Ncars_med_conflict','Ncars_sd_conflict',
                               'Npop_conflict', 'Year_prewar', 'Ncars_avg_prewar','Ncars_med_prewar',
                               'Npop_prewar', 'geometry', 'Ratio')
    
    ## Multiply cars from 2022 by 2019 pop~car ratio
    dat22b[[i]]$Npop_war <- dat22b[[i]]$Ratio * dat22b[[i]]$Ncars_avg_conflict
    
    ## Get relative change at the grid cell level
    dat22b[[i]]$Relative_change <- ((dat22b[[i]]$Npop_war - dat22b[[i]]$Npop_prewar) / dat22b[[i]]$Npop_prewar)*100
    
    ## Set data frame back to sf object (for plotting)
    dat22b[[i]] <- st_as_sf(dat22b[[i]])
    
    
  }
  
  
  
  ## Bind data into single data frame
  if(length(dat_list) == 3) dat20c <- do.call(what = sf:::rbind.sf, args=dat20b) # Covid year (2020)
  dat22c <- do.call(what = sf:::rbind.sf, args=dat22b) # Conflict year (2022)
  
  
  ### Bind year-specific data into a single data frame
  #### But first we have to standardize column names
  
  ## Get most relevant columns
  dat19c <- dat19c[,c("grid_id","City","Year","Ncars_avg","Npop","Ratio","geometry")]
  if(length(dat_list) == 3) dat20c <- dat20c[,c("grid_id","City","Year_conflict","Month","Ncars_avg_conflict","Npop_war", "Relative_change","geometry")]
  dat22c <- dat22c[,c("grid_id","City","Year_conflict","Month","Ncars_avg_conflict","Npop_war", "Relative_change","geometry")]
  
  ## Rename columns
  if(length(dat_list) == 3) colnames(dat20c) <- c("grid_id","City","Year","Month", "Ncars_avg","Npop", "Relative_change","geometry")
  colnames(dat22c) <- c("grid_id","City","Year","Month", "Ncars_avg","Npop", "Relative_change","geometry")
  
  
  ## Add dummy column to baseline year (to be able to merge all data into a single df)
  dat19c$Month <- 'Baseline'
  dat19c$Relative_change <- NA
  
  ## Reorganize columns
  dat19c <- dat19c[,c("grid_id","City","Year","Month", "Ncars_avg","Npop", "Relative_change","geometry")]
  
  
  ## Bind all into a single data frame
  if(length(dat_list) == 2){
    datall[[city]]  <- rbind(dat19c, dat22c)
  } else if(length(dat_list) == 3){
    datall[[city]]  <- rbind(dat19c, dat20c, dat22c)
  }
  
  
  ## Save logfile to keep track of cities with their number of years
  track <- data.frame(City = unique(dat$City), 
                      Nyears = nlevels(factor(dat$Year)), 
                      Years = paste(unique(dat$Year), collapse = "; "))
  
  logfile <- rbind(logfile, track) 
  
} #Close big loop


## Rename data list
names(datall) <- cities


## Save outputs for later use
saveRDS(datall, "../../../../Predictions/PopPred_ratio_method.rds")
write.csv(logfile, "../../../../Predictions/logfile_ratio.csv",row.names = F)
