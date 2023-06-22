#######################################################################
#                                                                     #
#      Evaluate relationship between No. people and No. of cars       #
#                                                                     #
#           - Part 2b: Predict IDPs from cars: GAM method -           #
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


## The current script applies the GAM method.


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
setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GitHub/IDP_UKR/") 


## Helper functions
source("Scripts/src/helper_functions.R") #Will be used to save GAM output as table



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
p1 <- list()

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
              Ncars_avg = round(mean(Ncars_avg, na.rm=T)),
              Ncars_med = median(Ncars_avg, na.rm=T),
              Npop = round(mean(Npop, na.rm = T)))
  
  ### To see the progress
  # plot(dat19c$Ncars_avg~dat19c$Ncars_med) #To check whether mean or median should be used
  # 
  # mapview(dat19c, zcol = 'Ncars_avg') +
  #   #mapview(dat19c, zcol = 'Ncars_med') +
  #   mapview(dat19c, zcol = 'Npop')

  
  ## Get pop/car ratio (will be used for plotting)
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
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4) Inferring IDPs from cars
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Approach B - The rationale behind this approach is to infer the car~population relationship 
  # from the baseline year (2019), and use the estimated parameters to predict
  # population during the conflict year (2022) from cars observed in the same year.
  
  # To do so, a Generalized Additive Model (GAM) will be used, as prior sensitivity 
  # analysis indicated a non-linear relationship between the two variables.
  
  # Predictions will be conducted on a monthly temporal resolution, as this was the 
  # smallest resolution for which we could retrieve information for matching times 
  # (i.e., month-year). Going at daily resolution is nearly impossible, because there 
  # is no data available for matching dates across different years (e.g., 10 march in 2019 and 2022).
  
  
  
  # 4.1) Fit a GAM for the pop~car relationship in the reference year
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(int == 'Berehove'){
    dat19c <- dat19c %>% filter(!(Npop < 250 & Ncars_avg >= 5)) #Removes only 1 data point
  } else if(int == "Bila-Tserkva"){
    dat19c <- dat19c %>% filter(!(Npop < 2000 & Ncars_avg >= 100)) #Removes only 2 data points
  } else if(int == "Chernivtsi"){
    dat19c <- dat19c %>% filter(Ncars_avg < 300) #Removes only 2 data points
  } else if(int == "Ivano-Frankivsk"){
    dat19c <- dat19c %>% filter(!(Npop < 2500 & Ncars_avg > 50)) #Removes 2 data points 
  } else if(int == "Konotop"){
    dat19c <- dat19c %>% filter(Ncars_avg < 50) %>% filter(!(Npop < 2000 & Ncars_avg > 25)) #Removes 3 data points
  } else if(int == "Kremenchuk"){
    dat19c <- dat19c %>% filter(!(Npop < 500 & Ncars_avg > 50)) #Removes 13 data points
  } else if(int == "Kropyvnytskyi"){
    dat19c <- dat19c %>% filter(Ncars_avg < 350) %>% filter(!(Npop < 1000 & Ncars_avg > 100)) #Removes 3 data points
  } else if(int == 'Mariupol'){
    dat19c <- dat19c %>% filter(!(Npop < 2000 & Ncars_avg > 100)) #Removes 4 data points
  } else if(int == "Melitopol"){
    dat19c <- dat19c %>% filter(!(Npop < 1000 & Ncars_avg > 30)) #Removes 8 data points
  } else if(int == 'Odessa'){
    dat19c <- dat19c %>% filter(Ncars_avg < 1500) #Removes 1 data point
  } else if(int == 'Reni'){
    dat19c <- dat19c %>% filter(Ncars_avg < 11) #Removes 1 data point
  } else if(int == "Ternopil"){
    dat19c <- dat19c %>% filter(!(Npop < 2000 & Ncars_avg > 150)) #Removes 3 data points
  } else if(int == "Uzhhorod"){
    dat19c <- dat19c %>% filter(!(Npop < 2000 & Ncars_avg > 150)) #Removes 3 data points
  } else if(int == "Vinnytsia"){
    dat19c <- dat19c %>% filter(!(Npop < 4500 & Ncars_avg > 250)) #Removes 6 data points
  }
  
  
  ## Quick visual check (to see whether there are any outliers to be removed)
  # dat19c %>%
  #   #filter(Ncars_avg < 1000) %>%
  #   #filter(!(Npop < 2000 & Ncars_avg > 100 )) %>%
  #   ggplot(aes(y = Npop, x = Ncars_avg)) +
  #   geom_point(aes(size= Ratio), alpha = 0.5, col = 'cyan4') +
  #   ylab("No. Population") + xlab("No. cars") + ggtitle(int) +
  #   scale_size(range = c(1,7),
  #              #breaks = c(range(dat19c$Ratio)[1], median(dat19c$Ratio), range(dat19c$Ratio)[2])) +
  #              breaks = c(round(seq(range(dat19c$Ratio)[1], range(dat19c$Ratio)[2], length.out = 5)))) +
  #              #breaks = c( cartography::getBreaks(dat19c$Ratio, mehtod = 'quantile', nclass = 4))) +
  #              #breaks = pretty(seq(min(dat19c$Ratio)+100, max(dat19c$Ratio), by =  diff( c(min(dat19c$Ratio),  max(dat19c$Ratio)))/5))) +
  #              #breaks = c(as.vector(quantile(dat19c$Ratio, probs = seq(0, 1, by = 0.2))))) +
  #   theme_bw() +
  #   geom_smooth(method = "gam", col="darkorange", fill = 'bisque',
  #               #method.args = list(family = "poisson"),
  #               method.args = list(family = "gaussian"),
  #               formula = y ~ s(x, bs = "cr", k=ifelse(int == "Mamalyha", 4,  #Reduce no. of knots for some of the cities
  #                                                      ifelse(int == "Shehyni", 3, 5))))

  ## Fit the model
  mod_gam = gam(Npop ~ s(Ncars_avg, bs = "cr", k = ifelse(int == "Mamalyha", 4,  #Reduce no. of knots for some of the cities
                                                   ifelse(int == "Shehyni", 3, 5))),
                family = 'poisson',
                #family = 'gaussian',
                data = dat19c)
  

  
  
  # 4.2) Predict covid & war population
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## 4.2.1) Covid population
  if(!(is.null(dat20))){
    
    ### Retrieve car values for 2020
    covidcar <- list()
    for(i in seq_along(dat20)){
      covidcar[[i]] = data.frame(Ncars_avg = dat20[[i]]$Ncars_avg)
    }
    
    ### Now predict
    covidpop <- list()
    for(i in seq_along(dat20)){
      covidpop[[i]] <- predict.gam(mod_gam,
                                 newdata = covidcar[[i]],
                                 type = 'response',
                                 se = TRUE)
    }
    
    
  } 
  
  ## 4.2.2) War population
  ### Retrieve car values for 2022
  warcar <- list()
  for(i in seq_along(dat22)){
    warcar[[i]] = data.frame(Ncars_avg = dat22[[i]]$Ncars_avg)
  }
  
  ### Now predict
  warpop <- list()
  for(i in seq_along(dat22)){
    warpop[[i]] <- predict.gam(mod_gam,
                                 newdata = warcar[[i]],
                                 type = 'response',
                                 se = TRUE)
  
  }
  
  
  # 4.3) Reshape the predicted data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Covid data
  if(!(is.null(dat20))){
    
    df_covid <- list()
    dat20b <- list()
    
    for(i in seq_along(dat20)){
      df_covid[[i]] <- data.frame(covidcar[[i]], covidpop[[i]]) %>%
        mutate(lower = fit - 1.96 * se.fit,
               upper = fit + 1.96 * se.fit)
      df_covid[[i]]$Month <- paste(unique(dat20[[i]]$Month))
      
      ## Append information to original data frame
      dat20b[[i]] <- cbind(dat20[[i]], df_covid[[i]])
      
      ## Calculate realtive change
      dat20b[[i]]$Relative_change <- ((dat20b[[i]]$fit - dat20b[[i]]$Npop) / dat20b[[i]]$Npop)*100
    }
    
    
    df_covid <- do.call('rbind', df_covid) ## Bind list into data frame
    dat20b <- do.call(what = sf:::rbind.sf, args=dat20b)## Bind list into data frame
    
    ## Reorganize columns & drop unused columns
    dat20b <- dat20b[,c("grid_id", "City", "Year", "Month", "Ncars_avg", "fit", "Relative_change", "geometry")]
    
    ## Rename columns
    colnames(dat20b) <- c("grid_id","City","Year","Month", "Ncars_avg","Npop", "Relative_change","geometry")
    
  }
  
  

  ## War data
  df_war <- list()
  dat22b <- list()
    
    for(i in seq_along(dat22)){
      df_war[[i]] <- data.frame(warcar[[i]], warpop[[i]]) %>%
        mutate(lower = fit - 1.96 * se.fit,
               upper = fit + 1.96 * se.fit)
      df_war[[i]]$Month <- paste(unique(dat22[[i]]$Month))
      
      ## Append information to original data frame
      dat22b[[i]] <- cbind(dat22[[i]], df_war[[i]])
      
      ## Calculate realtive change
      dat22b[[i]]$Relative_change <- ((dat22b[[i]]$fit - dat22b[[i]]$Npop) / dat22b[[i]]$Npop)*100
    }
    
    
    df_war <- do.call('rbind', df_war) ## Bind list into data frame
    dat22b <- do.call(what = sf:::rbind.sf, args=dat22b)## Bind list into data frame
    
    ## Reorganize columns & drop unused columns
    dat22b <- dat22b[,c("grid_id", "City", "Year", "Month", "Ncars_avg", "fit", "Relative_change", "geometry")]
    
    ## Rename columns
    colnames(dat22b) <- c("grid_id","City","Year","Month", "Ncars_avg","Npop", "Relative_change","geometry")
    
    
  
    ## Visualize the progress
    # palfunc <- function (n, alpha = 1, begin = 0, end = 1, direction = 1) 
    # {
    #   colors <- RColorBrewer::brewer.pal(11, "RdBu")
    #   if (direction < 0) colors <- rev(colors)
    #   colorRampPalette(colors, alpha = alpha)(n)
    # }
    # 
    # dat19c %>% 
    #   mapview(zcol = 'Npop', at = seq(0, 5000, 500)) #summary(dat19c$Npop)
    # 
    # mapview(dat19c, alpha.region = 0, lwd=2) +
    # dat22b %>% 
    #   filter(Month == 'jun') %>%
    #   mapview(zcol = 'Relative_change', col.regions = palfunc, at = seq(-100, 200, 20))

    
    
    
    # 4.4) Bind all data into a single data frame
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ### I.e., Bind year-specific data into a single data frame
    #### But first we have to standardize column names
    
    ## Get most relevant columns
    dat19c <- dat19c[,c("grid_id","City","Year","Ncars_avg","Npop","Ratio","geometry")]
    
    ## Add dummy column to baseline year (to be able to merge all data into a single df)
    dat19c$Month <- 'Baseline'
    dat19c$Relative_change <- NA
    
    ## Reorganize columns
    dat19c <- dat19c[,c("grid_id","City","Year","Month", "Ncars_avg","Npop", "Relative_change","geometry", "Ratio")]
    
    
    ## Bind all into a single data frame
    if(length(dat_list) == 2){
      datall[[city]]  <- rbind(dat19c[,-ncol(dat19c)], dat22b)
    } else if(length(dat_list) == 3){
      datall[[city]]  <- rbind(dat19c[,-ncol(dat19c)], dat20b, dat22b)
    }
  
  
    ## Save logfile to keep track of cities with their number of years
    track <- data.frame(City = unique(dat$City), 
                        Nyears = nlevels(factor(dat$Year)), 
                        Years = paste(unique(dat$Year), collapse = "; "))
    
    logfile <- rbind(logfile, track) 
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 5) Save model and figure outputs
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # 5.1) Save the pop~car raw plots
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(int == "Mamalyha"){
      p1[[city]] <- dat19c %>%
        ggplot(aes(y = Npop, x = Ncars_avg)) +
        geom_smooth(method = "gam", col="darkorange", fill = 'bisque',
                    #method.args = list(family = "poisson"),
                    method.args = list(family = "gaussian"),
                    formula = y ~ s(x, bs = "cr", k =  4)) +  #Reduce no. of knots for some of the cities
                                                             
        # geom_smooth(method = "gam", col="darkorange", fill = 'bisque',
        #             #method.args = list(family = "poisson"),
        #             method.args = list(family = "gaussian"),
        #             formula = y ~ s(x, bs = "cr", k =   ifelse(int == "Mamalyha", 4,  #Reduce no. of knots for some of the cities
        #                                                        ifelse(int == "Shehyni", 3, 5)))) +
        geom_point(aes(size= Ratio), alpha = 0.5, col = 'cyan4') +
        scale_size(range = c(3,10),
                   breaks = c(round(seq(range(dat19c$Ratio)[1], range(dat19c$Ratio)[2], length.out = 5)))) +
        #breaks = c(range(dat19c$Ratio)[1], median(dat19c$Ratio), range(dat19c$Ratio)[2])) +
        #breaks = c( cartography::getBreaks(dat19c$Ratio, mehtod = 'quantile', nclass = 4))) +
        #breaks = pretty(seq(min(dat19c$Ratio)+100, max(dat19c$Ratio), by =  diff( c(min(dat19c$Ratio),  max(dat19c$Ratio)))/5))) +
        #breaks = c(as.vector(quantile(dat19c$Ratio, probs = seq(0, 1, by = 0.2))))) +
        ylab("No. People") + xlab("No. Cars") +
        labs(size="Pop/Car Ratio") +
        facet_wrap(City ~.,) + 
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = comma) +
        theme_bw() +
        theme(axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.title = element_text(size = 22, face = 'bold'),
              legend.title = element_text(hjust = 0.5, size=20, face="bold"),
              legend.text = element_text(size = 20),
              strip.background =element_rect(fill="gray98"),
              strip.text = element_text(colour = 'gray40', size = 24, face ='bold'),
              legend.position = "right")
    } else if(int == "Shehyni") {
      p1[[city]] <- dat19c %>%
        ggplot(aes(y = Npop, x = Ncars_avg)) +
        geom_smooth(method = "gam", col="darkorange", fill = 'bisque',
                    #method.args = list(family = "poisson"),
                    method.args = list(family = "gaussian"),
                    formula = y ~ s(x, bs = "cr", k =  3)) +  #Reduce no. of knots for some of the cities
        
        # geom_smooth(method = "gam", col="darkorange", fill = 'bisque',
        #             #method.args = list(family = "poisson"),
        #             method.args = list(family = "gaussian"),
        #             formula = y ~ s(x, bs = "cr", k =   ifelse(int == "Mamalyha", 4,  #Reduce no. of knots for some of the cities
        #                                                        ifelse(int == "Shehyni", 3, 5)))) +
        geom_point(aes(size= Ratio), alpha = 0.5, col = 'cyan4') +
        scale_size(range = c(3,10),
                   breaks = c(round(seq(range(dat19c$Ratio)[1], range(dat19c$Ratio)[2], length.out = 5)))) +
        #breaks = c(range(dat19c$Ratio)[1], median(dat19c$Ratio), range(dat19c$Ratio)[2])) +
        #breaks = c( cartography::getBreaks(dat19c$Ratio, mehtod = 'quantile', nclass = 4))) +
        #breaks = pretty(seq(min(dat19c$Ratio)+100, max(dat19c$Ratio), by =  diff( c(min(dat19c$Ratio),  max(dat19c$Ratio)))/5))) +
        #breaks = c(as.vector(quantile(dat19c$Ratio, probs = seq(0, 1, by = 0.2))))) +
        ylab("No. People") + xlab("No. Cars") +
        labs(size="Pop/Car Ratio") +
        facet_wrap(City ~.,) + 
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = comma) +
        theme_bw() +
        theme(axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.title = element_text(size = 22, face = 'bold'),
              legend.title = element_text(hjust = 0.5, size=20, face="bold"),
              legend.text = element_text(size = 20),
              strip.background =element_rect(fill="gray98"),
              strip.text = element_text(colour = 'gray40', size = 24, face ='bold'),
              legend.position = "right")
    } else{
      p1[[city]] <- dat19c %>%
        ggplot(aes(y = Npop, x = Ncars_avg)) +
        geom_smooth(method = "gam", col="darkorange", fill = 'bisque',
                    #method.args = list(family = "poisson"),
                    method.args = list(family = "gaussian"),
                    formula = y ~ s(x, bs = "cr", k =  5)) +  #Reduce no. of knots for some of the cities
        
        # geom_smooth(method = "gam", col="darkorange", fill = 'bisque',
        #             #method.args = list(family = "poisson"),
        #             method.args = list(family = "gaussian"),
        #             formula = y ~ s(x, bs = "cr", k =   ifelse(int == "Mamalyha", 4,  #Reduce no. of knots for some of the cities
        #                                                        ifelse(int == "Shehyni", 3, 5)))) +
        geom_point(aes(size= Ratio), alpha = 0.5, col = 'cyan4') +
        scale_size(range = c(3,10),
                   breaks = c(round(seq(range(dat19c$Ratio)[1], range(dat19c$Ratio)[2], length.out = 5)))) +
        #breaks = c(range(dat19c$Ratio)[1], median(dat19c$Ratio), range(dat19c$Ratio)[2])) +
        #breaks = c( cartography::getBreaks(dat19c$Ratio, mehtod = 'quantile', nclass = 4))) +
        #breaks = pretty(seq(min(dat19c$Ratio)+100, max(dat19c$Ratio), by =  diff( c(min(dat19c$Ratio),  max(dat19c$Ratio)))/5))) +
        #breaks = c(as.vector(quantile(dat19c$Ratio, probs = seq(0, 1, by = 0.2))))) +
        ylab("No. People") + xlab("No. Cars") +
        labs(size="Pop/Car Ratio") +
        facet_wrap(City ~.,) + 
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = comma) +
        theme_bw() +
        theme(axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.title = element_text(size = 22, face = 'bold'),
              legend.title = element_text(hjust = 0.5, size=20, face="bold"),
              legend.text = element_text(size = 20),
              strip.background =element_rect(fill="gray98"),
              strip.text = element_text(colour = 'gray40', size = 24, face ='bold'),
              legend.position = "right")
    }
    
    
    
    
      
      ## Set main directory 
      DIR <- file.path("/Users","marie-christinerufener", "OneDrive - Hamad bin Khalifa University",
                       "Projects", "Ukraine", "Manuscript", "Figures", "IDP", "PopCar", int)
      
      ## Create city-specific directory
      if(isFALSE(dir.exists(DIR))){
        dir.create(DIR)
      }
      
      ## Set output file name
      OUTFILE <- paste(paste(DIR, "/PopCar_relationship.jpg", sep = ""))

      
      ## Nos save the output
      jpeg(OUTFILE,
           bg = "white", res = 300, 
           width = 25, height = 15, units = 'cm')
      print(p1[[city]])
      dev.off()
      
      
      # 5.2) Save the model's visual and numerical assessment
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Generate the plot
      p2 <- check(getViz(mod_gam),
                  a.qq = list(method = "normal", 
                              a.qqpoi = list("shape" = 19,"size"=2, "col" = 'honeydew3', "alpha" = 0.7), 
                              a.ablin = list("linetype" = 2, "colour" = 'gray40')), 
                              a.respoi = list(col = 'honeydew3', size = 2, alpha =0.7), 
                              a.hist = list(bins = 10, fill = 'honeydew3',col = 'honeydew4', alpha = 0.7))
      
    
      ## Set main directory 
      DIR <- file.path("/Users","marie-christinerufener", "OneDrive - Hamad bin Khalifa University",
                       "Projects", "Ukraine", "Manuscript", "Figures", "IDP", "ModelFit", int)
      
      ## Create city-specific directory
      if(isFALSE(dir.exists(DIR))){
        dir.create(DIR)
      }
      
      ## Set output file name
      OUTFILE <- paste(paste(DIR, "/Model_fit.jpg", sep = ""))
      
      
      ## Nos save the output
      jpeg(OUTFILE,
           bg = "white", res = 300, 
           width = 20, height = 15, units = 'cm')
      print(p2)
      dev.off()
      
      
      
      ## And now the numerical output
      ## And finally, the numerical output
      OUTFILE2 <- paste(paste(DIR, paste("Model_NumSummary", int, sep = "_"), sep ="/"), ".csv",sep = "")

      gamOut(res = summary(mod_gam), 
             file = OUTFILE2,
             writecsv = T)
      
    
} # Close big forloop



## Rename data list
names(datall) <- cities


## Save outputs for later use
saveRDS(datall, "../../../../Predictions/PopPred_gam_method.rds")
write.csv(logfile, "../../../../Predictions/logfile_gam.csv",row.names = F)


#~~~~~~~~~~~~~
# FINAL PLOTS
#~~~~~~~~~~~~~


## First 10 plots
ggarrange(p1[[1]], p1[[2]],
          p1[[3]], p1[[4]],
          p1[[5]], p1[[6]],
          p1[[7]], p1[[8]],
          p1[[9]], p1[[10]],
          ncol = 2, nrow = 5)

ggsave("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/IDP/popcar_S1.jpg",
       dpi = 300,
       width = 45,
       height = 65,
       units = "cm")


##  11-20 plots
ggarrange(p1[[11]], p1[[12]],
          p1[[13]], p1[[14]],
          p1[[15]], p1[[16]],
          p1[[17]], p1[[18]],
          p1[[19]], p1[[20]],
          ncol = 2, nrow = 5)

ggsave("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/IDP/popcar_S2.jpg",
       dpi = 300,
       width = 45,
       height = 65,
       units = "cm")



##  21-30 plots
ggarrange(p1[[21]], p1[[22]],
          p1[[23]], p1[[24]],
          p1[[25]], p1[[26]],
          p1[[27]], p1[[28]],
          p1[[29]], p1[[30]],
          ncol = 2, nrow = 5)

ggsave("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/IDP/popcar_S3.jpg",
       dpi = 300,
       width = 45,
       height = 65,
       units = "cm")



##  31-40 plots
ggarrange(p1[[31]], p1[[32]],
          p1[[33]], p1[[34]],
          p1[[35]], p1[[36]],
          p1[[37]], p1[[38]],
          p1[[39]], p1[[40]],
          ncol = 2, nrow = 5)

ggsave("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/IDP/popcar_S4.jpg",
       dpi = 300,
       width = 45,
       height = 65,
       units = "cm")


##  41-43 plots
ggarrange(p1[[41]], p1[[42]],
          p1[[43]],
          ncol = 2, nrow = 5)

ggsave("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/IDP/popcar_S5.jpg",
       dpi = 300,
       width = 45,
       height = 65,
       units = "cm")


