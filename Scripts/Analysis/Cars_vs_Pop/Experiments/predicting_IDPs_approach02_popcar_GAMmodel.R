#######################################################################
#                                                                     #
#      Evaluate relationship between No. people and No. of cars       #
#                                                                     #
#               - Part 2: Predict IDPs from cars -                    #
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

# To do so, a Generalized Additive Model (GAM) will be used, as prior sensitivity 
# analysis indicated a non-linear relationship between the two variables.

# Predictions will be conducted on a monthly temporal resolution, as this was the 
# smallest resolution for which we could retrieve information for matching times 
# (i.e., month-year). Going at daily resolution is nearly impossible, because there 
# is no data available for matching dates across different years (e.g., 10 march in 2019 and 2022).


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
library(INLA)


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
#load("Npop_cars_1000_m.RData")
# popcar <- popcar2; rm('popcar2')

load("Npop_cars_1000_m_updated.RData")



#~~~~~~~~~~~~~~~~~~~~~~
# 3) Reshaping data
#~~~~~~~~~~~~~~~~~~~~~~
# There are only a few cities in which there is data
# for the same month in both baseline (2019) and conflict (2022) years.
# We have to identify those cities, and subsequently evaluate which months are common
# between the two years. In addition, we have to ensure that only matching grid ids
# are selected for a given month of different years, such that the total area
# is preserved.


# 3.1) Get Centroid of polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Aggregate spatial grid list to data frame
# dat <- do.call(what = sf:::rbind.sf, args=popcar_aggr_grid)
# 
# grid_coords <- st_centroid(st_geometry(dat)) %>%
#   st_coordinates() %>%
#   data.frame() %>%
#   mutate(Lon = X, Lat = Y)
# 
# dat$Lon <- grid_coords[,'Lon']
# dat$Lat <- grid_coords[,'Lat']
# 
# 
# ## Append centroid to popcar data frame
# popcar <- merge(popcar, dat[,c('grid_id', 'City', 'Year', 'Month', 'Lon', 'Lat')], by = c('grid_id', 'City', 'Year', 'Month'))
# 
# 



# 3.2) Get city names
#~~~~~~~~~~~~~~~~~~~~~~~~~
YEARS <- c('2019', '2022') #Define baseline year and year of interes


CITY <- unique(popcar$City)
cities <- c()

for(i in seq_along(CITY)){
  cit <- paste(CITY[i])
  
  tmp <- filter(popcar, City == cit & Year %in% YEARS) %>% 
    droplevels() #Filter data for given city
  
  tab <- as.data.frame.matrix(table(tmp$Month, tmp$Year))
  tab[tab>1] <- 1 #transform to binary
  
  #if(isTRUE(any(rowSums(tab) >= 2))){ #Any row with sum >= 2 indicates matching months between both years
  if(isTRUE(any(rowSums(tab) >= 1))){ #Any row with sum >= 1 indicates that data is present for at least one of the months between the two years
    
    
    cities[i] <- cit
  }
  
}

cities <- cities[!is.na(cities)] #Remove NAs




# 3.3) Get common months for city of interest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Correct city name (for plotting purposes)
cities[which(cities == 'Bila_Tserkva')] <- 'Bila-Tserkva' 
cities[which(cities == 'Velyki_Kopani')] <- 'Velyki-Kopani' 


names(popcar_aggr_grid)[which(names(popcar_aggr_grid) == 'Bila_Tserkva')] <- 'Bila-Tserkva' 
names(popcar_aggr_grid)[which(names(popcar_aggr_grid) == 'Velyki_Kopani')] <- 'Velyki-Kopani' 



## Select city of interest
int <- cities[26] #Kiev


## Filter data for given city
dat <- popcar_aggr_grid[[int]] %>%
  filter(Year %in% YEARS)



## Get common months between the two years
dat_list <- split(dat, dat$Year)
comn <- intersect(levels(factor(dat_list[[1]]$Month)),factor(dat_list[[2]]$Month))


## Rework some columns
for(i in seq_along(dat_list)){
  #dat_list[[i]] <- filter(dat_list[[i]], Month %in% comn)
  dat_list[[i]]$Month <- factor(dat_list[[i]]$Month)
  
  ## Round averaged values
  dat_list[[i]]$Ncars_avg <-  round(dat_list[[i]]$Ncars_avg)
  dat_list[[i]]$Npop <-  round(dat_list[[i]]$Npop)
}




# 3.3) Calculate average No. of cars & people per grid-cell for reference year
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The reference year may contain information from multiple months, each of which can cover slightly 
# different protion of the given city's AOI.
# For robustness, for a given city we will calculate the average no. of cars per & people per grid cell
# for the reference year (2019), and use the averaged values to estimate the pop~car relationship from the reference year.
# This will then be used to predict the 'post-war' population.


### First, split 2019/2022 data on month
dat19 <- split(dat_list[[1]], dat_list[[1]]$Month)
dat22 <- split(dat_list[[2]], dat_list[[2]]$Month)


### Keep only matching grid_ids between the different months in 2019
# ids <- Reduce(intersect, lapply(dat19, `[[`, 'grid_id'))
# dat19 <- lapply(dat19, subset, grid_id %in% ids)



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
            Ncars_med = round(median(Ncars_avg, na.rm=T)),
            Npop = round(mean(Npop, na.rm = T)))




### To see the progress
plot(dat19c$Ncars_avg~dat19c$Ncars_med) #To check whether mean or median should be used


library(mapview)
mapview(dat19c, zcol = 'Ncars_avg') +
  mapview(dat19c, zcol = 'Ncars_med') +
  mapview(dat19c, zcol = 'Npop')






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Inferring IDPs from cars
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We will conduct a sensitivty test to evaluate which approach is more robust
# to infer IDPs during the conflict period.
# The approach will increase in level of complexity, each of which considers the following method:

## Approach 1 - Calculate the ratio between No. of cars & People at the grid cell level for the reference year.
## Take this ratio and multiply it for the number of cars for the same grid cell for the war year.
## This approach essentially assumes that there is a linear and constant relationship between the different years.

## Approach 2 - For the reference year, fit a GAM model. The fitted model is then used to predict IDPs during the
## conflict year based on the number of cars observed in that same year. This approach assumes that the pop~car relationship
## is non-linear, and that average trends is constant over grid cell (i.e., analysis considers average trends across all the grid cells).

## Approach 3 - Similar to approach 2, but considering a spatial model. The advante here is that the model will
## take into account grid cell values with similar number of cars, and therefore predictions us the correlation matrix
## to infer the no. of population. Here the identity of the grid cell is indirectly considered, as we assume that the 
## number of cars (hence people) located at closer grid cells will be more similar than grid cells located furher apart.


## Quick visual check (to see whether there are any outliers to be removed)
dat19c %>%
  ggplot(aes(y = Npop, x = Ncars_avg)) +
  geom_point(alpha = 0.5, size = 3, col = 'cyan4') +
  ylab("No. Population") + xlab("No. cars") + ggtitle(int) +
  theme_bw() +
  geom_smooth(method = "gam", col="darkorange", fill = 'bisque',
              #method.args = list(family = "poisson"),
              method.args = list(family = "gaussian"),
              formula = y ~ s(x, bs = "cr", k=5))



# 4.1.1) Fit the model to baseline data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod_gam = gam(Npop ~ s(Ncars_avg, bs = "cr", k = 5),
              family = 'poisson',
              #family = 'gaussian',
              data = dat19c)

# library(splines)
# mod_glm = glm(Npop ~ Ncars_avg,
#               family = 'poisson',
#               #family = 'gaussian',
#               data = dat19c)
# 
# mod_glm2 = glm(Npop ~ ns(Ncars_avg,2),
#               family = 'poisson',
#               #family = 'gaussian',
#               data = dat19c)
# 
# 
# mod_glm3 = glm(Npop ~ ns(Ncars_avg,3),
#                family = 'poisson',
#                #family = 'gaussian',
#                data = dat19c)


summary(mod_gam)
check(getViz(mod_gam),
      a.qq = list(method = "normal", 
                  a.qqpoi = list("shape" = 19,"size"=2, "col" = 'honeydew3', "alpha" = 0.7), 
                  a.ablin = list("linetype" = 2, "colour" = 'gray40')), 
      a.respoi = list(col = 'honeydew3', size = 2, alpha =0.7), 
      a.hist = list(bins = 10, fill = 'honeydew3',col = 'honeydew4', alpha = 0.7))



# 4.1.2) Predict 2022 population
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Retrieve car values for 2022
warcar <- list()
for(i in seq_along(dat22)){
  warcar[[i]] = data.frame(Ncars_avg = dat22[[i]]$Ncars_avg)
}

## Now predict
warpop <- list()
for(i in seq_along(dat22)){
  warpop[[i]] <- predict.gam(mod_gam,
                         newdata = warcar[[i]],
                         type = 'response',
                         se = TRUE)
}


# 4.1.3) Let's plot the output
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inspired to some extent by:
# https://stackoverflow.com/questions/46824998/adding-percent-change-between-bars-on-a-ggplot


df_preds <- list()
dat22b <- list()

for(i in seq_along(dat22)){
  df_preds[[i]] <- data.frame(warcar[[i]], warpop[[i]]) %>%
                              mutate(lower = fit - 1.96 * se.fit,
                                     upper = fit + 1.96 * se.fit)
    df_preds[[i]]$Month <- paste(unique(dat22[[i]]$Month))
    
    ## Append information to original data frame
    dat22b[[i]] <- cbind(dat22[[i]], df_preds[[i]])
    
    ## Calculate realtive change
    dat22b[[i]]$Relative_change <- ((dat22b[[i]]$fit - dat22b[[i]]$Npop) / dat22b[[i]]$Npop)*100
}


df_preds <- do.call('rbind', df_preds) ## Bind list into data frame
dat22b <- do.call(what = sf:::rbind.sf, args=dat22b)## Bind list into data frame


## Visualize the progress
palfunc <- function (n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
  colors <- RColorBrewer::brewer.pal(11, "RdBu")
  if (direction < 0) colors <- rev(colors)
  colorRampPalette(colors, alpha = alpha)(n)
}

dat19c %>% 
  mapview(zcol = 'Npop', at = seq(0, 11000, 1000)) #summary(dat19c$Npop)

dat22b %>% 
  filter(Month == 'jun') %>%
  mapview(zcol = 'Relative_change', col.regions = palfunc, at = seq(-100, 200, 20))
  #mapview(zcol = 'fit', at = seq(0, 11000, 1000))



### Plot with dynamic baseline (i.e., we don't care about matching grid_ids across 2022 months)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Reshape data for some plotting 
war <- dat22b[,c('grid_id', 'City','Year','Month','Ncars_avg','fit')]; war$grid_id <- factor(war$grid_id)



## Calculate total population before/during war - Note, during war images don't always cover the full
## AOI extent, and thereore the baseline population will differ. A workaround solution to this is to 
## Get only matching grid cells across the differnt months (of 2022), but here we rist at loosing considerable
## information if one month occupies only a smaller fraction of the city.

war2 <- split(war, war$Month) #Split by month

war_list <- list()
prewar_list <- list()


for(i in seq_along(war2)){
  war2[[i]]$grid_id <- factor(war2[[i]]$grid_id)
  
  ## Get matching grid id
  tmp <- intersect(war2[[i]]$grid_id, dat19c$grid_id)
  
  ## Filter for matching grid_id - war period
  war2[[i]] <- filter(war2[[i]], grid_id %in% tmp)
  
  prewar <- filter(dat19c, grid_id %in% tmp)
  
  war_list[[i]] <- war2[[i]] %>%
    st_drop_geometry() %>%
    summarize(Totpop = round(sum(fit)),
              Year = paste(unique(Year)),
              Month = paste(unique(Month)))
  
  prewar_list[[i]] <- data.frame(Totpop = sum(prewar$Npop))
  prewar_list[[i]]$Year <- '2019'
  prewar_list[[i]]$Month <- paste(unique(war2[[i]]$Month))
  
}


warpop <- do.call('rbind', war_list)
baselinepop <- do.call('rbind', prewar_list)

dat <- rbind(baselinepop, warpop)
dat$Month <- stringr::str_to_title(dat$Month); dat$Month <- as.factor(dat$Month)

## Rearrange months by chronological order
dat$Month <- factor(dat$Month, levels=c('Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))



### Now go for the plot
dat %>%
  group_by(Month, Year) %>%
  arrange(Month = factor(Month, levels=c('Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
  as_tibble() %>%
  group_by(Month) %>%
  mutate(End = lag(Totpop),
         xpos = 1:n()-0.5,
         Diff = Totpop - Totpop[1],
         Percent = paste(round(Diff/End*100,1),"%")) %>%
  ggplot(aes(x=Month, y=Totpop, fill = Year, col = Year, label = Percent)) +
  geom_bar(stat = "identity",  
           width = 0.8,
           position=position_dodge(width = 0.9),
           alpha = 0.5,
           lwd = 0.8) +
  scale_fill_manual(values=c("cyan4","darkorange")) +
  scale_color_manual(values=c("cyan4", "darkorange")) +
  # geom_text(position = position_dodge(width = .9),
  #           vjust = -0.5)
  geom_text(aes(x = Month, y =  (End+Diff), label = Percent), 
            position = position_dodge(width = .9),
            vjust = -0.5, fontface = 2,
            col = 'gray30') +
  theme_bw() +
  scale_y_continuous(labels = comma) +
  ylab('No. people') + xlab('') +
  theme(axis.text.x = element_text(size = 15, face = 'bold'),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 17, face = "bold", vjust = 5),
        panel.border = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = 'bold'),
        plot.margin = unit(c(t=1,b=0,r=0,l=1), "cm"))

