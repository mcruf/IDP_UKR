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
# verify that the population decrease/increase in war period are true .


YEARS <- c('2019','2022') #Define baseline year and years of interest


# 3.1) Get city names
#~~~~~~~~~~~~~~~~~~~~~~~~~
CITY <- unique(popcar$City)
cities <- c()

for(i in seq_along(CITY)){
  cit <- paste(CITY[i])
  
  tmp <- filter(popcar, City == cit & Year %in% YEARS) %>% 
    droplevels() #Filter data for given city
  
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
int <- cities[26] #Kiev


## Filter data for given city
dat <- popcar_aggr_grid[[int]] %>%
  filter(Year %in% YEARS)


## Get common months between baseline and year(s) of interest
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





# 4.1) Approach 1 - Proportional changes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Calculate the pop~car ratio for the reference year
dat19c$Ratio <- dat19c$Npop / dat19c$Ncars_avg


## Look-up for matching grid cells between reference and conflict year
for(i in seq_along(dat22)){
  tmp <- intersect(dat19c$grid_id, dat22[[i]]$grid_id) #Get matching grid cells
  dat22[[i]] <- filter(dat22[[i]], grid_id %in% tmp) #Filter for matching grid cells in reference year
  dat22[[i]]$grid_id <- as.factor(dat22[[i]]$grid_id) #Drop-out empty factor levels 
}




## Infer no. of people from baseline car~pop ratio
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


## Visualize the progress
mapview(dat22b[[1]], zcol = 'Npop_prewar') +
  mapview(dat22b[[1]], zcol = 'Npop_war') +
  mapview(dat22b[[1]], zcol = 'Relative_change') 
  

## Bind data into single data frame
dat22c <- do.call(what = sf:::rbind.sf, args=dat22b)


## Remove grid cells whose Ratio could not be caculated (i.e., Ration = Inf. - happens when a grid cell doesn't have any detected cars)
rmv <- which(!(is.finite(dat22c$Ratio)))
dat22c <- dat22c[-rmv,]



# brks <- cartography::getBreaks(dat22c$Relative_change, method = "quantile", nclass = 5) # When on a yearly basis
# 
# dat22c$change_grouped <- cut(dat22c$Relative_change, brks,
#                                    include.lowest = TRUE)

#colors <- RColorBrewer::brewer.pal(11, "RdBu")

#zCuts <- seq(min(test$Relative_change), max(test$re), length.out = 20)



palfunc <- function (n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
  colors <- RColorBrewer::brewer.pal(11, "RdBu")
  if (direction < 0) colors <- rev(colors)
  colorRampPalette(colors, alpha = alpha)(n)
}


dat22c %>% 
  filter(Month == 'mar') %>%
  # mutate(change_grouped = cut(Relative_change,
  #                             breaks = seq(min(Relative_change), max(Relative_change), by = 50), include.lowest = TRUE)) %>%
  # #mapview(zcol = 'Npop_prewar') 
  #mapview(zcol = 'Npop_war') 
  mapview(zcol = 'Relative_change', col.regions = palfunc,at = seq(-100, 200, 20))




### Plot with dynamic baseline (i.e., we don't care about matching grid_ids across 2022 months)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Reshape data for some plotting 
#prewar <- dat19c[,c('grid_id', 'City','Year','Ncars_avg','Npop')]; prewar$grid_id <- factor(prewar$grid_id)
war <- dat22c[,c('grid_id', 'City','Year_conflict','Month','Ncars_avg_conflict','Npop_war')]; war$grid_id <- factor(war$grid_id)

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
                   summarize(Totpop = round(sum(Npop_war)),
                             Year = paste(unique(Year_conflict)),
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




### Plot with static baseline (i.e., we keep only matching grid id for 2022 months)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## First, keep only matching grid IDs across 2022 months
war <- dat22c %>%
       dplyr::select(c('grid_id', 'City','Year_conflict','Month','Ncars_avg_conflict','Npop_war', 'Relative_change')) %>%
       group_by( grid_id) %>%
       mutate(duplicate_count = n()) %>% 
       filter(duplicate_count == nlevels(factor(dat22c$Month))) 
       #%>% distinct(grid_id, .keep_all = TRUE) 

#war %>% filter(Month == 'feb') %>% mapview(zcol = 'Relative_change')



## Calculate total population before/during war 

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
    group_by(Month) %>%
    summarize(Totpop = round(sum(Npop_war)),
              Year = paste(unique(Year_conflict)),
              Month = paste(unique(Month)))
  
  prewar_list[[i]] <- data.frame(Totpop = sum(prewar$Npop))
  prewar_list[[i]]$Year <- '2019'
  prewar_list[[i]]$Month <- 'baseline'
  
}

warpop <- do.call('rbind', war_list)
baselinepop <- do.call('rbind', prewar_list)

dat <- rbind(baselinepop[1,], warpop)
dat$Month <- stringr::str_to_title(dat$Month); dat$Month <- as.factor(dat$Month)

## Rearrange months by chronological order
dat$Month <- factor(dat$Month, levels=c('Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))

dat$Contrast <- ifelse(dat$Month == 'Baseline', 'Pre-war', 'War')



dat %>%
  group_by(Month, Contrast) %>%
  mutate(Month = replace(as.character(Month), Contrast=='Pre-war', "Baseline")) %>%
  arrange(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
  mutate(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
  
  as_tibble() %>%
  mutate(
    Diff = Totpop - Totpop[1],
    Percent = round(Diff/Totpop[1]*100,3),
    Direction = ifelse(Percent >0, 'Increase', 'Decrease'))  %>% 
  mutate(Direction = ifelse(Percent == 0, NA_integer_, Direction)) %>%
  mutate_at(c('Percent'), ~na_if(., 0)) %>%
  

  ggplot(aes(x=interaction(Month, Contrast), y=Totpop, fill = Contrast, col = Contrast)) +
  
  geom_bar(stat="identity",
           position=position_dodge(width = 0.9), 
           alpha = 0.5,
           lwd = 1) +
  
  scale_x_discrete(guide = "axis_nested") +
  
  
  geom_text(aes(label=Percent, col = Direction),
            position = position_dodge(width = 1),
            vjust = -0.25,
            size = 12, fontface = 'bold') +
  #col = 'gray40') +
  
  scale_fill_manual(name = "Contrast", values=c("cyan4","darkorange")) +
  
  #scale_color_manual(name = "Contrast", values=c('#B2182B','#2166AC', "cyan4", "darkorange")) + #Increase & Decrease
  #scale_color_manual(name = "Contrast", values=c('#2166AC', "cyan4", "darkorange")) + #Increase
  scale_color_manual(name = "Contrast", values=c('#B2182B', "cyan4", "darkorange")) + #Decrease
  
  
  #scale_color_manual(name = 'Direction', values = c('#B2182B','#2166AC')) +
  scale_y_continuous(labels = comma) +
  ylab('No. people') + xlab('') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 38, face = 'bold'),
        axis.text.y = element_text(size = 31),
        axis.title.y = element_text(size = 35, face = "bold", vjust = 5),
        panel.border = element_blank(),
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
        legend.position = "none",
        plot.margin = unit(c(t=1,b=0,r=0,l=2), "cm"),
        panel.spacing = unit(0, units = "cm"), # removes space between panels
        strip.placement = "outside", # moves the states down
        strip.background = element_rect(fill = "white"),
        ggh4x.axis.nestline.x = element_line(linewidth = 0.5),
        
        ggh4x.axis.nesttext.x = element_text(colour = "gray40", angle = 360, 
                                             hjust = 0.5, 
                                             face ='bold',
                                             margin = unit(c(3, 0, 0, 0), "mm")))


# dat %>%
#   group_by(Month, Contrast) %>%
#   mutate(Month = replace(as.character(Month), Contrast=='Pre-war', "Baseline")) %>%
#   arrange(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
#   as_tibble() %>%
#   mutate(End = Totpop[1],
#          xpos = 1:n()-0.5,
#          Diff = Totpop - Totpop[1],
#          Percent = paste(round(Diff/End*100,1),"%")) %>%
#   ggplot(aes(x=Contrast, y=Totpop, fill = Contrast, col = Contrast)) +
#   geom_bar(stat="identity",
#            position=position_dodge2(preserve = "single"),
#            alpha = 0.5,
#            lwd = 1,
#            width=1)  +
#   
#   geom_text(aes(Contrast, label=Month),
#             position = position_dodge2(width = 1),
#             vjust=-0.8, size = 12, fontface = 'bold',
#             col = 'gray40') +
#   
#   
#   # geom_text(aes(x = xpos, y =  (End+Diff/2), label = Percent, 
#   #               hjust = ifelse(Diff[-1] < 0 , -0.1, 1.2), 
#   #               fontface = 2), size = 15, col = 'gray30') +
#   
#   scale_fill_manual(name = "Contrast", values=c("cyan4","darkorange")) +
#   scale_color_manual(name = "Contrast", values=c("cyan4", "darkorange")) +
#   scale_y_continuous(labels = comma) +
#   ylab('No. people') + xlab('') + ggtitle(int) +
#   theme_bw()

