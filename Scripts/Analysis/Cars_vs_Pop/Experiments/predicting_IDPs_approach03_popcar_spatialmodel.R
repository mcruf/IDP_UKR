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




## Get centroid of polygon grid (will be used later for the spatial model)

### First, transform data to UTM projection (to get units in meters)
dat <- dat %>%
  st_transform(crs = 6381)

coords <- dat %>%
  st_centroid(st_geometry(.)) %>%
  st_coordinates() %>%
  data.frame() 

dat[,c('X', 'Y')] <- coords




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
            Npop = mean(Npop, na.rm = T),
            X = paste(unique(X)),
            Y = paste(unique(Y)))

dat19c$X <- as.numeric(as.character(dat19c$X)) #Convert character to numeric
dat19c$Y <- as.numeric(as.character(dat19c$Y)) #Convert character to numeric



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





# 4.1) Approach 3 - Bayesian Spatial model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## First, look-up for matching grid cells between reference and conflict year
for(i in seq_along(dat22)){
  tmp <- intersect(dat19c$grid_id, dat22[[i]]$grid_id) #Get matching grid cells
  dat22[[i]] <- filter(dat22[[i]], grid_id %in% tmp) #Filter for matching grid cells in reference year
  dat22[[i]]$grid_id <- as.factor(dat22[[i]]$grid_id) #Drop-out empty factor levels 
}


## Create dummy column to store prediction results (Will be used in the prediction stack)
for(i in seq_along(dat22)){
  dat22[[i]]$Npop_pred <- NA
}




# 4.1.1) Build the mesh
#~~~~~~~~~~~~~~~~~~~~~~~~

## Get AOI of the input city
### List all files in the folders
fp <- list.files(path = "../../../../../GIS/SatelliteImage/Img_AOI",
                 recursive = TRUE,
                 pattern = "\\.shp$",
                 full.names = TRUE)


### Load the files & set them into a list
aoipoly <- list()
for(i in seq_along(fp)) {
  aoipoly[[i]] <- st_read(fp[i])
  names(aoipoly)[[i]] <- basename(dirname(fp))[i]
  aoipoly[[i]]$City <- as.factor(basename(dirname(fp))[i])
  aoipoly[[i]] <- st_transform(aoipoly[[i]],crs = 6381) # Project to CRS with units set in meters (to make the grid in the units of meters)
}


### Combine all polygons into a single shapefile 
aoipolyall <- do.call(what = sf:::rbind.sf, args=aoipoly)


### Standardize Poly names to city names
setdiff(levels(aoipolyall$City), cities)

levels(aoipolyall$City)[levels(aoipolyall$City) == "Velyki_Kopani"] <- "Velyki-Kopani"
levels(aoipolyall$City)[levels(aoipolyall$City) == "Bila_Tserkva"] <- "Bila-Tserkva"

### Subset AOI poly to input city
aoi <- filter(aoipolyall, City == int)


### Transform to sp object
aoi_sp <- as(aoi, "Spatial") #Transform to sp object

bound.outer <- diff(range(dat19c$X))/3
aoi.bdry <- inla.sp2segment(aoi_sp)
max.edge = diff(range(dat19c$X))/(3*5)

coords <- dat19c %>%
  st_drop_geometry() %>%
  data.frame()  %>%
  dplyr::select(c('X', 'Y'))



bnd <- inla.nonconvex.hull(as.matrix(coords))
mesh <- inla.mesh.create(loc = coords,
                         boundary = list(bnd),
                         refine = list(max.edge = max.edge*0.95))
#plot(mesh)
ggplot() + 
  inlabru::gg(mesh) + theme_void()

# mesh <- inla.mesh.2d(loc=coords,
#                      boundary = aoi.bdry,
#                      max.edge = c(2,3) * max.edge,
#                      offset=c(bound.outer/3, bound.outer))
# 
# plot(mesh)



# 4.1.2) Get mesh coordinates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Will be used for the spatial predictions
mesh_points <- cbind(mesh$loc[,1], mesh$loc[,2])



# 4.1.3) Make the SPDE
#~~~~~~~~~~~~~~~~~~~~~~

## Standard SPDE
spde <- inla.spde2.matern(mesh, alpha=2)


## SPDE pc-prior
# We put the prior median at approximately 0.5*diff(range(df$Y_UTM)) - this is roughly the extent of the study area
# The prior probability of marginal standard deviation 3 or more is 0.01.
#pr = min(c(diff(range(mesh$loc[, 1])), diff(range(mesh$loc[, 2])))) / 5
#pr = 0.5*diff(range(df$Y_UTM))


#0.5*diff(range(coords$Y))
spde.pc <- inla.spde2.pcmatern(mesh, alpha = 2,
                               prior.range = c(12000, .5),
                               prior.sigma = c(3, 0.01))



## We can use the same index across the different spdes
indexs <- inla.spde.make.index(name = "spatial", spde$n.spde)



# 4.1.4) Make the A-matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~
## Estimation matrix
A_est <- inla.spde.make.A(mesh = mesh,
                          loc = as.matrix(coords)) ## Estimation matrix


## Prediction matrix
pred_coords <- list()
A_pred <- list()

for(i in seq_along(dat22)){
  pred_coords[[i]] <- dat22[[i]][,c('X','Y')]
  pred_coords[[i]] <- pred_coords[[i]] %>% st_drop_geometry() 
  
  A_pred[[i]] <- inla.spde.make.A(mesh = mesh, 
                                  loc = as.matrix(pred_coords[[i]])) ## Prediction matrix
}



# 4.1.5) Data and prediction stacks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Estimation stack
stk.est <- inla.stack(
  data = list(Npop = round(dat19c$Npop)),
  A = list(A_est,1), 
  effects = list(spatial = indexs,
                 data.frame(Intercept = 1,
                            Ncars = round(dat19c$Ncars_avg),
                            ID = 1:nrow(dat19c))),
  tag = 'estimation')



### Prediction stack

stk.pred <- list()

for(i in seq_along(dat22)){
  
  stk.pred[[i]] <- inla.stack(data = list(Npop_pred = dat22[[i]]$Npop_pred),
                              A = list(A_pred[[i]], 1), 
                              effects = list(spatial = indexs,
                                             data.frame(Intercept = 1,
                                                        Ncars = round(dat22[[i]]$Ncars_avg),
                                                        ID = 1:nrow(dat22[[i]]))),
                              tag = 'prediction')
}

### Put stacks together
stk.full <- list()

for(i in seq_along(dat22)){
  stk.full[[i]] <- inla.stack(stk.est, stk.pred[[i]])
}




# 4.1.6) Fit the model
#~~~~~~~~~~~~~~~~~~~~~~
m0 <- list()
for(i in seq_along(dat22)){
  print(paste('Fitting 2022 model for:', names(dat22)[i]))
  
  m0[[i]] <- inla(Npop ~ 0 + Intercept + f(Ncars, model = "rw1", hyper = list(theta = list(prior="pc.prec", param=c(0.05,0.01)))) + f(spatial, model = spde.pc), #0 + Intercept 
                  family="poisson",
                  control.compute = list(config = TRUE,dic=T, waic=T, cpo=T),
                  data = inla.stack.data(stk.full[[i]]),
                  #control.fixed = list(expand.factor.strategy = 'inla'),
                  control.predictor = list(A = inla.stack.A(stk.full[[i]]), link=1, compute=T)) #compute gives you the marginals of the linear predictor
}


m1 <- list()

for(i in seq_along(dat22)){
  print(paste('Fitting 2022 model for:', names(dat22)[i]))
  
  m1[[i]] <- inla(Npop ~ 0 + Intercept + f(Ncars, model = "rw1") + f(spatial, model=spde), #0 + Intercept 
                  family="poisson",
                  control.compute = list(config = TRUE,dic=T, waic=T, cpo=T),
                  data = inla.stack.data(stk.full[[i]]),
                  #control.fixed = list(expand.factor.strategy = 'inla'),
                  control.predictor = list(A = inla.stack.A(stk.full[[i]]), link=1, compute=T)) #compute gives you the marginals of the linear predictor
}
# m1b <- inla(Npop ~ 0 + Intercept + f(Ncars, model = "rw1") + f(spatial,model=spde) + f(ID, model = 'iid'), #0 + Intercept 
#            family="poisson",
#            control.compute = list(config = TRUE,dic=T, waic=T, cpo=T),
#            data = inla.stack.data(stk.full),
#            #control.fixed = list(expand.factor.strategy = 'inla'),
#            control.predictor = list(A = inla.stack.A(stk.full), link=1, compute=T)) #compute gives you the marginals of the linear predictor



# m2 <- inla(Npop ~ 0 + Intercept + f(inla.group(Ncars), model = "rw2") + f(spatial, model=spde), #0 + Intercept 
#            family="poisson",
#            control.compute = list(config = TRUE,dic=T, waic=T, cpo=T),
#            data = inla.stack.data(stk.full),
#            #control.fixed = list(expand.factor.strategy = 'inla'),
#            control.predictor = list(A = inla.stack.A(stk.full), link=1, compute=T)) #compute gives you the marginals of the linear predictor



#~~~~~~~~~~~~~~~~~~~~~~~~
# 5) Go for the plotting
#~~~~~~~~~~~~~~~~~~~~~~~~

# Get the predictions
index.pred <- list()
pred_vals <- list()
obs_vals <- list()


model <- m1

for(i in seq_along(stk.full)){
  
  ## Get the right index
  index.pred[[i]] <- inla.stack.index(stk.full[[i]], "prediction")$data
  
  ## Dummy observed vs. fitted values (observed are not real 'observed', as this is pop from 2019 and not 2022, where we are actually estimating)
  pred_vals[[i]] <- model[[i]]$summary.fitted.values[index.pred[[i]],'mean'] 
  obs_vals[[i]] <- dat22[[i]]$Npop #Actual observed values
  
  
  ## Append predicted values to 2022 data frame
  dat22[[i]]$Npop_war <- round(pred_vals[[i]])
  
  
  ## Get relative change at the grid cell level
  dat22[[i]]$Relative_change <- ((dat22[[i]]$Npop_war - dat22[[i]]$Npop) / dat22[[i]]$Npop)*100
  
  
  ## Make data frame for plotting smoothed function of baseline year
  df <- data.frame(ID = model[[i]]$summary.random$Ncars$ID,
                        Mean = model[[i]]$summary.random$Ncars$mean,
                        LB = model[[i]]$summary.random$Ncars$`0.025quant`,
                        UB = model[[i]]$summary.random$Ncars$`0.975quant`)
  
}


## Bind data into single data frame
dat22b <- do.call(what = sf:::rbind.sf, args=dat22)


## Visualize the progress
i=2
plot(pred_vals[[i]] ~ obs_vals[[i]])

ggplot(df, aes(x = ID, y = Mean)) + 
  geom_line(aes(y = Mean)) + 
  geom_ribbon(aes(ymin = LB, ymax = UB), alpha = 0.2) +
  theme_bw(base_size = 20)

# data.plot <- data.frame(ID = m2$summary.random$`inla.group(Ncars)`$ID,
#                         Mean = m2$summary.random$`inla.group(Ncars)`$mean,
#                         LB = m2$summary.random$`inla.group(Ncars)`$`0.025quant`,
#                         UB = m2$summary.random$`inla.group(Ncars)`$`0.975quant`)

              

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
  filter(Month == 'feb') %>%
  #mapview(zcol = 'Npop_war', at = seq(0, 11000, 1000)) 
  mapview(zcol = 'Relative_change', col.regions = palfunc)



### Plot with dynamic baseline (i.e., we don't care about matching grid_ids across 2022 months)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Reshape data for some plotting 
war <- dat22b[,c('grid_id', 'City','Year','Month','Ncars_avg','Npop_war')]; war$grid_id <- factor(war$grid_id)


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


