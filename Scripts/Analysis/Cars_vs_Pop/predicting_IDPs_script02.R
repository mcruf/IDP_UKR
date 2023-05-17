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
# cities with small extent.

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
## Last update: May 2023


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
load("Npop_cars_1000_m.RData")
popcar <- popcar2; rm('popcar2')



#~~~~~~~~~~~~~~~~~~~~~~
# 2) Data cleaning
#~~~~~~~~~~~~~~~~~~~~~~
# We have to remove images in which the bulk of the city
# was obstructed by clouds. This is necessary in order
# to preserve the 'cleanest' relationship between car and population.
# Ignoring these images otherwise risk to underestimate the IDPs.


# 2.1) Load the commented imagery dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This dataset contains information on the presence of
# snow and clouds for each of the downloaded satellite images.
# It also contains additional comments on the cloud extent, all based on
# visual examination of each individual image.
# There is a column called 'Keep_image', which basically contains information
# on which images to keep. Images marked as 'No' refer to cases in which
# it was heavily obstructed by clouds, and would as such mask the underlying car~pop dynamics.
# Note: the same dataset was also used in the FINAL_Filtering_script_04.R script.


## Read the data
dfcl <- readxl::read_excel("../../../../ImageFeatures/Image_cloud_snow_carDetection_evaluation.xlsx"); dfcl <- as.data.frame(dfcl)
colnames(dfcl)[1:2] <- c('City', 'Date')


# 2.2) Standardize the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Standardize city names
setdiff(popcar$City, dfcl$City)
dfcl$City <- as.factor(dfcl$City)
levels(dfcl$City)[levels(dfcl$City) == "Zaporizhzhia"] <- "Zaporizhia"
setdiff(popcar$City, dfcl$City)


## Create a common identifier
dfcl$CityDate <- paste(dfcl$City, dfcl$Date, sep = "_")

## Get images that should be filtered out
img <- filter(dfcl, Keep_image == 'No') %>% dplyr::select(CityDate)


# 2.3) Filter out the selected images
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
length(unique(popcar$CityDate)) #Total number of images
length(intersect(popcar$CityDate, img$CityDate)) #No. of images that will be removed

## Filter out images
popcar <- filter(popcar, !(CityDate %in% img$CityDate))

## Remove empty factor levels
popcar[,c('CityDate', 'grid_id','City', 'Date', 'Year', 'Month')] <- lapply(popcar[,c('CityDate', 'grid_id','City', 'Date', 'Year', 'Month')]
,factor)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Inferring IDPs from cars
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# There are only a few cities in which there is data
# for the same month in both baseline (2019) and conflict (2022) years.
# We have to identify those cities, and subsequently which months are common
# between the two years.

YEARS <- c('2019', '2022') #Define baseline year and year of interes


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
  
  if(isTRUE(any(rowSums(tab) >= 2))){ #Any row with sum >= 2 indicates matching months between both years

    cities[i] <- cit
  }

}

cities <- cities[!is.na(cities)] #Remove NAs



# 3.2) Get common months for city of interest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Select city of interest
int <- cities[1]

## Filter data for given city
dat <- popcar_aggr_grid[[int]] %>%
  filter(Year %in% YEARS)

## Get common months between the two years
dat_list <- split(dat, dat$Year)
comn <- intersect(levels(factor(dat_list[[1]]$Month)),factor(dat_list[[2]]$Month))


## Filter data for common month
for(i in seq_along(dat_list)){
  dat_list[[i]] <- filter(dat_list[[i]], Month %in% comn)
  dat_list[[i]]$Month <- factor(dat_list[[i]]$Month)
  
  ## Round averaged values
  dat_list[[i]]$Ncars_avg <-  round(dat_list[[i]]$Ncars_avg)
  dat_list[[i]]$Npop <-  round(dat_list[[i]]$Npop)
}



# 3.3) Fit Generalized Additive Model (GAM)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We estimate the car~pop relationship for a given month from the baseline year (2019).
# The estimated parameters are then applied on cars detected for the same month 
# for the conflict year,such that number of people can be predicted.


### Split 2019 data on month
dat19 <- split(dat_list[[1]], dat_list[[1]]$Month)


### Quick visual check
dat19[[1]] %>%
  # filter(!(Npop < 3000 & Ncars_avg > 85)) %>% 
  #filter(Ncars_avg < 100) %>% 
  ggplot(aes(y = Npop, x = Ncars_avg, col = Month, group = Year)) +
  geom_point(alpha = 0.5, size = 3) +
  facet_wrap(Month~ ., scales = "free", ncol=1) +
  ylab("No. Population") + xlab("No. cars") + ggtitle(int) +
  theme_bw() +
  geom_smooth(method = "gam", col="black",
              #method.args = list(family = "poisson"),
              method.args = list(family = "gaussian"),
              formula = y ~ s(x, bs = "cr", k=5))


### Filter outstanding outliers for improved model fit
if(int == 'Alchevsk'){
  # dat19[[1]] <- dat19[[1]] %>%
  #               filter(Ncars_avg < 20) 
} else if(int == 'Ivano-Frankivsk'){
  # dat19[[1]] <- dat19[[1]] %>%
  #   filter(Ncars_avg < 90) 
  # 
  # dat19[[2]] <- dat19[[2]] %>%
  #   filter(Ncars_avg < 50) 
  # 
  # dat19[[3]] <- dat19[[3]] %>%
  #   filter(Ncars_avg < 70) 
} else if(int  == 'Kherson'){
  # dat19[[1]] <- dat19[[1]] %>%
  #   filter(Ncars_avg < 600) 
} else if(int  == 'Kropyvnytskyi'){
  # dat19[[1]] <- dat19[[1]] %>%
  #   filter(Ncars_avg < 400) 
} else if(int  == 'Lviv'){
  dat19[[1]] <- dat19[[1]] %>%
    filter(!(Npop < 5000 & Ncars_avg > 400))
} else if(int  == 'Mariupol'){
  dat19[[1]] <- dat19[[1]] %>%
    filter(!(Npop < 2000 & Ncars_avg > 45)) %>%
    filter(!(Npop < 3000 & Ncars_avg > 85)) %>% 
    filter(!(Ncars_avg > 80)) 
} else if(int  == 'Melitopol'){
  # dat19[[1]] <- dat19[[1]] %>%
  #   filter(Ncars_avg < 150) 
} else if(int  == 'Melitopol'){
  # dat19[[1]] <- dat19[[1]] %>%
  #   filter(!(Npop < 3000 & Ncars_avg > 80))
} else if(int  == 'Uzhhorod'){
  # dat19[[2]] <- dat19[[2]] %>%
  #  filter(Ncars_avg < 400)
} else if(int  == 'Uzhhorod'){
  dat19[[1]] <- dat19[[1]] %>%
   filter(Ncars_avg < 1000)
} else if(int  == 'Zhytomyr'){
  dat19[[1]] <- dat19[[1]] %>%
    filter(Ncars_avg < 100)
}


### Now fit the model
mod_gam <- list()
for(i in seq_along(dat19)){
  mod_gam[[i]] = gam(Npop ~ s(Ncars_avg, bs = "cr", k = 5),
                     family = 'poisson',
                     #family = 'gaussian',
                     data = dat19[[i]])
}
#summary(mod_gam[[1]])


###  Save the model's visual assessment

p <- list()
for(i in seq_along(mod_gam)){
  
  ## Generate the plot
  p[[i]] <- check(getViz(mod_gam[[i]]),
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
  OUTFILE <- paste(paste(DIR, paste("Model_fit", int, paste(unique(dat19[[i]]$Month)),sep = "_"), sep ="/"), ".jpg",sep = "")

  
  ## Nos save the output
  jpeg(OUTFILE,
       bg = "white", res = 300, 
       width = 20, height = 15, units = 'cm')
  print(p[[i]])
  dev.off()
  
}



# 3.4) Predict 2022 population
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Split 2022 data on month
dat22 <- split(dat_list[[2]], dat_list[[2]]$Month)


## Retrieve car values for 2022
warcar <- list()
for(i in seq_along(dat22)){
  warcar[[i]] = data.frame(Ncars_avg = dat22[[i]]$Ncars_avg)
}

## Now predict
warpop <- list()
for(i in seq_along(dat22)){
  warpop[[i]] <- predict(mod_gam[[i]],
                         newdata = warcar[[i]],
                         type = 'response',
                         se = TRUE)
}


# 3.5) Let's plot the output
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Transform to data frame
df_preds <- list()
for(i in seq_along(dat22)){
  df_preds[[i]] <- data.frame(warcar[[i]], warpop[[i]]) %>%
    mutate(lower = fit - 1.96 * se.fit,
           upper = fit + 1.96 * se.fit)
  df_preds[[i]]$Month <- paste(unique(dat22[[i]]$Month))
}

df_preds <- do.call('rbind', df_preds) ## Bind list into data frame



### To see the progress
df_preds %>%
  filter(Month == levels(factor(df_preds$Month))[1] ) %>%
ggplot(aes(x = Ncars_avg, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'gray92') +
  facet_wrap(Month ~., scales = 'free') +
  geom_line(color = '#56B4E9') +
  theme_bw()


### Retrieve relevant info form baseline year
baseline <- list()

for(i in seq_along(dat19)){
  baseline[[i]] <- as.data.frame(dat19[[i]][c('Ncars_avg', 'Npop', 'Month')])
  baseline[[i]]$geometry <- NULL
}
baseline <- do.call('rbind', baseline)


### Identify period in each data frame
baseline$Contrast <- 'Pre-war'
df_preds$Contrast <- 'War'


### Keep only important columns from df_preds
df_preds2 <- df_preds[,c('Ncars_avg', 'fit', 'Month', 'Contrast')]
colnames(df_preds2)[2] <- 'Npop'
df_tot <- rbind(baseline, df_preds2)
df_tot[,c('Contrast', 'Month')] <- lapply(df_tot[,c('Contrast', 'Month')] , factor)


### Set first letter to uppercase (for plotting purposes)
df_tot$Month <- stringr::str_to_title(df_tot$Month) 

### Standardize theme across plots
Main_theme <- theme(axis.text.x = element_text(size = 16),
                    axis.text.y = element_text(size = 16),
                    axis.title = element_text(size = 18, face = "bold"),
                    panel.border = element_blank(),
                    plot.title = element_text(hjust = 0.5, face= "bold", size = 18),
                    legend.position = "none")


### Now go for the final plot
if(length(dat19) == 1){
  
 p2 <- df_tot %>%
    group_by(Month, Contrast) %>%
    dplyr::summarize(Totpop = sum(Npop)) %>%
    #ungroup() %>%
    #slice(-1) %>%
    mutate(Month = replace(as.character(Month), Contrast=='Pre-war', "Baseline")) %>%
    mutate(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
    as_tibble() %>%
    mutate(End = lag(Totpop),
           xpos = 1:n()-0.5,
           Diff = Totpop - Totpop[1],
           Percent = paste(round(Diff/End*100,1),"%")) %>%
    
    
    ggplot(aes(x=Contrast, y=Totpop, fill = Month, col = Month)) +
    geom_bar(stat="identity",
             #position=position_dodge(preserve = "single"),
             alpha = 0.5,
             lwd = 1) +
    
    # geom_text(aes(Contrast, label=scales::comma(Totpop)),
    #           position = position_dodge2(width = 1),
    #           vjust=5, size = 5, fontface = 'bold',
    #           col = 'gray40') +
    # geom_text(aes(Contrast, label=Month),
    #           position = position_dodge2(width = 1),
    #           vjust=-0.8, size = 5, fontface = 'bold',
    #           col = 'gray40') +
    
    geom_segment(aes(x = xpos, y = End, xend = xpos, yend = Totpop), 
                 size = 1.5,
                 arrow = arrow(length = unit(1.2, "mm"),type = "closed"), col = 'gray30') +
    geom_text(aes(x = xpos, y =  (End+Diff/2), label = Percent, hjust = 1.2, fontface = 2, size =5), col = 'gray30') +
    
    
    scale_fill_manual(name = "Contrast", values=c("cyan4","darkorange","darkorange")) +
    scale_color_manual(name = "Contrast", values=c("cyan4", "darkorange", "darkorange")) +
    scale_y_continuous(labels = comma) +
    ylab('No. people') + xlab('') + ggtitle(int) +
    
    theme_bw() +
    Main_theme

  
} else if(length(dat19) == 2){
  p2 <- df_tot %>%
    group_by(Month, Contrast) %>%
    dplyr::summarize(Totpop = sum(Npop)) %>%
    ungroup() %>%
    slice(-1) %>%
    mutate(Month = replace(as.character(Month), Contrast=='Pre-war', "Baseline")) %>%
    mutate(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
    
    as_tibble() %>%
    mutate(End = lag(Totpop),
           xpos = 1:n()-0.5,
           Diff = Totpop - Totpop[1],
           Percent = paste(round(Diff/End*100,1),"%")) %>%
    
    ggplot(aes(x=Contrast, y=Totpop, fill = Month, col = Month)) +
    geom_bar(stat="identity",
             position=position_dodge2(preserve = "single"),
             alpha = 0.5,
             lwd = 1,
             width=1) +
    # geom_text(aes(Contrast, label=scales::comma(Totpop)),
    #           position = position_dodge2(width = 1),
    #           vjust=5, size = 5, fontface = 'bold',
    #           col = 'gray40') +
    # geom_text(aes(Contrast, label=Month),
    #           position = position_dodge2(width = 1),
    #           vjust=-0.8, size = 5, fontface = 'bold',
    #           col = 'gray40') +
    # 
    geom_segment(aes(x = xpos, y = End, xend = xpos, yend = Totpop), 
                 size = 1.5,
                 arrow = arrow(length = unit(1.2, "mm"),type = "closed"), col = 'gray30') +
    geom_text(aes(x = xpos, y =  (End+Diff/2), label = Percent, hjust = 1.2, fontface = 2, size =5), col = 'gray30') +
    
    scale_fill_manual(name = "Contrast", values=c("cyan4","darkorange","darkorange")) +
    scale_color_manual(name = "Contrast", values=c("cyan4", "darkorange", "darkorange")) +
    scale_y_continuous(labels = comma) +
    
    ylab('No. people') + xlab('') + ggtitle(int) +
    theme_bw() +
    Main_theme()
  
} else if(length(dat19) == 3){
  p2 <- df_tot %>%
    group_by(Month, Contrast) %>%
    dplyr::summarize(Totpop = sum(Npop)) %>%
    ungroup() %>%
    slice(c(-1, -3)) %>%
    mutate(Month = replace(as.character(Month), Contrast=='Pre-war', "Baseline")) %>%
    mutate(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
    
    as_tibble() %>%
    mutate(End = lag(Totpop),
           xpos = 1:n()-0.5,
           Diff = Totpop - Totpop[1],
           Percent = paste(round(Diff/End*100,1),"%")) %>%
    
    ggplot(aes(x=Contrast, y=Totpop, fill = Month, col = Month)) +
    geom_bar(stat="identity",
             position=position_dodge2(preserve = "single"),
             alpha = 0.5,
             lwd = 1,
             width=1) +
    # geom_text(aes(Contrast, label=scales::comma(Totpop)),
    #           position = position_dodge2(width = 1),
    #           vjust=5, size = 5, fontface = 'bold',
    #           col = 'gray40') +
    # geom_text(aes(Contrast, label=Month),
    #           position = position_dodge2(width = 1),
    #           vjust=-0.8, size = 5, fontface = 'bold',
    #           col = 'gray40') +
    
    geom_segment(aes(x = xpos, y = End, xend = xpos, yend = Totpop), 
                 size = 1.5,
                 arrow = arrow(length = unit(1.2, "mm"),type = "closed"), col = 'gray30') +
    geom_text(aes(x = xpos, y =  (End+Diff/2), label = Percent, hjust = 1.2, fontface = 2, size =5), col = 'gray30') +
    
    scale_fill_manual(name = "Contrast", values=c("cyan4","darkorange","darkorange")) +
    scale_color_manual(name = "Contrast", values=c("cyan4", "darkorange", "darkorange")) +
    scale_y_continuous(labels = comma) +
    
    ylab('No. people') + xlab('') + ggtitle(int) +
    theme_bw() +
    Main_theme
}


### Save the output
DIR <- file.path("/Users","marie-christinerufener", "OneDrive - Hamad bin Khalifa University",
                 "Projects", "Ukraine", "Manuscript", "Figures", "IDP", "ModelFit", int)

## Create city-specific directory
if(isFALSE(dir.exists(DIR))){
  dir.create(DIR)
}

## Set output file name
OUTFILE <- paste(paste(DIR, paste("IDP_prediction", int,sep = "_"), sep ="/"), ".jpg",sep = "")


### Save
ggsave(filename = OUTFILE, 
       plot = p2,
       dpi = 300, width = 35, height = 25, unit='cm')

