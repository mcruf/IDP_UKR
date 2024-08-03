##########################################################
#                                                        #
#      Classify cities/oblasts into macro-regions        #
#                                                        #
##########################################################


## The following script classifies cities or oblsts in three macro-regions: east, central, and west.
## Outputs from this script are sourced in other scripts, mainly for plotting purposes.

## Please adapt the folder paths according to your own needs!


## Code written by: Marie-Christine Rufener < macrufener@gmail.com > 
## Last update: May 2023

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~
library(sf); sf_use_s2(FALSE)
library(mapview)

#~~~~~~~~~
# Set WD
#~~~~~~~~~
## Set main working directory
#setwd('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GitHub/IDP_UKR/')
setwd("~/Documents/IDP_UKR/") 


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~
# 1) Load data files
#~~~~~~~~~~~~~~~~~~~~~~


# 1.1) Load polygons of interest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Polygons are specific to whether 'city' or 'oblast' is being selected

if(AGGREGATE == 'City'){

files <- list.files(path = "GIS/SatelliteImage/Img_AOI/",
                    recursive = TRUE,
                    pattern = "\\.shp$",
                    full.names = TRUE)

imgaoi <- list()
for(i in seq_along(files)) {
  imgaoi[[i]] <- st_read(files[i],  quiet = T)
  imgaoi[[i]]$City <- basename(dirname(files))[i]
  #imgaoi[[i]] <- st_transform(imgaoi[[i]],crs = 6381) # Project to CRS with units set in meters (eases the process of making the grid)
}


aoi <- do.call('rbind', imgaoi)
centroids <- st_coordinates(st_centroid(aoi))

aoi <- cbind(aoi, centroids)

} else if(AGGREGATE == 'Oblast'){
  
  adm <- st_read("GIS/Administrative_divisions/Ukraine/Adm_1/ukr_admbnda_adm1_sspe_20220114.shp") 
  adm <-  subset(adm, !ADM1_EN %in% c("Avtonomna Respublika Krym","Sevastopilska")) # Remove Sevastopol & Cri
  adm$ADM1_EN <- as.factor(adm$ADM1_EN)
  colnames(adm)[3] <- "Oblast"
  adm <- adm[,c('Oblast')]
  
  ## Standrdize oblast names to match to the ones in the data
  levels(adm$Oblast)[levels(adm$Oblast) == "Cherkaska"] <- "Cherkasy"
  levels(adm$Oblast)[levels(adm$Oblast) == "Chernihivska"] <- "Chernihiv"
  levels(adm$Oblast)[levels(adm$Oblast) == "Chernivetska"] <- "Chernivtsi"
  levels(adm$Oblast)[levels(adm$Oblast) == "Dnipropetrovska"] <- "Dnipropetrovsk"
  levels(adm$Oblast)[levels(adm$Oblast) == "Donetska"] <- "Donetsk"
  levels(adm$Oblast)[levels(adm$Oblast) == "Ivano-Frankivska"] <- "Ivano-Frankivsk"
  levels(adm$Oblast)[levels(adm$Oblast) == "Kharkivska"] <- "Kharkiv"
  levels(adm$Oblast)[levels(adm$Oblast) == "Khersonska"] <- "Kherson"
  levels(adm$Oblast)[levels(adm$Oblast) == "Khmelnytska"] <- "Khmelnytskyy"
  levels(adm$Oblast)[levels(adm$Oblast) == "Kirovohradska"] <- "Kirovohrad"
  levels(adm$Oblast)[levels(adm$Oblast) == "Kyivska"] <- "Kiev"
  levels(adm$Oblast)[levels(adm$Oblast) == "Kyivska City"] <- "Kiev City"
  levels(adm$Oblast)[levels(adm$Oblast) == "Luhanska"] <- "Luhansk"
  levels(adm$Oblast)[levels(adm$Oblast) == "Lvivska"] <- "Lviv"
  levels(adm$Oblast)[levels(adm$Oblast) == "Mykolaivska"] <- "Mykolayiv"
  levels(adm$Oblast)[levels(adm$Oblast) == "Odeska"] <- "Odessa"
  levels(adm$Oblast)[levels(adm$Oblast) == "Poltavska"] <- "Poltava"
  levels(adm$Oblast)[levels(adm$Oblast) == "Rivnenska"] <- "Rivne"
  levels(adm$Oblast)[levels(adm$Oblast) == "Sumska"] <- "Sumy"
  levels(adm$Oblast)[levels(adm$Oblast) == "Ternopilska"] <- "Ternopil"
  levels(adm$Oblast)[levels(adm$Oblast) == "Vinnytska"] <- "Vinnytsia"
  levels(adm$Oblast)[levels(adm$Oblast) == "Volynska"] <- "Volyn"
  levels(adm$Oblast)[levels(adm$Oblast) == "Zakarpatska"] <- "Zakarpattia"
  levels(adm$Oblast)[levels(adm$Oblast) == "Zaporizka"] <- "Zaporizhzhya"
  levels(adm$Oblast)[levels(adm$Oblast) == "Zhytomyrska"] <- "Zhytomyr"
  levels(adm$Oblast)[levels(adm$Oblast) == "Avtonomna Respublika Krym"] <- "Crimea"
  levels(adm$Oblast)[levels(adm$Oblast) == "Sevastopilska"] <- "Sevastopilska"
  
  centroids <- st_coordinates(st_centroid(adm))
  
  adm <- cbind(adm, centroids)

}



# 1.2) Load Ukraine shapefile
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ukr <- st_read('GIS/Administrative_divisions/Ukraine/Adm_0/ukr_admbnda_adm0_sspe_20220114.shp')
#plot(st_geometry(ukr), col='gray70', axes = TRUE, graticule = st_crs(ukr), las=1) #Might need to simplify geometry of shapefile

bbox <- st_bbox(ukr) #Get bbox of Ukraine



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Classify AOIs into east, center, west
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
diff(bbox[c(1,3)]) # ~18degrees from east-west

## If we want to divide that into 3 regions (west, center, east), then its
## roughly 6 degrees longitude per division

west = paste(round(c(bbox[1], bbox[1] + 6),2)); west <- as.numeric(as.character(west))
central = c( (west[2] + 0.01), (west[2] + 0.01 + 6))
east =  c( (central[2] + 0.01), (central[2] + 0.01 + 6))


### Classify regions
if(AGGREGATE == 'City'){
aoi_classified  <- aoi %>%
       mutate(Region = ifelse(X <= west[2], 'West',
                       ifelse(X >= central[1] & X <= central[2], 'Central',
                              'East')))
} else if(AGGREGATE == 'Oblast'){
  aoi_classified  <- adm %>%
    mutate(Region = ifelse(X <= west[2], 'West',
                           ifelse(X >= central[1] & X <= central[2], 'Central',
                                  'East')))
}
#mapview(ukr, alpha.region = 0, lwd= 1.5) + 
#mapview(aoi, zcol = 'Region', lwd = 1)


  
#rm(list = ls()[-match("aoi_classified", ls())])
