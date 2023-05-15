###################################################################################
#                                                                                 #
#               Identify images below population threshold                        #
#                                                                                 #
###################################################################################


# The following script aims to identify images that did not cover the bulk of the 
# population distribution.

# Population distribution will be evaluated in terms of the WorldPop proportion coverage
# of the given image relative to its city-specific AOI.
# The threshold is defined upon the proportion value, where images below the given threshold
# will be removed. Here, we use a threshold of 50%, that is, images covering less than 50% of the
## population distribution will be removed.

## Please adapt the folder paths according to your own needs!

## Code written by: Marie-Christine Rufener < macrufener@gmail.com > 
## Last update: May 2023


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~
library(sf)
library(raster)
library(exactextractr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(viridis)
library(plot.matrix)


#~~~~~~~~~~~~~~~~~~
# Default inputs
#~~~~~~~~~~~~~~~~~~

## Population threshold
POP_THRESHOLD <- 0.5 #Define the proportion of population coverage; default is 50% - images covering less than the threshold will be removed from the data

## Worldpop data
DINPUT <- c("Count", "Density")[1] #Define whether to use population density or population count; default is 'count'


#~~~~~~~~~
# Set WD
#~~~~~~~~~
## Set main working directory
#setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine")
setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GitHub/IDP_UKR/") 

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~
# 1) Load the data
#~~~~~~~~~~~~~~~~~~~

# 1.1) Get AOIs
#~~~~~~~~~~~~~~~~
## Each city has a unique AOI

## List all files 
files <- list.files(path= "GIS/SatelliteImage/Img_AOI/",
                    recursive = TRUE,
                    full.names = TRUE,
                    pattern = "\\.shp$"
                    )



## Read all files
aoi <- list()
for(i in seq_along(files)) {
  aoi[[i]] <- st_read(files[i])
  names(aoi)[[i]] <- basename(dirname(files))[i]
  #poly[[i]] <- st_transform(poly[[i]],crs = 6381) # Project to CRS with units set in meters (eases the process of making the grid)
}



## Combine all AOI polygons into a single shapefile 
aoi2 <- do.call(what = sf:::rbind.sf, args=aoi)
aoi2$id <- rownames(aoi2)



# 1.2) Get polygons of satellite images
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## List all files
files2 <- list.files(path= "GIS/SatelliteImage/Img_coverage",
                    recursive = TRUE,
                    full.names = TRUE,
                    pattern = "\\.geojson$"
)


## List all files in the folders -- only partial path name
files2b <- list.files(path = "GIS/SatelliteImage/Img_coverage/",
                  recursive = TRUE,
                  pattern = "\\.geojson$",
                  full.names = F)



## Remove b_s1/s2 images (older image version not in use anymore)
idxbs <- grep("b_s", files2)

if(length(idxbs) != 0){
  files2 <- files2[-idxbs]
  files2b <- files2b[-idxbs]
}



## Read all files
img <- list()
for(i in seq_along(files2)) {
  img[[i]] <- st_read(files2[i])
  names(img)[[i]] <- basename(dirname(files2))[i]
  #imgpoly[[i]] <- st_transform(imgpoly[[i]], crs = 6381) # Project to CRS with units set in meters (to make the grid in the units of meters)
  
  img[[i]] <- img[[i]][,"geometry"] # Keep only geometry
}


## Combine geometries for dates & cities with multiple geometries
fname <- strsplit(files2b, "/") #Split partial path names to retrieve city-date in forloop

for(i in seq_along(img)){
  
  if(length(img[[i]]$geometry) > 1){
    img[[i]] <- st_combine(img[[i]])
    img[[i]] <- st_as_sf(img[[i]])
    st_geometry(img[[i]]) <- "geometry" #Change column name to match with original data
  }
  
  
  ## Append City-Date column 
  img[[i]]$City <- fname[[i]][1]
  img[[i]]$Date <- fname[[i]][2]
}


# Combine all satellite image polygons into a single shapefile 
img2 <- do.call(what = sf:::rbind.sf, args=img)




# 1.3) Get Worldpop data
#~~~~~~~~~~~~~~~~~~~~~~~~
if(DINPUT == "Count"){
  #worldpop <- raster("GIS/Population/WorldPop/Shapefile/Numbers/Unconstrained/100m_resoultion/ukr_ppp_2020_UNadj.tif") # 100m resolution
  worldpop <- raster("GIS/Population/WorldPop/Shapefile/Numbers/Constrained/v2/100m/ukr_pop_2020_100m_constrained_v2.tif") # 100m resolution
} else if(DINPUT == "Density"){
  worldpop <- raster("GIS/Population/WorldPop/Shapefile/Density/ukr_pd_2020_1km_UNadj.tif") # 1km resolution
}

## To see the progress
# plot(log1p(worldpop), col=rev(inferno(50)), axes=FALSE)
# adm <- st_read("/Users/marie-christinerufener/Google Drive/My Drive/Satellite&Cars&Ukraine/Data/GIS/Administrative_divisions/Ukraine/New/Adm_1/ukr_admbnda_adm1_sspe_20220114.shp", quiet=T)
# plot(st_geometry(adm),add=T)




# 1.4) Set same CRS across 
#~~~~~~~~~~~~~~~~~~~~~~~~~~
## Between worlpop data and AOI
if(!isTRUE(st_crs(aoi2)$proj4string == worldpop@crs@projargs)){
  crs(worldpop) <- st_crs(aoi2)$proj4string
}


## Between satellite image polygons and AOI
for(i in seq_along(img2)){
if(!isTRUE(st_crs(aoi2)$proj4string == st_crs(img2$proj4string))){
  st_crs(img2) <- st_crs(aoi2)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Calculate population threshold proportion
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First, we calculate the total number of people per AOI (or average, if pop density is chosen).
# Then, we do the same for the polygons of each individual satellite image.
# Finally, we divide the Npop_image / Npop_aoi to get the proportion.


# 2.1) Extract total population counts for AOI polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(DINPUT == "Count"){
  worldpop_aoi <- exact_extract(worldpop, aoi2, 'sum', append_cols=("id"))
  colnames(worldpop_aoi) <- c("City", "Npop_aoi")
  
} else if(DINPUT == "Density"){
  worldpop_aoi <- exact_extract(worldpop, aoi2, 'mean', append_cols=("id"))
  colnames(worldpop_aoi) <- c("City", "Dpop_aoi")
  
}


# 2.2) Extract total population counts for satellite image polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(DINPUT == "Count"){
  worldpop_img <- exact_extract(worldpop, img2, 'sum', append_cols=(c("City","Date")))
  colnames(worldpop_img) <- c("City", "Date", 'Npop_img')
  
} else if(DINPUT == "Density"){
  worldpop_img <- exact_extract(worldpop, img2, 'mean', append_cols=(c("City","Date")))
  colnames(worldpop_img) <- c("City", "Date", 'Dpop_img')
  
}


# 2.3) Calculate proportion
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
## merge the two dataframes
worldpop_img$City <- as.factor(worldpop_img$City)
worldpop_aoi$City <- as.factor(worldpop_aoi$City)

### Check for spelling differences
setdiff(levels(worldpop_img$City), levels(worldpop_aoi$City))
setdiff(levels(worldpop_aoi$City), levels(worldpop_img$City))


### Fix city names
levels(worldpop_aoi$City)[levels(worldpop_aoi$City) == "Zaporizhia"] <- "Zaporizhzhia"


### Sanity check
setdiff(levels(worldpop_img$City), levels(worldpop_aoi$City))
setdiff(levels(worldpop_aoi$City), levels(worldpop_img$City))


### Now merge the two data 
df <- merge(worldpop_img, worldpop_aoi, by = "City")


### Calculate the proportion
df$Proportion <- df$Npop_img / df$Npop_aoi


### Set Threshold
df$Pop_threshold <- ifelse(df$Proportion < POP_THRESHOLD, "Below", "Above") # Below = below 50%, Above = above 50%

OUTDIR <- file.path('Data','Population','Coverage')
OUTFILE <-  paste(OUTDIR, 'Population_coverage.csv', sep='/')
write.csv(df, OUTFILE, row.names = FALSE)


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Add-on: Go for some analysis
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# # 3.2) Set Year Month column
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ids <- strsplit(df$Date, "_")
# 
# df$Month <- NA
# df$Year <- NA
# df$Day <- NA
# 
# for( i in seq_along(ids)){
#   
#     df[i,]$Month <- paste(ids[[i]][1])
#     df[i,]$Year <- paste(ids[[i]][2])
#     df[i,]$Day <- paste(ids[[i]][3])
#     
# }
# 
# ## Rename month levels - character to numbe
# df$Month <- factor(df$Month)
# levels(df$Month) <- c("4","8","12","2","1","7","6","3","5","11","10","9")
# 
# 
# 
# # 3.1) Boxplot
# #~~~~~~~~~~~~~~
# ggplot(df, aes(y=Proportion)) +
#   geom_boxplot(position=position_dodge(), alpha=0.85, fill="darkorange") +
#   #stat_summary(fun.data = n_fun, geom = "text") +
#   ylab("") + xlab("") +
#   #ggtitle("Number of satellite images per year") +
#   # scale_fill_viridis_d() +
#   theme_pubclean() +
#   theme(axis.text = element_text(size = 12),
#         axis.title = element_text(size=13,face="bold"),
#         axis.text.x = element_blank(),
#         plot.title = element_text(hjust = 0.5, size=15, face="bold"),
#         legend.position = "left")
# 
# 
# 
# 
# # 3.2) Evaluate proportions
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## How many images cover 100% of the total AOI population?
# all <- df[which(df$Proportion >= 1),]
# 
# 
# library(RColorBrewer)
# cols <- brewer.pal(5,"YlGnBu")
# plot(log10(worldpop), colNA="white", col=cols, 
#      xlim = c(35, 37), ylim = c(49.0, 51))
# plot(st_geometry(aoi2[17,]), add= T, lwd = 2)
# plot(st_geometry(img2[248,]), add= T, lwd = 2, col = "darkorange")
# 
# 
# 
# ## How many images cover less than 50% of total AOI population?
# dim(df[which(df$Proportion < 0.5),])[1] / dim(df)[1]
# 
# 
# df %>% filter(Pop_threshold == "Above") %>%
#   ggplot(aes(x = City)) +
#   geom_bar(fill = "darkorange") +
#   geom_text(stat='count', aes(label=after_stat(count)), vjust=0, size = 5) +
#   theme_bw() +
#   coord_flip() +
#   theme(axis.text.x = element_text(size = 12, angle = 90, hjus = 1, vjus =0.5),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_blank(),
#         legend.position = "right")
# 
# 
# dobs <- df %>% 
#   filter(Pop_threshold == "Above") %>%
#   group_by(City) %>% 
#   dplyr::summarize(nobs = n()) %>%
#   data.frame()
# 
# 
# ggplot(dobs, aes(x = reorder(City, -nobs), y = nobs,  fill= nobs)) +
#   geom_col() + 
#   scale_fill_viridis() +
#   #coord_flip() +
#   scale_y_discrete(expand = c(0, 0)) +
#   theme_pubclean() +
#   #geom_text(aes(label = nobs), nudge_y = 2, hjust = 1.5, size= 6) + 
#   geom_text(aes(label = nobs), nudge_y = 2, vjust =1, size= 6) + 
#   theme(axis.text.y = element_blank(),
#         axis.text.x = element_text(size = 12, angle = 90, hjus = 1, vjus =0.5, face = 'bold'),
#         axis.title = element_blank(),
#         legend.position = "none")
# 
# 
# dobs %>% filter(City %in% c('Uzhhorod', 'Berehove', 'Shehyni', 'Drohobych',
#                          'Rava-Ruska','Solotvino', 'Lviv', 'Lviv', 'Kovel',
#                          'Ivano-Frankivsk', 'Kolomyiska', 'Lutsk', 'Ternopil',
#                          'Kyiv','Velykodolynske','Odessa','Pervomaisk','Chernihiv',
#                          'Smila', 'Nizhyn', 'Mykolaiv', 'Cherkasy', 'Kropyvnytskyi',
#                          'Kherson','Sumy', 'Dnipro', 'Zaporizhia','Melitopol', 'Merefa',
#                          'Kharkiv', 'Pletenivka','Mariupol','Kramatorsk','Donetsk',
#                          'Alchevsk','Luhansk')) %>%
# ggplot(aes(x = reorder(City, -nobs), y = nobs,  fill= nobs)) +
#   #ggplot(aes(x = City, y = nobs,  fill= nobs)) +
# 
#   geom_col() +
#   #coord_flip() +
#   scale_y_discrete(expand = c(0, 0)) +
#   scale_fill_viridis() +
#   theme_pubclean() +
#   #geom_text(aes(label = nobs), nudge_y = 2, hjust = 1.5, size= 6) +
#   geom_text(aes(label = nobs), nudge_y = 2, vjust =1, size= 6) +
#   theme(axis.text.y = element_blank(),
#         axis.text.x = element_text(size = 12, angle = 90, hjus = 1, vjus =0.5, face = 'bold'),
#         axis.title = element_blank(),
#         legend.position = "none")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## plotting
# ggplot(df, aes(x = City, y=1, fill = Pop_threshold)) +
#   geom_bar(position="fill", stat="identity") +
#   geom_hline(yintercept = 0.5, col ="black", lwd = 1.5) +
#   theme_bw() +
#   scale_fill_manual(values = c("darkorange", "darkcyan")) +
#   labs(fill='Coverage \nthreshold') +
#   coord_cartesian(expand=F)+
#   theme(axis.text.x = element_text(size = 12, angle = 90, hjus = 1, vjus =0.5),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_blank(),
#         legend.position = "right")
# 
# 
# 
# 
# 
# # 3.3) Check spatio-temporal data availability
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# dobs2 <- df %>% 
#   filter(Pop_threshold == "Above") %>%
#   group_by(City, Year, Month) %>% 
#   dplyr::summarize(nobs = n()) %>%
#   mutate(YearMonth = factor(paste(Year, Month, sep="_"))) %>%
#   data.frame()
# 
# 
# colorder <- c(paste(2019, 1:12, sep="_"),
#               paste(2020, 1:12, sep="_"),
#               paste(2021, 1:12, sep="_"),
#               paste(2022, 1:9, sep="_"))
# 
# dobs2$YearMonth <- factor(dobs2$YearMonth, 
#                          levels=colorder)
# 
# 
# #dobs <- dobs %>% arrange(YearMonth, descending = T) ## Arrange by increase month-year
# 
# 
# 
# dobs_wide <- tidyr::spread(dobs2, City, nobs)
# 
# dobs_wide[is.na(dobs_wide)] <- 0 #Time stamps for which we don't have observation
# 
# dobs_wide <- dobs_wide %>% arrange(YearMonth, descending = T) ## Arrange by increase month-year
# 
# 
# dm <- data.matrix(dobs_wide[,4:ncol(dobs_wide)])
# 
# rownames(dm) <- paste(dobs_wide$YearMonth)
# 
# 
# ## Reorder matrix according to the order of No. observations per city
# xx <- dobs %>% arrange(desc(nobs)) ## Arrange by increase month-year
# xx <- xx$City
# 
# dm2 <- dm[,xx]
# 
# 
# par(mar=c(6,6,0,5))
# plot(dm2, breaks=seq(0,7,1), col=turbo(7),
#      digits=1, text.cell=list(cex=0.5), las=2,
#      xlab="", ylab="", main="")
# 
# 
# ## Reorder matrix with cities ranked from west-east
# source('~/Library/CloudStorage/OneDrive-HamadbinKhalifaUniversity/Projects/Ukraine/Imagery_EDA/WorldPop_coverage/City_classification_west-east.R')
# 
# classification <- aoi_classified %>% 
#                   arrange(X, Y) %>%
#                   select(City, Region) %>%
#                   st_drop_geometry() %>%
#                   data.frame()
# 
# cit <- classification$City
# 
# dm3 <- dm[,cit]
# 
# 
# 
# par(mar=c(6,6,0,5))
# 
# 
# plot(dm3, breaks=seq(0,7,1), col=turbo(7),
#      digits=1, text.cell=list(cex=0.5), las=2,
#      xlab="", ylab="", main="")
# 
# 
# 
# dm4 <- dm[, c('Kyiv', 'Bila_Tserkva')]
# par(mar=c(2,5,1,20))
# plot(dm4, breaks=seq(0,5,1), col=turbo(5),
#      digits=1, text.cell=list(cex=0.8), las=1,
#      xlab="", ylab="", main="", key = NULL)
# 
# dm4b <- dm[, c('Cherkasy', 'Smila')]
# par(mar=c(2,5,1,20))
# plot(dm4b, breaks=seq(0,5,1), col=turbo(5),
#      digits=1, text.cell=list(cex=0.8), las=1,
#      xlab="", ylab="", main="", key = NULL)
# 
# 
# dm4c <- dm[, c('Donetsk', 'Mariupol', 'Kramatorsk')]
# par(mar=c(2,5,1,20))
# plot(dm4c, breaks=seq(0,7,1), col=turbo(7),
#      digits=1, text.cell=list(cex=0.8), las=1,
#      xlab="", ylab="", main="", key = NULL)
# 
# 
# 
# dm4d <- dm[, c('Lviv', 'Shehyni', 'Rava-Ruska')]
# par(mar=c(2,5,1,20))
# plot(dm4d, breaks=seq(0,3,1), col=turbo(3),
#      digits=1, text.cell=list(cex=0.8), las=1,
#      xlab="", ylab="", main="", key = NULL)
# 
# 
# dm4e <- dm[, c('Ivano-Frankivsk', 'Kolomyiska')]
# par(mar=c(2,5,1,20))
# plot(dm4e, breaks=seq(0,3,1), col=turbo(4),
#      digits=1, text.cell=list(cex=0.8), las=1,
#      xlab="", ylab="", main="", key = NULL)
# 
# 
# dm4f <- dm[, c('Solotvino', 'Berehove', 'Uzhhorod')]
# par(mar=c(2,5,1,20))
# plot(dm4f, breaks=seq(0,4,1), col=turbo(4),
#      digits=1, text.cell=list(cex=0.8), las=1,
#      xlab="", ylab="", main="", key = NULL)
# 
# 
# 
# 
# ## City by city
# #~~~~~~~~~~~~~~~
# sort(levels(dobs$City))
# cit <- dobs2 %>% filter(City == "Lviv") %>% arrange(YearMonth, descending = T) #
# 
# 
# cit$Year <- factor(cit$Year, 
#                           levels=c("2019","2020","2021","2022"))
# 
# cit$Month <- factor(cit$Month, 
#                    levels=c(paste(1:12)))
# 
# library(reshape2)
# cit_mat <- dcast(cit, Year~Month, length,drop = FALSE); rownames(cit_mat) <- cit_mat$Year
# 
# cit_mat <- cit_mat[, paste(1:12)]
# 
# # cit_mat <- cit_mat[, paste(1:11)]
# # cit_mat$'12' <- 0
# # tt <- data.frame("1" = 0, "2" = 0, "3" = 0, "4" = 0,"5" = 0, "6" = 0, "7" = 0, "8" = 0, "9" = 0, "10" = 0,"11" = 0, "12" = 0)
# # colnames(tt) <- paste(1:12)
# # rownames(tt) <- "2019"
# # cit_mat <- rbind(tt,cit_mat)
# 
# 
# plot(data.matrix(cit_mat),  col=c("darkcyan","darkgoldenrod1"),
#      digits=1, text.cell=list(cex=1), las=1,
#      fmt.cell='%.f',
#      xlab="Months", ylab="Years", main=unique(cit$City),key=NULL)
# 
# 
# 
# 
# 
# #######################################
# 
# 
# # Evaluate the most outstanding cases (Chortkiv, Luhansk and Porubne)
# dl = strsplit(df$ID, "_")
# 
# for(i in 1:nrow(df)){
#   #print(dl[[i]][3])
#   df$month[i] = dl[[i]][2]
#   df$year[i] = dl[[i]][3]
# }
# 
# 
# ### luhansk
# luhansk_all <- subset(df, City == "Luhansk")
# #luhansk <- subset(df, City == "Luhansk" & Classification == "Yes")
# 
# table(luhansk_all$year, luhansk_all$Classification)
# 
# 
# ### Porubne
# porubne_all <- subset(df, City == "Porubne")
# #porubne <- subset(df, City == "Porubne" & Classification == "Yes" & year %in% c("2020", "2021"))
# 
# table(porubne_all$year, porubne_all$Classification)
# 
# 
# ## Chortkiv
# chortkiv_all <- subset(df, City == "Chortkiv")
# chortkiv <- subset(df, City == "Chortkiv" & Classification == "Yes")
# 
# table(chortkiv_all$year, chortkiv_all$Classification)
