###############################################
#                                             #
#         Filter false-positives with         #
#           Open Street Map features          #
#                                             #
###############################################

# The following script filters false-positives
# based on features retrieved from Open Street Maps (OSM).

# The first part of the script retrieves the OSM features given the
# AOI of the input city. Features in focus are: landuse, place, natural, leisure & aeroway.
# Detailed consideration for each feature can be seen further below, within the given fetaure.
# Please note that currently only polygon features are retrieved - lines, such as railways, are not considered.
# For more fine tuning, one could take railway lines and add a buffer around it.

# In the second part of the script, we load the geolocations of the detected cars. These are then
# overlayed to the OSM features retrieved in the first part. Cars overlapping with OSM features
# are considered as false-positives and will be filtered out from the data.
# Note: OSM features is not always 100% precise and some level of error has to be acknowledged.
# For instance, sometime OSM considers a given area as 'grassland', whereas in the satellite image
# it is a small parking area.

# Filtered cars are then saved in an appropriate folder in the same input format.



## Code written by: Marie-Christine Rufener < macrufener@gmail.com > 
## Last update: March 2023


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~

#library("OpenStreetMap")
library("mapview")
library("osmdata")
library("sf"); sf_use_s2(FALSE)
library('ggplot2')
library('viridis')
library('httr')


#~~~~~~~~~~~~~~~~~~
# Default inputs
#~~~~~~~~~~~~~~~~~~
## Cities to consider - currently all cities are considered
cities <- c("Cherkasy", "Smila", "Chernihiv", "Hremiach", "Nizhyn", "Chernivtsi",
            "Mamalyha", "Porubne", "Storozhynets", "Dnipro", "KryvyiRih",
            "Donetsk", "Kramatorsk", "Mariupol", "Ivano-Frankivsk", "Kolomyiska",
            "Kharkiv", "Merefa", "Pletenivka", "Kherson", "Velyki_Kopani",
            "Kamyanets-Podilskyi", "Khmelnytskyi", "Kropyvnytskyi", "Oleksandriya",
            "Bila_Tserkva", "Kyiv", "Alchevsk", "Luhansk", "Milove", "Drohobych",
            "Lviv", "Rava-Ruska", "Shehyni", "Mykolaiv", "Pervomaisk", "Kuchurhan",
            "Odessa", "Reni", "Udobne", "Velykodolynske", "Kremenchuk", "Poltava",
            "Rivne", "Sarny", "Katerynivka", "Konotop", "Sumy", "Chortkiv",
            "Ternopil", "Kozyatyn", "Vinnytsia", "Kovel", "Lutsk", "Berehove",
            "Solotvino", "Uzhhorod", "Melitopol", "Zaporizhzhia", "Berdychiv",
            "Zhytomyr")



## Confidence threshold (based on small vehicles only - object class 18)
THRESHOLD <- c("TH_15", "TH_45")[2] # 0.15 and 0.45 (less and more conservative thresholds)
  

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OSM false-positive filtering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## NOTE: update input & output folder paths accordingly 


## Loop over cities
for(city in 1:length(cities)){
  
  #Sys.sleep(60)
  
  cat("OSM cleaning for:  ", cities[city], "\n",sep="")
  
  CITY <- cities[city]

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 1) Import AOI shapefiles
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # 1.1) List all files
  #~~~~~~~~~~~~~~~~~~~~~~~
  files <- list.files(path= "~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GIS/SatelliteImage/Img_AOI/",
                      recursive = TRUE,
                      full.names = TRUE,
                      pattern = "\\.shp$")
  
  
  # 1.2) Import files
  #~~~~~~~~~~~~~~~~~~
  aoi <- list()
  for(i in seq_along(files)) {
    aoi[[i]] <- st_read(files[i],quiet = T)
    names(aoi)[[i]] <- basename(dirname(files))[i]
    #poly[[i]] <- st_transform(poly[[i]],crs = 6381) # Project to CRS with units set in meters (eases the process of making the grid)
  }
  
  # # Combine all AOI polygons into a single shapefile 
  # aoi2 <- do.call(what = sf:::rbind.sf, args=aoi)
  # aoi2$id <- rownames(aoi2)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2) Get extent of the study area
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  bboxes <- list()
  for(i in seq_along(aoi)){
    bboxes[[i]] <- st_bbox(aoi[[i]])
  }
  names(bboxes) <- names(aoi)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3) Get OSM features for desired city
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # 3.1) Define city of interest & get bounding box
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(CITY == 'Zaporizhzhia'){
    id <- which(names(bboxes) == 'Zaporizhia')
    cbox <- bboxes[[id]]  
  }else{
    id <- which(names(bboxes) == CITY)
    cbox <- bboxes[[id]]  
  }
  
  
  # 3.2) Get OSM features
  #~~~~~~~~~~~~~~~~~~~~~~
  
  ### Landuse
  ############
  while(TRUE){
    
  landuse <-
    
    ## preventing http 504 error with try-error mechanism; runs over the function again if the api call was not fullfilled
    try(  
      opq(bbox = cbox) %>%
      add_osm_feature(key = 'landuse', value = c("farmland", 
                                               "farmyard",
                                               "vineyard",
                                               "forest",
                                               "grass",
                                               "scrub",
                                               "meadow",
                                               "heath",
                                               "railway_station",
                                               "railway",
                                               "industrial")) %>% 
    osmdata_sf()
    
  )
  
  if(!is(landuse, 'try-error')) break
  }
  cat('Retrieving LANDUSE info sucessful for....', CITY, "\n",sep="")
  
  
  
  
  ### Place
  ##########
  tmp <- paste(CITY,",", " Ukraine", sep="")
  
  # place <- opq(bbox = cbox) %>%
  #   add_osm_feature(key = 'place', 
  #                   value=c("sea")) %>%
  #   osmdata_sf () 
  
  while(TRUE){
    
    if(CITY %in% c('Porubne', 'KryvyiRih', 'Kolomyiska', 'Kamyanets-Podilskyi', 'Oleksandriya', 'Kozyatyn')){ ## Some cities do not exists with the given name on OSM, so take the bbox instead
      place <- 
        try(
          opq(bbox = cbox)%>%
          add_osm_feature(key = 'place', 
                          value=c("sea")) %>%
            osmdata_sf() 
        )
    } else{
      place <- 
        try(
          opq(bbox = tmp)%>%
            add_osm_feature(key = 'place', 
                            value=c("sea")) %>%
            osmdata_sf() 
        )
    }
  
  if(!is(place, 'try-error')) break
  }
  cat('Retrieving PLACE info sucessful for....', CITY, "\n",sep="")
  
  
  
  ### Natural
  ##############
  #tmp <- paste(CITY,",", " Ukraine", sep="")
  
  
  while(TRUE){
    
    if(CITY %in% c('Porubne', 'Dnipro', 'KryvyiRih', 'Kolomyiska', 'Kamyanets-Podilskyi', 'Oleksandriya', 'Kozyatyn')){ ## Some cities do not exists with the given name on OSM, so take the bbox instead
      
  natural <- 
    try( 
        opq(bbox = cbox) %>%
        add_osm_feature(key = 'natural', 
                        value=c("grassland", 
                                "water", 
                                "wetland", 
                                "scrub", 
                                "wood", 
                                "bay")) %>%
    osmdata_sf()
  )
  
    }else{
      natural <- 
        try( 
          opq(tmp) %>%
            add_osm_feature(key = 'natural', 
                            value=c("grassland", 
                                    "water", 
                                    "wetland", 
                                    "scrub", 
                                    "wood", 
                                    "bay")) %>%
            osmdata_sf()
        )
    }
  
  if(!is(natural, 'try-error')) break 
  }
  cat('Retrieving NATURAL info sucessful for....', CITY, "\n",sep="")
  
  
  
  
  ### Leisure
  #############
  while(TRUE){
  leisure <- 
    try( opq(bbox = cbox) %>%
    add_osm_feature(key = c('leisure'), value = "park") %>%
    osmdata_sf()
    )
  if(!is(leisure, 'try-error')) break
  }
  
  cat('Retrieving LEISURE info sucessful for....', CITY, "\n",sep="")
  
  
  
  ### Aeroway
  ############
  while(TRUE){
  aeroway <- 
    try( opq(bbox = cbox) %>%
    add_osm_feature(key = c('aeroway'), value = c("aerodrome","apron")) %>%
    osmdata_sf()
    )
  if(!is(aeroway, 'try-error')) break
  }
  
  cat('Retrieving AEROWAY info sucessful for....', CITY, "\n",sep="")
  
  
  ## to see that on the map
  # mapview(cbox,  alpha.regions=0, col.regions = 2) +
  #   mapview(aoi[[CITY]], alpha.regions = 0.1) +
  #   mapview(landuse$osm_polygons) +
  #   mapview(landuse$osm_multipolygons) +
  #   mapview(place$osm_polygons) +
  #   mapview(place$osm_multipolygons) +
  #   mapview(natural$osm_polygons) +
  #   mapview(natural$osm_multipolygons) +
  #   mapview(leisure$osm_polygons) +
  #   mapview(leisure$osm_multipolygons) +
  #   mapview(aeroway$osm_polygons) +
  #   mapview(aeroway$osm_multipolygons)
  
  
  
  
  # 3.3 Combine all features into a single file
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  ## 3.3.1) Landuse
  
  # if( !(is.null(landuse$osm_polygons)) | !(is.null(landuse$osm_multipolygons))){
  #   landuse2 <- rbind(landuse$osm_polygons[,c('landuse','geometry')],
  #                 landuse$osm_multipolygons[,c('landuse','geometry')])
  #   colnames(landuse2)[1] <- "value"
  #   landuse2$Key <- "landuse"
  # }
  
  lpoly <- length(landuse$osm_polygons$osm_id)
  lmpoly <- length(landuse$osm_multipolygons$osm_id)
  
  
  if( lpoly >= 1  & lmpoly >= 1){
      lluse <- rbind(landuse$osm_polygons[,c('landuse','geometry')],
                     landuse$osm_multipolygons[,c('landuse','geometry')])
      
      colnames(lluse)[1] <- "value"
      lluse$Key <- "landuse"
      
    } else if(lpoly == 0 & lmpoly >= 1){
      lluse <- landuse$osm_multipolygons[,c('landuse','geometry')]
      
      colnames(lluse)[1] <- "value"
      lluse$Key <- "landuse"
      
    } else if(lpoly >= 1 & lmpoly == 0){
      lluse <- landuse$osm_polygons[,c('landuse','geometry')]
      
      colnames(lluse)[1] <- "value"
      lluse$Key <- "landuse"
      
    } else if(lpoly == 0 & lmpoly == 0){
    #lluse <- NULL
    lluse <- data.frame('value' = NA, 'geometry' = NA, 'Key' = NA)
  }
  
  
  
  
  
  ## 3.3.2) Place
  
  # if( !(is.null(place$osm_polygons)) | !(is.null(place$osm_multipolygons))){
  # place2 <- rbind(place$osm_polygons[,c('place','geometry')],
  #                 place$osm_multipolygons[,c('place','geometry')])
  # colnames(place2)[1] <- "value"
  # place2$Key <- "place"
  # }
  
  ppoly <- length(place$osm_polygons$osm_id)
  pmpoly <- length(place$osm_multipolygons$osm_id)
  
    
  if( ppoly >= 1  & pmpoly >= 1){
      puse <- rbind(place$osm_polygons[,c('place','geometry')],
                     place$osm_multipolygons[,c('place','geometry')])
      
      colnames(puse)[1] <- "value"
      puse$Key <- "place"
      
    } else if(ppoly == 0 & pmpoly >= 1){
      puse <- place$osm_multipolygons[,c('place','geometry')]
      
      colnames(puse)[1] <- "value"
      puse$Key <- "place"
      
    } else if(ppoly >= 1 & pmpoly == 0){
      puse <- place$osm_polygons[,c('place','geometry')]
      
      colnames(puse)[1] <- "value"
      puse$Key <- "place"
      
    } else if(ppoly == 0 & pmpoly == 0){
    #puse <- NULL
    puse <- data.frame('value' = NA, 'geometry' = NA, 'Key' = NA)
  }
  
  
  
  ## 3.3.3) Natural
  # if( !(is.null(natural$osm_polygons)) | !(is.null(natural$osm_multipolygons))){
  # natural2 <- rbind(natural$osm_polygons[,c('natural','geometry')],
  #                   natural$osm_multipolygons[,c('natural','geometry')])
  # colnames(natural2)[1] <- "value"
  # natural2$Key <- "natural"
  # }
  
  npoly <- length(natural$osm_polygons$osm_id)
  nmpoly <- length(natural$osm_multipolygons$osm_id)
  
  
  if( npoly >= 1  & nmpoly >= 1){
      nuse <- rbind(natural$osm_polygons[,c('natural','geometry')],
                    natural$osm_multipolygons[,c('natural','geometry')])
      
      colnames(nuse)[1] <- "value"
      nuse$Key <- "natural"
      
    } else if(npoly == 0 & nmpoly >= 1){
      nuse <- natural$osm_multipolygons[,c('natural','geometry')]
      
      colnames(nuse)[1] <- "value"
      nuse$Key <- "natural"
      
    } else if(npoly >= 1 & nmpoly == 0){
      nuse <- natural$osm_polygons[,c('natural','geometry')]
      
      colnames(nuse)[1] <- "value"
      nuse$Key <- "natural"
      
    } else if(npoly == 0 & nmpoly == 0){
    #nuse <- NULL
    nuse <- data.frame('value' = NA, 'geometry' = NA, 'Key' = NA)
  }
  
  
  
  
  ## 3.3.4) Leisure
  
  # if( !(is.null(leisure$osm_polygons)) | !(is.null(leisure$osm_multipolygons))){
  # leisure2 <- rbind(leisure$osm_polygons[,c('leisure','geometry')],
  #                   leisure$osm_multipolygons[,c('leisure','geometry')])
  # colnames(leisure2)[1] <- "value"
  # leisure2$Key <- "leisure"
  # }
  
  lepoly <- length(leisure$osm_polygons$osm_id)
  lempoly <- length(leisure$osm_multipolygons$osm_id)
  
  
    
  if( lepoly >= 1  & lempoly >= 1){
      leuse <- rbind(leisure$osm_polygons[,c('leisure','geometry')],
                    leisure$osm_multipolygons[,c('leisure','geometry')])
      
      colnames(leuse)[1] <- "value"
      leuse$Key <- "leisure"
      
    } else if(lepoly == 0 & lempoly >= 1){
      leuse <- leisure$osm_multipolygons[,c('leisure','geometry')]
      
      colnames(leuse)[1] <- "value"
      leuse$Key <- "leisure"
      
    } else if(lepoly >= 1 & lempoly == 0){
      leuse <- leisure$osm_polygons[,c('leisure','geometry')]
      
      colnames(leuse)[1] <- "value"
      leuse$Key <- "leisure"
      
    } else if(lepoly == 0 & lempoly == 0){
    #leuse <- NULL
    leuse <- data.frame('value' = NA, 'geometry' = NA, 'Key' = NA)
  }
  
  
  
  
  
  ## 3.3.5) Aeroway
  # if( !(is.null(aeroway$osm_polygons)) | !(is.null(aeroway$osm_multipolygons))){
  # aeroway2 <- rbind(aeroway$osm_polygons[,c('aeroway','geometry')],
  #                   aeroway$osm_multipolygons[,c('aeroway','geometry')])
  # colnames(aeroway2)[1] <- "value"
  # aeroway2$Key <- "aeroway"
  # }
  
  apoly <- length(aeroway$osm_polygons$osm_id)
  ampoly <- length(aeroway$osm_multipolygons$osm_id)
  
  
  if( apoly >= 1  & ampoly >= 1){
      ause <- rbind(aeroway$osm_polygons[,c('aeroway','geometry')],
                     aeroway$osm_multipolygons[,c('aeroway','geometry')])
      
      colnames(ause)[1] <- "value"
      ause$Key <- "aeroway"
      
    } else if(apoly == 0 & ampoly >= 1){
      ause <- aeroway$osm_multipolygons[,c('aeroway','geometry')]
      
      colnames(ause)[1] <- "value"
      ause$Key <- "aeroway"
      
    } else if(apoly >= 1 & ampoly == 0){
      ause <- aeroway$osm_polygons[,c('aeroway','geometry')]
      
      colnames(ause)[1] <- "value"
      ause$Key <- "aeroway"
      
    } else if(apoly == 0 & ampoly == 0){
    #ause <- NULL
    ause <- data.frame('value' = NA, 'geometry' = NA, 'Key' = NA)
  }
  
  
  
  
  # mapview(landuse2, col.regions = "forestgreen") +
  #   mapview(natural2, col.regions = "darksalmon") +
  #   #mapview(place2, col.regions = 'deepskyblue3') +
  #   mapview(leisure2, col.regions = 'darkorange') +
  #   mapview(aeroway2, col.regions = 'gray') 
  
  #osmfeat <- rbind(landuse2, place2, natural2, leisure2, aeroway2)
  #osmfeat <- rbind(landuse2, natural2, leisure2, aeroway2)
  #osmfeat <- rbind(landuse2, place2, natural2, leisure2)
  #osmfeat <- rbind(landuse2, natural2, leisure2)
  
  
  osml <- list(lluse, puse, nuse, leuse, ause) ; names(osml) <- c('lluse', 'puse', 'nuse', 'leuse', 'ause')
  
  idx <- vector()
  
  
  ## Identify empty objects & remove them from list
  for(i in seq_along(osml)){
    idx[i] <- isTRUE(length(osml[[i]]$Key) == 1)
    idx2 <- which(idx == TRUE)
    idx3 <- setdiff((1:5), idx2)
  }
  
  
  osml2 <- osml[idx3]
  osmfeat <- osml2 %>% dplyr::bind_rows() 
  
  
  #cropped <- st_crop(osmfeat$geometry,aoi[[id]])
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4) Remove false car detections
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Any cars detected inside the osm feature polygons will be removed
  
  
  # 4.1) Load car coordinates data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # There is a sepparate csv file for each satelltile image (city-date)
  # Here all individual csv files will be bind into a single data frame
  
  
  
  if(THRESHOLD == "TH_15"){
    ## List all files
    ### With full path
    # f <- list.files("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Data/Cars/Car_coords/th_015_oct22/Clean_new", # This version uses vehicle categories other than class 18 
    #                 full.names = TRUE,
    #                 pattern = ".csv" )
  
    f <- list.files("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Data/Cars/Car_coords/th_015_oct22_carclass_18/original",
                    full.names = TRUE,
                    pattern = ".csv" )
  
    ### Only file names
    # ff <- list.files("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Data/Cars/Car_coords/th_015_oct22/Clean_new", # This version uses vehicle categories other than class 18 
    #                  full.names = F,
    #                  pattern = ".csv" )
    
    ff <- list.files("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Data/Cars/Car_coords/th_015_oct22_carclass_18/original",
                     full.names = F,
                     pattern = ".csv" )
  
  
  } else if(THRESHOLD == "TH_45"){
    
    f <- list.files("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Data/Cars/Car_coords/th_045_carclass_18/original",
                    full.names = TRUE,
                    pattern = ".csv" )
    
    
    ### Only file names
    ff <- list.files("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Data/Cars/Car_coords/th_045_carclass_18/original",
                     full.names = F,
                     pattern = ".csv" )
  } 
  
  
  #### Remove b_s1/s2 images (older image version not in use anymore)
  idxbs <- grep("b_s", ff)
  
  if(length(idxbs) != 0){
    ff <- ff[-idxbs]
    f <- f[-idxbs]
  }
  
  
  ## Read all files and retrieve city & date information
  ### Get list of city names and dates
  
  df <- strsplit(ff, split="_")
  
  cars <- list()
  for(i in seq_along(f)){
    
    if(length(df[[i]]) == 7){ #For cities composed by single name
      city <- df[[i]][3]
      date <- paste(df[[i]][4],df[[i]][5],df[[i]][6], sep="_")
    } else { #For cities with composite names (e.g., Bila-Tserkva, Velyki-Kopani)
      city <- paste(df[[i]][3], df[[i]][4], sep = "-")
      date <- paste(df[[i]][5],df[[i]][6],df[[i]][7], sep="_")
    }
    
    
    cars[[i]] <- read.csv(f[i], sep = "\t")
    
    cars[[i]]$City <- city
    cars[[i]]$Date <- date
    
  }
  
  cars2 <- do.call("rbind", cars)
  
  
  ## Make Month & Year columns
  cars2$Month <- substr(cars2$Date,1,3)
  cars2$Year <- substr(cars2$Date,5,8)
  
  
  ## Make unique ID for each car
  cars2$ID <- 1:nrow(cars2)
  
  
  # 4.2) Remove false-positives
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cars2 <- st_as_sf(cars2, coords = c('Longitude', 'Latitude'), crs = st_crs(aoi[[1]]))
  
  cars2$CityDate <- paste(cars2$City, cars2$Date, sep="_")
  
  car_list <- split(cars2, cars2$City)
  
  if(CITY == 'Velyki_Kopani'){
    idx <- which(names(car_list) ==  "Velyki-Kopani")
  } else if(CITY == 'Bila_Tserkva'){
    idx <- which(names(car_list) ==  "Bila-Tserkva")
  } else{
    idx <- which(names(car_list) == CITY)
  }
  
  
  car_list2 <- split(car_list[[idx]],car_list[[idx]]$Date)
  
  
  car_list_clean <- list()
  for(i in seq_along(car_list2)){
    
    print(names(car_list2)[i])
    car_list_clean[[i]] <- car_list2[[i]][lengths(st_intersects(car_list2[[i]], osmfeat)) == 0,]
  
  }
  
  names(car_list_clean) <- names(car_list2)
  
  
  
  # 4.3) Save cleaned data to appropriate directory
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ### Main output directory
  
  if(THRESHOLD == 'TH_15'){
    DIR <- file.path("/Users","marie-christinerufener", "OneDrive - Hamad bin Khalifa University","Projects", "Ukraine","Data","Cars", "Car_coords","th_015_oct22_carclass_18", "OSM_filtered")
  }else if(THRESHOLD == 'TH_45'){
    DIR <- file.path("/Users","marie-christinerufener", "OneDrive - Hamad bin Khalifa University","Projects", "Ukraine","Data","Cars", "Car_coords","th_045_carclass_18", "OSM_filtered")
  }
  
  if(isFALSE(dir.exists(DIR))){
    dir.create(DIR)
  }
  
  
  
  for(i in seq_along(car_list_clean)){
    
    if(dim(car_list_clean[[i]])[1] == 0) next ## Sometimes the OSM filtering can remove all data points, leading to empty data frames.
    
    print(paste("Saving....", names(car_list_clean)[i]), sep="")
    
    coords <- car_list_clean[[i]] %>% 
    st_coordinates() %>%
    as.data.frame() %>%
    dplyr::rename("Longitude" = "X", "Latitude" = "Y")
    
    IDFILE <- unique(car_list_clean[[i]]$CityDate)
    
    if(THRESHOLD == 'TH_15'){
      OUTFILE <- paste("car_coords_", IDFILE, "_0.15", ".csv", sep = "")
    } else if(THRESHOLD == 'TH_45'){
      OUTFILE <- paste("car_coords_", IDFILE, "_0.45", ".csv", sep = "")
    }
      
    write.csv(coords, file.path(DIR, OUTFILE), row.names = FALSE)
  
  }


} #End big city for-loop

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5) Additional analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # 5.1) Get worldpop file with coverage proportion
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## Read file
# proportion <- readRDS("~/Library/CloudStorage/OneDrive-HamadbinKhalifaUniversity/Projects/Ukraine/Imagery_EDA/WorldPop_coverage/worlpop_proportion_image_over_aoi_df.rds")
# 
# ## Set threshold column (Yes - below 50% coverage, No - above 50% coverage)
# proportion$Classification <- ifelse(proportion$Proportion < 0.5, "Yes", "No") #Yes = below 50% population, No = above 50% population
# 
# 
# ## Remove old, unused time stamps (b_s1, b_s2)
# b_s1 <- grep("b_s1", proportion$ID)
# b_s2 <- grep("b_s2", proportion$ID)
# 
# idxrm <- c(b_s1, b_s2)
# proportion <- proportion[-idxrm,]
# 
# 
# ## Filter data for given city & worldpop threshold
# prop2 <- dplyr::filter(proportion, City == CITY & Classification == "No")
# prop2$ID <- factor(prop2$ID)
# 
# 
# ## Get time stamp (will be used to filter out the non-interested time stamps from the cleaned car data)
# prop2$Month <- NA
# prop2$Year <- NA
# prop2$Day <- NA
# 
# ids <- strsplit(as.character(prop2$ID), "_")
# 
# for( i in seq_along(ids)){
#   
#   if(length(ids[[i]]) == 4){
#     
#     prop2[i,]$Month <- paste(ids[[i]][2])
#     prop2[i,]$Year <- paste(ids[[i]][3])
#     prop2[i,]$Day <- paste(ids[[i]][4])
#     
#   } else if(length(ids[[i]]) == 5){
#     
#     prop2[i,]$Month <- paste(ids[[i]][3])
#     prop2[i,]$Year <- paste(ids[[i]][4])
#     prop2[i,]$Day <- paste(ids[[i]][5])
#     
#   }
#   
# }
# 
# prop2$ID2 <- as.factor(with(prop2, paste(Month, Year, Day, sep="_")))
# 
# 
# 
# ### Now analyze the data based on time-stamps of interest (i.e, those in which the image covers at least 50% of the worldpop)
# keep <- which(names(car_list2) %in% levels(prop2$ID2))
# keep <- names(car_list2)[keep]
# 
# car_list3 <- list()
# for( i in keep){
# car_list3[[i]] <- car_list2[[i]]
# }
# names(car_list3) <- keep
# 
# car_list_clean2 <- list()
# for( i in keep){
#   car_list_clean2[[i]] <- car_list_clean[[i]]
# }
# names(car_list_clean2) <- keep
# 
# 
# 
# ## Check for amount of false-detections
# loss <- data.frame(Before = NA, After = NA)
# 
# 
# for(i in seq_along(car_list3)){
#   
#   loss[i, 'Before'] <-  dim(car_list3[[i]])[1]
#   loss[i, 'After'] <-  dim(car_list_clean2[[i]])[1]
#   loss$Diff <- loss$Before - loss$After
#   loss$Perc_loss <- (1 - (loss$After/loss$Before))*100
#   loss[i,"Date"] <- names(car_list3)[i]
# }
# 
# loss %>% arrange(desc(Diff))
# 
# names(car_list3)
# i <- 3
# idx <- setdiff(car_list3[[i]]$ID, car_list_clean2[[i]]$ID)
# false_cars <- dplyr::filter(car_list3[[i]], ID %in% idx)
# st_write(false_cars, '/Users/marie-christinerufener/Downloads/Ivano-Frankivsk/aug_2022_25_false.shp')
# st_write(car_list_clean2[[i]], '/Users/marie-christinerufener/Downloads/Ivano-Frankivsk/aug_2022_25_clean.shp')
# 
# 
# 
# ## See that on a plot
# ggplot(loss, aes(y=Diff)) +
#   geom_boxplot(position=position_dodge(), alpha=0.85, fill="darkorange") +
#   #stat_summary(fun.data = n_fun, geom = "text") +
#   ylab("No. cars") + xlab("") +
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
# ggplot(loss, aes(y=Perc_loss)) +
#   geom_boxplot(position=position_dodge(), alpha=0.85, fill="darkcyan") +
#   #stat_summary(fun.data = n_fun, geom = "text") +
#   ylab("Loss (%)") + xlab("") +
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


