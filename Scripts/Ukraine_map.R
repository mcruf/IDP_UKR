###############################
#                             #
#         Ukraine map         #
#                             #
###############################


# Code written by: Marie-Christine Rufener
# Last update: 03/2023

# The following script produces the map of the study area displayed
# in the paper.


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~
# Default inputs
#~~~~~~~~~~~~~~~~


# Load libraries
#~~~~~~~~~~~~~~~~
library(ggplot2) #For nicer plotting
library(sf) #Read spatial data 
library(ggsn) #To plot spatial data in ggplot
library(ggspatial) #Spatial plotting in ggplot
library(ggpattern) #Add patterns to polygon
library(cowplot) #To add inset map 
#library(repr) #To increase plot size from jupyter


# Set main working directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Set your own directory
setwd("/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GitHub/IDP_UKR/")


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Import shapefiles & data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.1) Ukraine Adminstrative boundaries 1 & 2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data downloaded from: https://data.humdata.org/dataset/cod-ab-ukr

## Adm-1 (Oblast)
adm1 <- st_read('GIS/Administrative_divisions/Ukraine/Adm_1/ukr_admbnda_adm1_sspe_20220114.shp') 


### Get centroid coordinates of the capital (Kyiv) - for nicer plotting
capital <- subset(adm1, ADM1_EN == "Kyivska City")
capital <- st_centroid(capital$geometry)


### Subset ADM-1 without Crimea & Sevastapol - for nicer plotting
ukrsub1 <-  subset(adm1, !ADM1_EN %in% c("Avtonomna Respublika Krym","Sevastopilska"))


### Subset Crimea & Sevastopol - for nicer plotting
ukr1sub_b <-  subset(adm1, ADM1_EN %in% c("Avtonomna Respublika Krym","Sevastopilska"))


### Get centroid coordinates of AMD-1 polygons - will be used later for the positioning of the Oblast labels
adm1_coords <- as.data.frame(st_coordinates(st_centroid(adm1$geometry)))
adm1_coords$oblast <- adm1$ADM1_EN


#### Adapt Oblast names
adm1_coords$oblast <- as.factor(adm1_coords$oblast)
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Avtonomna Respublika Krym'] = "Crimea"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Cherkaska'] = "Cherkasy"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Chernihivska'] = "Chernihiv"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Chernivetska'] = "Chernivtsi"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Dnipropetrovska'] = "Dnipropetrovsk"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Donetska'] = "Donetsk"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Ivano-Frankivska'] = "Ivano-Frankivsk"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Kharkivska'] = "Kharkiv"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Khersonska'] = "Kherson"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Khmelnytska'] = "Khmelnytskyi"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Kirovohradska'] = "Kirovohrad"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Kyivska'] = "Kiev"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Kyivska City'] = "Kiev City"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Luhanska'] = "Luhansk"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Lvivska'] = "Lviv"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Mykolaivska'] = "Mykolaiv"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Odeska'] = "Odessa"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Poltavska'] = "Poltava"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Rivnenska'] = "Rivne"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Sevastopilska'] = "Sevastopol"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Sumska'] = "Sumy"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Ternopilska'] = "Ternopil"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Vinnytska'] = "Vinnytsia"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Volynska'] = "Volyn"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Zakarpatska'] = "Zakarpattia"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Zaporizka'] = "Zaporizhia"
levels(adm1_coords$oblast)[levels(adm1_coords$oblast) == 'Zhytomyrska'] = "Zhytomyr"




## Adm-2 (Raion) - for nicer plotting
adm2 <- st_read("GIS/Administrative_divisions/Ukraine/Adm_2/ukr_admbnda_adm2_sspe_20220114.shp")

### Subset ADM-2 without Crimea & Sevastapol - for nicer plotting
ukrsub2 <-  subset(adm2, !ADM1_EN %in% c("Avtonomna Respublika Krym","Sevastopilska"))




# 1.2) World Administrative boundaries 0 (countries)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data downloaded from: https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/
world <- st_read("GIS/Administrative_divisions/World/world-administrative-boundaries.shp")

## We can also get it directly from the maps package, but at the cost of coarser resolution. See...
# wrld <- st_as_sf(maps::map("world", fill = TRUE, plot = FALSE))


## subset countries around Ukraine
ctry <- world[world$name %in% c("Belarus", "Russian Federation",
                                "Hungary", "Romania", "Slovakia",
                                "Poland", "Moldova, Republic of"), ]

ctry$name[3:4] <- c("Moldova", "Russia") #Rename for convenience


## Get centroids of bordering countries
ctry_coords <- as.data.frame(st_coordinates(st_centroid(ctry$geometry)))
ctry_coords$country <- ctry$name




# 1.3) Areas of interest (AOI)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Polygons related to the 61 cities/villages selected across Ukraine

## List all files
files <- list.files(path= "GIS/SatelliteImage/Img_AOI/",
                    recursive = TRUE,
                    full.names = TRUE,
                    pattern = "\\.shp$")


## Read all files
aoi <- list()

for(i in seq_along(files)) {
  aoi[[i]] <- st_read(files[i])
  names(aoi)[[i]] <- basename(dirname(files))[i]
}


## Combine all AOI polygons into a single shapefile 
aoi <- do.call(what = sf:::rbind.sf, args=aoi)
aoi$id <- rownames(aoi)


## Get centroids of polygons
sf_use_s2(FALSE)
aoi_coords <- st_centroid(aoi)


#~~~~~~~~~~~~~~~~~~~~
# 2) Make the map
#~~~~~~~~~~~~~~~~~~~


## Extend the range of the Ukraine bounding box
bbox_new <- st_bbox(adm1) #Return bbox of Ukraine
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.15 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.20 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] - (0.15 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.17 * yrange) # ymax - top



## Create a general background layot, 
main_theme <- theme(panel.grid.major = element_blank(), #No graticule
                    panel.grid.minor = element_blank(), #No graticule
                    #panel.background = element_rect(fill = '#dcedfc'), #background color 
                    #panel.background = element_rect(fill = '#fefefe'), #background color 
                    panel.background = element_rect(fill = '#e8f2fa'), #background color
                    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), #panel border
                    axis.title = element_blank(), #No title on the axes
                    axis.text = element_text(size = 20))

## Remove Kiev oblast & city form coords
adm1_coords <- filter(adm1_coords, !(oblast %in% c('Kiev', 'Kiev City')))



## Create Ukraine map
ukr_map <- ggplot() +
  
          ## Add shapefiles
          geom_sf(data = ctry, fill = "#F2F2F2", col = "gray80") + # Bordering countries
          geom_sf(data = ukrsub2, col = "gray90", fill = "white") + # Ukr adm-2
          geom_sf(data = ukrsub1, fill = "transparent", col = "gray70", lwd = 0.6) + # Ukr adm-1
          geom_sf_pattern(data = ukr1sub_b, pattern = "stripe", fill = "white", col = "gray70", 
                          lwd = 0.2, pattern_density = 0.2, pattern_spacing = 0.01,
                          pattern_colour  = 'gray70') + # Temporally occupied territories
          
          ## Add AOIs
          geom_sf(data = aoi, fill = "darkorange", col="black", stroke=1, alpha = 0.7) +
          #geom_sf(data = aoi_coords, fill = "#ffa333", shape = 23, size = 2, col="black", stroke=.8) +
          
          ## Add capital
          geom_sf(data = capital, shape = 13, size = 2, stroke = 1.5) +
          
          
          ## Extend bounding box
          coord_sf(xlim = c(bbox_new[1],bbox_new[3]), ylim = c(bbox_new[2],bbox_new[4]),
                   expand = FALSE) +
          
          ## Add Oblast names
          geom_text(data = adm1_coords, aes(x=X, y=Y, label = oblast), 
                    size = 4.5, col= "gray55") +
  
  
          # Add country names
          annotate(geom = "text", x = st_coordinates(capital)[1] + 0.6,
                   y = st_coordinates(capital)[2] + 0.25, label = "Kyiv",
                   fontface = "bold", color = "gray30", size = 5) +
          
          annotate(geom = "text", x = ctry_coords[1, 1],
                   y = ctry_coords[1, 2], label = "ROMANIA", 
                   fontface = "bold", color = "gray75", size = 5) +
          
          annotate(geom = "text", x = ctry_coords[2, 1],
                   y = ctry_coords[2, 2]-0.5, label = "BELARUS", 
                   fontface = "bold", color = "gray75", size = 5) +
          
          annotate(geom = "text", x = ctry_coords[3, 1]+0.05,
                   y = ctry_coords[3, 2], label = "MOLDOVA", 
                   fontface = "bold", color = "gray75", size = 5) +
          
          annotate(geom = "text", x = ctry_coords[6, 1]+18,
                   y = ctry_coords[6, 2]-0.5, label = "RUSSIA", 
                   fontface = "bold", color = "gray75", size = 5) +
          
          annotate(geom = "text", x = ctry_coords[5, 1]+1.2,
                   y = ctry_coords[5, 2], label = "HUNGARY", 
                   fontface = "bold", color = "gray75", size = 5) +
          
          annotate(geom = "text", x = ctry_coords[6, 1]+2,
                   y = ctry_coords[6, 2]-0.5, label = "POLAND", 
                   fontface = "bold", color = "gray75", size = 5) +
          
          annotate(geom = "text", x = ctry_coords[7, 1]+1.5,
                   y = ctry_coords[7, 2]+0.15, label = "SLOVAKIA", 
                   fontface = "bold", color = "gray75", size = 5) +
          
          annotate(geom = "text", x = 32,
                   y = 44.5, label = "B l a c k   S e a", 
                   fontface = "italic", color = "gray70", size = 5,
                   angle = 310) +
          
          ## Add north arrow
          ggsn::north(data = adm1, location = "bottomright", symbol = 1,
                      anchor = c(x = 41.25, y = 44.6)) +
          
          ## Add scalebar
          ggsn::scalebar(data = adm1, 
                         anchor = c(x = 42.3, y = 44.1),
                         location = "bottomright", dist = 150,
                         dist_unit = "km", transform = TRUE,  model = "WGS84",
                         border.size = 0.5, st.size=3.5) +
          
          main_theme


## Create inset map related to Europe
## Make the inset map for Europe based on WGS84 crs 
europe_map <- ggplot() +
              geom_sf(data = world, col="gray50", fill="gray85") +
              geom_rect( aes(
                xmin = st_bbox(adm1)[[1]],
                ymin = st_bbox(adm1)[[2]],
                xmax = st_bbox(adm1)[[3]],
                ymax = st_bbox(adm1)[[4]]),
                fill = NA, 
                colour = "darkred",
                size = 0.8 ) +
              coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE) +
              theme_minimal() +
              theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = '#FEFEFE'),
                    panel.border = element_rect(colour = "black", fill=NA, size=1),
                    axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank())


## Now go for the final map...
ukr_map_final <- ggdraw() +
                 draw_plot(ukr_map) +
                draw_plot(europe_map, 
                          x = 0.792, # The distance along a (0,1) x-axis to draw the left edge of the plot
                          y = 0.75,  # The distance along a (0,1) y-axis to draw the bottom edge of the plot
                          width = .2,  # The width and height of the plot expressed as proportion of the entire ggdraw object
                          height = .2)


#ukr_map_final


## Save the output
OUTFILE <- ('/Users/marie-christinerufener/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/map.png')
ggsave(filename=OUTFILE, plot = ukr_map_final, dpi = 400) #png,jpeg,pdf,....
