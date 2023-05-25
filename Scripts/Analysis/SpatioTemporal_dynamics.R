#########################################################
#                                                       #
#      Evaluate the spatio-temporal car dynamics        #
#                                                       #
#########################################################


## The following code produces two types of analysis/figures:
## 1) Average car density over time per city (dot-chart)
## 2) Map of relative change of car density across Ukraine (map)

## Code written by: Marie-Christine Rufener < macrufener@gmail.com > 
## Last update: May 2023

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(RColorBrewer)
library(ggrepel)
library(ggpattern)



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine") ## Set appropriate WD


#~~~~~~~~~~~~~~~~~~~~~
# 1) Read data files
#~~~~~~~~~~~~~~~~~~~~~

## List all files (from all the thresholds)
files <- list.files(path = 'Data/Cars/Car_aggregated/POP_CLOUD_filtered/',
                    pattern = '.csv',
                    full.names = TRUE,
                    recursive = T)


## Read the files
cars <- list()
for(i in seq_along(files)) {
  cars[[i]] <- read.csv(files[i])
  cars[[i]]$Threshold <- basename(dirname(files))[i]
}


## Bind the two data files into single data frame
cars <- do.call('rbind', cars)

## To see the progress
table(cars$Threshold) #No. of images for each threshold



#~~~~~~~~~~~~~~~~~~~~~
# 2) Data analysis
#~~~~~~~~~~~~~~~~~~~~~

# 2.1) Calculate the car density
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dive the No. of cars by the area of the image
cars$Dcars <- cars$Ncars / cars$AOI_area_covered_sqkm


# Classify each city into east, west or central region
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Source script that classifies the city/oblast according to its geographical position
### Define to which spatial resolution the data should be evaluated
AGGREGATE <- c('City', 'Oblast')[2]

source('~/Library/CloudStorage/OneDrive-HamadbinKhalifaUniversity/Projects/Ukraine/Imagery_EDA/WorldPop_coverage/CityOblast_classification_west-east.R')

## To see the progress
table(aoi_classified$Region) 


## Merge the info to the car data
### First, standardize city/oblast names

if(AGGREGATE == 'City'){
aoi_classified$City <- as.factor(aoi_classified$City); cars$City <- as.factor(cars$City)
setdiff(aoi_classified$City, cars$City)

levels(aoi_classified$City)[levels(aoi_classified$City) == 'Velyki_Kopani'] <- 'Velyki-Kopani'
levels(aoi_classified$City)[levels(aoi_classified$City) == 'Bila_Tserkva'] <- 'Bila-Tserkva'
levels(aoi_classified$City)[levels(aoi_classified$City) == 'Zaporizhia'] <- 'Zaporizhzhia'
setdiff(aoi_classified$City, cars$City) # Sanity check

### Now merge
cars <- merge(cars, aoi_classified[,c('City','Region')], by = 'City')


} else if(AGGREGATE == 'Oblast'){
  
  ## Rename Oblast for Kiev city and Oblast
  kyiv <- filter(cars, City == 'Kyiv'); kyiv$Oblast <- factor(paste('Kiev City'))
  carstmp <- filter(cars, !(City == 'Kyiv'))
  
  cars <- rbind(carstmp, kyiv) 
  
  aoi_classified$Oblast <- as.factor(aoi_classified$Oblast); cars$Oblast <- as.factor(cars$Oblast); cars$City <- as.factor(cars$City)
  setdiff(aoi_classified$Oblast, cars$Oblast)
  
  ### Now merge
  cars <- merge(cars, aoi_classified[,c('Oblast','Region')], by = 'Oblast')

}



# 2.3) Evaluate the temporal dynamics in car density
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(cars$City, cars$Threshold)
cars[,c('Year', 'Threshold', 'Region')] <- lapply(cars[,c('Year', 'Threshold', 'Region')] , factor)


# Rename threshold factor level for better readability
levels(cars$Threshold)[levels(cars$Threshold) == 'th_015_carclass_18'] <- 'Th-15'
levels(cars$Threshold)[levels(cars$Threshold) == 'th_045_carclass_18'] <- 'Th-45'



# Make MonthYear column
cars$MonthYear <- as.factor(paste(cars$Month, cars$Year, sep='-'))
colorder <- c(paste(1:12, 2019, sep="-"),
              paste(1:12, 2020, sep="-"),
              paste(1:12, 2021, sep="-"),
              paste(1:9, 2022, sep="-"))

cars$MonthYear  <- factor(cars$MonthYear, 
                          levels=colorder)

### Now make that numeric (to be used later)
cars$MonthYear2  <- factor(cars$MonthYear, 
                          levels=paste(1:nlevels(cars$MonthYear)))




# Make QuarterYear column
cars$Quarter <- as.factor(lubridate::quarter(cars$DateTime))
cars$QuarterYear <- as.factor(paste(paste('Q', cars$Quarter, sep=''), cars$Year, sep = '-'))

colorder2 <- c(paste(paste('Q', 1:4, sep=''), 2019, sep="-"),
               paste(paste('Q', 1:4, sep=''), 2020, sep="-"),
               paste(paste('Q', 1:4, sep=''), 2021, sep="-"),
               paste(paste('Q', 1:3, sep=''), 2022, sep="-"))

cars$QuarterYear  <- factor(cars$QuarterYear, 
                            levels=colorder2)



## Set baseline, covid and war rectangles (to be used later in the plots)
#baseline <- data.frame(xmin=1, xmax=12, ymin=-Inf, ymax=Inf)
#covid <- data.frame(xmin=15, xmax=36, ymin=-Inf, ymax=Inf)
#war <- data.frame(xmin=36, xmax=45, ymin=-Inf, ymax=Inf)

baseline <- data.frame(xmin=1, xmax=15, ymin=-Inf, ymax=Inf) #Including first three months in 2020 (Covid was declared in March 2020)
covid <- data.frame(xmin=15, xmax=38, ymin=-Inf, ymax=Inf)
war <- data.frame(xmin=38, xmax=45, ymin=-Inf, ymax=Inf)




### Go for the plots ###

## To plot only every second axis label
everysecond <- function(x){
  x <- unique(x)
  x[seq(2, length(x), 2)] <- ""
  x
}


### Average car density per month (line chart)
# cars %>%
#   group_by(MonthYear, City, Threshold) %>%
#   summarize(Avg_cars = median(Dcars, na.rm=T),
#             sd = sd(Dcars, na.rm=T),
#             Region = paste(unique(Region))) %>%
#   
#   ## Filter 5-top cities within each region
#   #filter(Threshold == 'Th-45' & Region == 'West' & City %in% c('Chernivtsi','Ivano-Frankivsk', 'Lviv','Shehyni','Uzhhorod', 'Ternopil' ) & Threshold == 'Th-45') %>%
#   filter(Threshold == 'Th-45' & Region == 'Central' & City %in% c('Kyiv','Odessa', 'Mykolaiv', 'Kherson', 'Chernihiv', 'Cherkasy')) %>%
#   #filter(Threshold == 'Th-45' & Region == 'East'& City %in% c('Donetsk','Kharkiv','Luhansk','Mariupol','Alchevsk', 'Melitopol') & Threshold == 'Th-45') %>%
# 
#   ggplot(aes(x=MonthYear, y = Avg_cars, group = Threshold)) +
#   scale_x_discrete(drop=FALSE, labels = everysecond(colorder)) +
# 
#   geom_rect(data=baseline, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
#             fill="cyan4",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_rect(data=covid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
#             fill="grey70",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_rect(data=war, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
#             fill="darkorange",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_vline(xintercept = 38, color = "darkred", size=0.8) +
#   #geom_line(size =1, aes(col = Threshold)) +
#   #geom_point(size = 3, aes(col = Threshold) +
#   
#   geom_line(size =1, aes(), col = 'cyan4') +
#   geom_point(size = 3, aes(), shape = 21, fill = 'cyan4', col ='gray20') +
#   
#     facet_wrap(City ~ ., scales = 'free_y', ncol = 2) +
#   #scale_color_manual(values=c("cyan4", "#E7B800")) +
#   #scale_color_manual(values=c("#1d72b4", "#e89d41")) +
#   #scale_color_manual(values = '#14746f') +
#   #scale_color_hp(house = "LunaLovegood", discrete=T) + 
#   theme_bw() +
#   ylab(expression(bold(paste("Avergage Car Density (No. cars/km" ^ "2", ")")))) +
#   xlab('Time') +      
#          
#   theme(#legend.position = 'top',
#         legend.position = 'none',
#         axis.text.x = element_text(angle = 90,size = 12, vjust = 0.5),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 13, face='bold', margin = unit(c(3, 0, 0, 0), "mm")),
#         axis.title.y = element_text(size = 13, face='bold', margin = unit(c(0, 3, 0, 0), "mm")),
#         strip.text =  element_text(size = 14, face = 'bold'),
#         strip.background = element_rect(fill = 'gray95'))

#ggsave('~/Desktop/car_dynamics_monthly_east.jpg', dpi = 300, width = 40, height = 20, unit='cm')



### PLOT FOR PAPER ###
### Calculate monthly average car density
tmp <- cars %>%
  group_by(MonthYear, City, Threshold) %>%
  summarize(Avg_cars = median(Dcars, na.rm=T),
            sd = sd(Dcars, na.rm=T),
            Region = paste(unique(Region))) %>%
  filter(Threshold == 'Th-45')

  
cities <- levels(factor(tmp$City))

## Cities for main figure of paper
cit <- c('Chernivtsi', 'Ivano-Frankivsk', 'Uzhhorod',
        'Kyiv', 'Chernihiv', 'Odessa',
        'Donetsk', 'Kharkiv', 'Mariupol')


## Supplementary figure
### Cities to the west
# cit <- c('Berehove',
#          'Drohobych',
#          'Kamyanets-Podilskyi',
#          'Khmelnytskyi',
#          'Kolomyiska',
#          'Kovel',
#          'Lutsk',
#          'Lviv',
#          'Mamalyha',
#          'Porubne',
#          'Rava-Ruska',
#          'Rivne',
#          'Sarny',
#          'Shehyni',
#          'Solotvino',
#          'Storozhynets',
#          'Ternopil')

### Cities to central region
# cit <- c('Bila-Tserkva',
#          'Cherkasy',
#          'Katerynivka',
#          'Kherson',
#          'Konotop',
#          'Kremenchuk',
#          'Kropyvnytskyi',
#          'KryvyiRih',
#          'Kuchurhan',
#          'Mykolaiv',
#          'Nizhyn',
#          'Oleksandriya',
#          'Pervomaisk',
#          'Reni',
#          'Smila',
#          'Velyki-Kopani',
#          'Velykodolynske',
#          'Vinnytsia',
#          'Zhytomyr')

### Cities to east
# cit <- c('Alchevsk',
#          'Dnipro',
#          'Kramatorsk',
#          'Luhansk',
#          'Melitopol',
#          'Merefa',
#          'Milove',
#          'Pletenivka',
#          'Poltava',
#          'Sumy',
#          'Zaporizhzhia')

tmp %>%
  filter(City %in% cit) %>%
  ggplot(aes(x=MonthYear, y = Avg_cars, group = Threshold)) +
  scale_x_discrete(drop=FALSE, labels = everysecond(colorder)) +
  scale_y_continuous(limit=c(0,NA),oob=squish, expand = expansion(mult = c(0, 0.1))) + 
  
  geom_smooth(col = 'gray40') +
  
  geom_rect(data=baseline, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="Before war"),
            #fill="cyan4",
            alpha=0.4,
            inherit.aes = FALSE) +
  
  geom_rect(data=covid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="COVID-19 pandemic"),
            #fill="grey70",
            alpha=0.4,
            inherit.aes = FALSE) +
  
  geom_rect(data=war, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="War"),
            #fill="darkorange",
            alpha=0.4,
            inherit.aes = FALSE) +
  
  #geom_line(size =1, aes(), col = 'cyan4') +
  geom_vline(aes(xintercept = 38, color = "Start of the War"), size=0.8) +
  
  geom_bar(stat = "identity",  position = "stack",fill = 'mistyrose4', col ='gray30', alpha = 0.7) +
  
  
  facet_wrap(factor(City, levels = cit) ~ ., scales = 'free_y', ncol = 3) + #For main plot
  #facet_wrap(factor(City, levels = cit) ~ ., scales = 'free_y', ncol = 2) + #For supplementary plot
  
  theme_bw() +
  ylab(expression(bold(paste("Avergage Car Density (No. cars/km" ^ "2", ")")))) +
  xlab('Time') +
  scale_fill_manual('Period',
                    values = c('cyan4','grey70','darkorange'),  
                    guide = guide_legend(override.aes = list(alpha = c(rep(0.4,3))))) +
  
  scale_color_manual(name = "", 
                     values = c("darkred"))+
  
  theme(legend.position = 'bottom',
        #legend.spacing.x = unit(0.2, "cm"),
        legend.margin = margin(0.5,0,0,-0.5, unit="cm"),
        legend.title.align = 0.5,
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face ='bold'),
        
        axis.text.x = element_text(angle = 90,size = 14, vjust = 0.5),
        axis.text.y = element_text(size = 16),
        
        axis.title.x = element_text(size = 18, face='bold', margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 18, face='bold', margin = unit(c(0, 3, 0, 0), "mm")),
        strip.text =  element_text(size = 15, face = 'bold'),
        strip.background = element_blank())

## Area plot
# tmp %>%
#   filter(City %in% cit) %>%
#   ggplot(aes(x=MonthYear, y = Avg_cars, group = Threshold)) +
#   scale_x_discrete(drop=FALSE, labels = everysecond(colorder)) +
#   scale_y_continuous(limit=c(0,NA),oob=squish, expand = expansion(mult = c(0, 0.1))) + 
#   
#   geom_rect(data=baseline, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="Before war"),
#             #fill="cyan4",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_rect(data=covid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="COVID-19 pandemic"),
#             #fill="grey70",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_rect(data=war, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="War"),
#             #fill="darkorange",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   #geom_line(size =1, aes(), col = 'cyan4') +
#   #geom_bar(stat = "identity",  position = "stack",fill = 'cyan4', col ='gray20') +
#   geom_area(alpha = 0.5, color = 'gray40', fill = 'mistyrose4') +
#   #geom_point(alpha = 0.5, size = 2, shape = 21, fill = 'cyan4', col ='gray20') +
#   geom_point(alpha = 0.5, size = 2, shape = 21, fill = 'mistyrose4', col ='gray40') +
#   
#   geom_vline(aes(xintercept = 38, color = "Start of War"), size=0.8) +
#   
#   facet_wrap(factor(City, levels = cit) ~ ., scales = 'free_y', ncol = 3) + #For main plot
#   #facet_wrap(factor(City, levels = cit) ~ ., scales = 'free_y', ncol = 2) + #For supplementary plot
#   
#   theme_bw() +
#   ylab(expression(bold(paste("Avergage Car Density (No. cars/km" ^ "2", ")")))) +
#   xlab('Time') +
#   scale_fill_manual('Period',
#                     values = c('cyan4','grey70','darkorange'),  
#                     guide = guide_legend(override.aes = list(alpha = c(rep(0.4,3))))) +
#   
#   scale_color_manual(name = "", 
#                      values = c("darkred"))+
#   
#   theme(legend.position = 'bottom',
#         #legend.spacing.x = unit(0.2, "cm"),
#         legend.margin = margin(0.5,0,0,-0.5, unit="cm"),
#         legend.title.align = 0.5,
#         legend.text = element_text(size = 16),
#         legend.title = element_text(size = 18, face ='bold'),
#         
#         axis.text.x = element_text(angle = 90,size = 14, vjust = 0.5),
#         axis.text.y = element_text(size = 16),
#         
#         axis.title.x = element_text(size = 18, face='bold', margin = unit(c(3, 0, 0, 0), "mm")),
#         axis.title.y = element_text(size = 18, face='bold', margin = unit(c(0, 3, 0, 0), "mm")),
#         strip.text =  element_text(size = 15, face = 'bold'),
#         strip.background = element_blank())


## Line plot
# tmp %>%
#   filter(City %in% cit) %>%
#   ggplot(aes(x=MonthYear, y = Avg_cars, group = Threshold)) +
#   scale_x_discrete(drop=FALSE, labels = everysecond(colorder)) +
#   
#   geom_rect(data=baseline, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="Before war"),
#             #fill="cyan4",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_rect(data=covid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="COVID-19 pandemic"),
#             #fill="grey70",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_rect(data=war, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill="War"),
#             #fill="darkorange",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_line(size =1, aes(), col = 'cyan4') +
#   geom_point(size = 3, aes(), shape = 21, fill = 'cyan4', col ='gray20') +
#   
#   geom_vline(aes(xintercept = 38, color = "Start of the War"), size=0.8) +
#   
#   facet_wrap(factor(City, levels = cit) ~ ., scales = 'free_y', ncol = 3) + #For main plot
#   #facet_wrap(factor(City, levels = cit) ~ ., scales = 'free_y', ncol = 2) + #For supplementary plot
#   
#   theme_bw() +
#   ylab(expression(bold(paste("Avergage Car Density (No. cars/km" ^ "2", ")")))) +
#   xlab('Time') +
#   scale_fill_manual('Period',
#                     values = c('cyan4','grey70','darkorange'),  
#                     guide = guide_legend(override.aes = list(alpha = c(rep(0.4,3))))) +
#   
#   scale_color_manual(name = "", 
#                      values = c("darkred"))+
# 
#   theme(legend.position = 'bottom',
#         #legend.spacing.x = unit(0.2, "cm"),
#         legend.margin = margin(0.5,0,0,-0.5, unit="cm"),
#         legend.title.align = 0.5,
#         legend.text = element_text(size = 16),
#         legend.title = element_text(size = 18, face ='bold'),
#         
#         axis.text.x = element_text(angle = 90,size = 14, vjust = 0.5),
#         axis.text.y = element_text(size = 16),
#         
#         axis.title.x = element_text(size = 18, face='bold', margin = unit(c(3, 0, 0, 0), "mm")),
#         axis.title.y = element_text(size = 18, face='bold', margin = unit(c(0, 3, 0, 0), "mm")),
#         strip.text =  element_text(size = 15, face = 'bold'),
#         strip.background = element_blank())

setwd('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Temporal_dynamics/')
OUT <- paste('cardyn_time_main_plot', '.jpg', sep='')
ggsave(OUT, dpi = 300, width = 35, height = 25, unit='cm')

## For Figures in the Supplementary material
# OUT <- paste('cardyn_time_Suppl_west_updated', '.jpg', sep='')
# ggsave(OUT, dpi = 300, width = 35, height = 40, unit='cm')






# for(i in seq_along(cities)){
# 
# tmp %>%
#   filter(City == cities[i]) %>%
#   ggplot(aes(x=MonthYear, y = Avg_cars, group = Threshold)) +
#   scale_x_discrete(drop=FALSE, labels = everysecond(colorder)) +
#   
#   geom_rect(data=baseline, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
#             fill="cyan4",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_rect(data=covid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
#             fill="grey70",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_rect(data=war, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
#             fill="darkorange",
#             alpha=0.4,
#             inherit.aes = FALSE) +
#   
#   geom_line(size =1, aes(), col = 'cyan4') +
#   geom_point(size = 3, aes(), shape = 21, fill = 'cyan4', col ='gray20') +
#   
#   geom_vline(xintercept = 38, color = "darkred", size=0.8) +
#   
#   facet_wrap(City ~ ., scales = 'free_y', ncol = 2) +
#   theme_bw() +
#   ylab(expression(bold(paste("Avergage Car Density (No. cars/km" ^ "2", ")")))) +
#   xlab('Time') +      
#   theme(#legend.position = 'top',
#     legend.position = 'none',
#     axis.text.x = element_text(angle = 90,size = 12, vjust = 0.5),
#     axis.text.y = element_text(size = 12),
#     axis.title.x = element_text(size = 13, face='bold', margin = unit(c(3, 0, 0, 0), "mm")),
#     axis.title.y = element_text(size = 13, face='bold', margin = unit(c(0, 3, 0, 0), "mm")),
#     strip.text =  element_text(size = 14, face = 'bold'),
#     strip.background = element_rect(fill = 'gray95'))
# 
#   setwd('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Temporal_dynamics/Sepparate_files')
#   OUT <- paste(cities[i], '.jpg', sep='')
#   ggsave(OUT, dpi = 300, width = 15, height = 10, unit='cm')
# 
# 
# }
# 







#########

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Visualize relative changes on map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 1) Load Adm-1 shapefile
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#adm <- st_read("GIS/Administrative_divisions/Ukraine/Adm_1/KievOblast_dissolved/ukr_admbnda_adm1_sspe_20220114.shp") 
adm <- st_read("GIS/Administrative_divisions/Ukraine/Adm_1/ukr_admbnda_adm1_sspe_20220114.shp") 

## Filter out Crimea and Sevastopol
adm1 <-  subset(adm, !ADM1_EN %in% c("Avtonomna Respublika Krym","Sevastopilska")) # Remove Sevastopol & Cri
adm1$ADM1_EN <- as.factor(adm1$ADM1_EN)
colnames(adm1)[3] <- "Oblast"
adm1 <- adm1[,c('Oblast')]

### Standardize oblast names to match to the ones in the data
levels(adm1$Oblast)[levels(adm1$Oblast) == "Cherkaska"] <- "Cherkasy"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Chernihivska"] <- "Chernihiv"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Chernivetska"] <- "Chernivtsi"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Dnipropetrovska"] <- "Dnipropetrovsk"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Donetska"] <- "Donetsk"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Ivano-Frankivska"] <- "Ivano-Frankivsk"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Kharkivska"] <- "Kharkiv"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Khersonska"] <- "Kherson"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Khmelnytska"] <- "Khmelnytskyy"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Kirovohradska"] <- "Kirovohrad"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Kyivska"] <- "Kiev"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Kyivska City"] <- "Kiev City"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Luhanska"] <- "Luhansk"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Lvivska"] <- "Lviv"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Mykolaivska"] <- "Mykolayiv"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Odeska"] <- "Odessa"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Poltavska"] <- "Poltava"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Rivnenska"] <- "Rivne"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Sumska"] <- "Sumy"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Ternopilska"] <- "Ternopil"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Vinnytska"] <- "Vinnytsia"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Volynska"] <- "Volyn"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Zakarpatska"] <- "Zakarpattia"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Zaporizka"] <- "Zaporizhzhya"
levels(adm1$Oblast)[levels(adm1$Oblast) == "Zhytomyrska"] <- "Zhytomyr"





## Keep Cimea and Sevastopol (For plotting purposes only)
adm2 <-  subset(adm, ADM1_EN %in% c("Avtonomna Respublika Krym","Sevastopilska")) # Remove Sevastopol & Cri
adm2$ADM1_EN <- as.factor(adm2$ADM1_EN)
colnames(adm2)[3] <- "Oblast"
adm2 <- adm2[,c('Oblast')]

### Change naming for mapping
levels(adm2$Oblast)[levels(adm2$Oblast) == "Avtonomna Respublika Krym"] <- "Crimea"
levels(adm2$Oblast)[levels(adm2$Oblast) == "Sevastopilska"] <- "Sevastopol"





#~~~~~~~~~~~~~~~~~#
### Yearly maps ###
#~~~~~~~~~~~~~~~~~#

## To calculate values during the war period, 
## we have to remove the month of January and all
## dates before 24 February (when war started)

filter(cars, MonthYear == '2-2022' & Threshold == 'Th-45') %>% dplyr::select(Date) %>% table() #To see all dates in february 2022

## Get average car density per Year & Oblast
cars2 <- filter(cars, Year %in% c('2019', '2022') & 
                  Threshold == 'Th-45' &
                  !(MonthYear %in% c("1-2022")) & #Remove January 2022
                  !(Date %in% c("feb_2022_04", "feb_2022_14", "feb_2022_15", "feb_2022_20", "feb_2022_21", "feb_2022_22"))) %>% ##Remove dates before 24. February
  group_by(Year, Oblast) %>%
  summarize(Dcar_min = min(Dcars), 
            Dcar_max = max(Dcars), 
            Dcar_median = median(Dcars, na.rm=T),
            Dcar_mean = mean(Dcars, na.rm=T),
            Dcar_sd = sd(Dcars, na.rm=T),
            nobs = n()) 




cars_change <- cars2 %>%
  arrange(Oblast, Year) %>%
  group_by(Oblast) %>%
  mutate(change_absolute = Dcar_median - Dcar_median[1L],
         change_relative = (Dcar_median - Dcar_median[1L]) / Dcar_median[1L]) %>%
  ungroup() %>%
  data.frame()


## There are sometimes  oblasts which cannot be contrasted (missing data for either 2019 or 2022)
## We have do identify these oblasts and set to NA
nocontrast = cars2 %>%
  arrange(Oblast, Year) %>%
  group_by(Oblast) %>%
  mutate(change_absolute = Dcar_median - Dcar_median[1L],
         change_relative = (Dcar_median - Dcar_median[1L]) / Dcar_median[1L]) %>%
  ungroup() %>%
  data.frame() %>%
  group_by(Oblast) %>%
  summarize(nrow = n()) %>%
  filter(nrow == 1) %>%
  dplyr::select(Oblast)


## Now set these cases do NA
cars_change2 <- filter(cars_change, !(Oblast %in% nocontrast$Oblast) & Year == '2022')
cars_change2$change_relative <- 100 * cars_change2$change_relative #To get values in %


## Grouping on a yearly basis
brks <- cartography::getBreaks(cars_change2$change_relative, method = "quantile", nclass = 9) # When on a yearly basis
#brks <- cartography::getBreaks(cars_change2$change_relative, method = "pretty", nclass = 10) # When on a yearly basis
#brks <- c(-100, -80, -50, -30, -10, 0, 10, 30, 50, 80, 100, 200, 300, 600)

cars_change2$change_grouped <- cut(cars_change2$change_relative, brks,
                                   # labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                                   include.lowest = TRUE)

## Merge the data to shapefile
cars_change2$Oblast <- as.factor(cars_change2$Oblast)
setdiff(cars_change2$Oblast, adm1$Oblast)

cars_change2$geometry <- NULL
cars_change2 <- merge(adm1, cars_change2, by = 'Oblast')


## Now go for the plot
#ncols <- length(brks)-1; mycolors <- colorRampPalette(brewer.pal(9, "RdYlBu"))(ncols) #Yellor-red

adm_coords <- as.data.frame(st_coordinates(st_centroid(adm1)))
adm_coords$Oblast <- adm1$Oblast

adm2_coords <- as.data.frame(st_coordinates(st_centroid(adm2)))
adm2_coords$Oblast <- adm2$Oblast


map <- ggplot() +
        geom_sf(data = st_geometry(adm1), color = "white", fill = 'gray40', lwd=1) +
        geom_sf(data=cars_change2, aes(fill = change_grouped ), color = "white", lwd = 1) +
       # geom_sf(data = st_geometry(adm), color = "white", fill = 'transparent') +
        geom_text_repel(data = adm_coords, aes(X, Y, label = Oblast), 
                        size = 10,
                        colour = "gray30", 
                        bg.color = "white",
                        bg.r = 0.15,
                        hjust = 1,
                        vjust = 0.7,
                        #nudge_x = 0.2,
                        #nudge_y = 0.2,
                        force = 1) +
  
        ## Sevastopol & Crimea
        geom_sf_pattern(data = adm2, pattern = "stripe", fill = "white", col = "gray70", 
                        lwd = 0.2, pattern_density = 0.2, pattern_spacing = 0.01,
                        pattern_colour  = 'gray70') + 
        geom_text_repel(data = adm2_coords, aes(X, Y, label = Oblast), 
                        size = 10,
                        colour = "gray30", 
                        bg.color = "white",
                        bg.r = 0.15,
                        hjust = 1,
                        vjust = 0.7,
                        #nudge_x = 0.2,
                        #nudge_y = 0.2,
                        force = 1) +
        
        #scale_fill_brewer(palette = "BrBG") +
        #scale_fill_brewer(palette = "RdYlBu") +
        scale_fill_brewer(palette = "RdBu", guide = guide_legend(reverse=TRUE)) +
        #scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)+
        #scale_fill_manual(values=mycolors)  +
        #scale_fill_hp(discrete = TRUE, option = "Ravenclaw") +
        #scale_fill_hp(discrete = TRUE, option = "LunaLovegood", direction = -1) +
        #scale_fill_hp(discrete = TRUE, option = "NewtScamander", direction = -1) +
        theme_minimal() +
        labs(fill='Relative change (%)') +  
        theme(axis.text.y = element_text(size = 26),
              axis.text.x = element_text(size = 26),
              axis.title = element_blank(),
              legend.title = element_text(hjust = 0.5, size=28, face="bold"),
              legend.text = element_text(size = 26),
              legend.position = "bottom",
              panel.spacing = unit(c(0, 0, 0 , 0), "cm"))

plot_ratio <- tmaptools::get_asp_ratio(adm)
ggsave('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Spatial_dynamics/Relative_change_map_year.jpg', 
       dpi=300, width = 30*plot_ratio, height = 30, units = 'cm', bg = "white")


## Same info in barplot
#display.brewer.pal(9, 'RdBu')
brewer.pal(9, 'RdBu')

cars_change2$Direction <- as.factor(ifelse(cars_change2$change_relative > 0, 'Increase', 'Decrease'))

# barplot <- cars_change2  %>% 
#            st_drop_geometry() %>%
#             ggbarplot(y = "change_relative", x = "Oblast",
#                       fill = "Direction",           # change fill color by mpg_level
#                       color = "white",            # Set bar border colors to white
#                       #palette = "jco",            # jco journal color palette. see ?ggpar
#                       palette = c('#B2182B','#2166AC'),
# 
#                       alpha = 0.8,
#                       sort.val = "desc",          # Sort the value in descending order
#                       sort.by.groups = FALSE,     # Don't sort inside each group
#                       #x.text.angle = 90,          # Rotate vertically x axis texts
#                       xlab = "Oblast",
#                       ylab = 'Relative change (%)',
#                       legend.title = "Car density",
#                       rotate = T,
#                       ggtheme = theme_minimal()
#             ) +
#               #scale_y_continuous(breaks = c(seq(from = -100, to = 600, by = 100))) +
#               scale_y_continuous(breaks = c(-100, 0, 200, 400, 600)) +
#   
#               geom_hline(yintercept = 0, linetype = 2, color = "gray30", lwd = 1.5) +
#               theme(legend.position = c(0.75, 0.91),
#                     plot.margin = unit(c(t=0,r=3,b=0,l=0), "cm"),
#                     legend.background = element_rect(fill = "white", color = "white"),
#                     legend.text = element_text(size = 32),
#                     legend.title = element_text(size = 34),
#                     axis.text = element_text(size = 34, face = 'bold'),
#                     axis.title = element_text(size = 36, face = 'bold'))

#cars_change3 <- cars_change2
cars_change2$Oblast <- factor(cars_change2$Oblast, cars_change2$Oblast[order(-cars_change2$change_relative)]) # reorder factors

cars_change2  %>% 
  ggplot(aes(x = Oblast, y = change_relative)) +
  geom_bar(stat = "identity",  aes(fill = change_grouped, col=Direction), 
           show_guide = T) +
  coord_flip() + 
  theme_minimal() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9,"RdBu"))(9),
                    guide = guide_legend(reverse=TRUE)) +
  scale_color_manual(name = "Car density", values = c(rep('gray30',2)),
                     guide = guide_legend(override.aes=list(fill=c('#B2182B','#2166AC'),
                                                            linetype = c(1, 1)))) +
  scale_y_continuous(breaks = c(-100, 0, 200, 400, 800)) +
  xlab("Oblast") +
  ylab('Relative change (%)') +
  guides(fill = 'none') +
  geom_hline(yintercept = 0, linetype = 2, color = "gray30", lwd = 1.5) +
  theme(legend.position = c(0.75, 0.91),
        plot.margin = unit(c(t=0,r=3,b=0,l=0), "cm"),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.text = element_text(size = 32),
        legend.title = element_text(size = 34),
        axis.text = element_text(size = 34, face = 'bold'),
        axis.title = element_text(size = 36, face = 'bold'))



ggsave('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Spatial_dynamics/Relative_change_barplot_year.jpg',
       dpi=300, width = 30, height = 40, units = 'cm', bg = "white")






#~~~~~~~~~~~~~~~~~~~~#
### Quarterly maps ###
#~~~~~~~~~~~~~~~~~~~~#

## To calculate values during the war period, 
## we have to remove the month of January and all
## dates before 24 February (when war started)

filter(cars, MonthYear == '2-2022' & Threshold == 'Th-45') %>% dplyr::select(Date) %>% table() #To see all dates in february 2022

## Get average car density per Quarter-Year & Oblast

cars2 <- filter(cars, Year %in% c('2019', '2022') & 
                      Threshold == 'Th-45' &
                        !(MonthYear %in% c("1-2022")) & #Remove January 2022
                        !(Date %in% c("feb_2022_04", "feb_2022_14", "feb_2022_15", "feb_2022_20", "feb_2022_21", "feb_2022_22"))) %>% ##Remove dates before 24. February
  group_by(Year, Quarter, Oblast) %>%
  summarize(Dcar_min = min(Dcars), 
            Dcar_max = max(Dcars), 
            Dcar_median = median(Dcars, na.rm=T),
            Dcar_mean = mean(Dcars, na.rm=T),
            Dcar_sd = sd(Dcars, na.rm=T),
            nobs = n()) 


cars_change <- list()
nocontrast <- list()
nquarter <- nlevels(factor(cars2$Quarter))



for(i in 1:nquarter){
  
  QUARTER <- levels(factor(cars2$Quarter))[i]
  
  cars_change[[i]] <- cars2 %>%
    filter(Quarter == QUARTER) %>%
    arrange(Oblast, Year, as.numeric(as.character(Quarter))) %>%
    group_by(Oblast) %>%
    mutate(change_absolute = Dcar_median - Dcar_median[1L],
           change_relative = (Dcar_median - Dcar_median[1L]) / Dcar_median[1L]) %>%
    ungroup() %>%
    data.frame()
  
  
  
  
  ## There are sometimes some oblasts which cannot be contrasted (missing data for either 2019 or 2022)
  ## We have do identify those oblasts and set to NA
  nocontrast[[i]] = cars2 %>%
    filter(Quarter == QUARTER) %>%
    arrange(Oblast, Year, as.numeric(as.character(Quarter))) %>%
    group_by(Oblast) %>%
    mutate(change_absolute = Dcar_median - Dcar_median[1L],
           change_relative = (Dcar_median - Dcar_median[1L]) / Dcar_median[1L]) %>%
    ungroup() %>%
    data.frame() %>%
    group_by(Oblast) %>%
    summarize(nrow = n()) %>%
    filter(nrow == 1) %>%
    dplyr::select(Oblast)
  
}


## Now set these cases do NA
cars_change2 <- list()

for(i in 1:nquarter){
  cars_change2[[i]] <- filter(cars_change[[i]], !(Oblast %in% nocontrast[[i]]$Oblast) & Year == '2022')
  
}


## Bind them into single row
cars_change2 <- do.call('rbind',cars_change2 )
cars_change2$change_relative <- 100 * cars_change2$change_relative #To get values in %


## Grouping on a yearly basis
brks <- cartography::getBreaks(cars_change2$change_relative, method = "quantile", nclass = 9) # When on a yearly basis
#brks <- quantile(cars_change2$change_relative, probs = seq(0, 1, by = .1))

cars_change2$change_grouped <- cut(cars_change2$change_relative, brks,
                                   # labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                                   include.lowest = TRUE)



## Standardize oblast names
cars_change2$Oblast <- as.factor(cars_change2$Oblast)
setdiff(cars_change2$Oblast, adm1$Oblast)
cars_change2$geometry <- NULL
cars_change2 <- merge(adm1, cars_change2, by = 'Oblast')


## Now go for the plot

#cars_change3 <- full_join(adm, st_drop_geometry(cars_change2), by ='Oblast')

## Vinnytsia values for Q1 are strange -- remove those for plotting
map2 <- ggplot() +
        geom_sf(data = st_geometry(adm1), color = "white",  fill = 'gray40', lwd= 1) +
        geom_sf(data=cars_change2, aes(fill = change_grouped), color = "white",  lwd = 1) +
        scale_fill_brewer(palette = "RdBu", guide = guide_legend(reverse=TRUE)) +
        # geom_text_repel(data = adm_coords, aes(X, Y, label = Oblast), 
        #                 size = 7,
        #                 colour = "gray30", 
        #                 bg.color = "white",
        #                 bg.r = 0.15,
        #                 hjust = 1,
        #                 vjust = 0.7,
        #                 #nudge_x = 0.2,
        #                 #nudge_y = 0.2,
        #                 force = 1) +
  
        ## Sevastopol & Crimea
        geom_sf_pattern(data = adm2, pattern = "stripe", fill = "white", col = "gray70", 
                        lwd = 0.2, pattern_density = 0.2, pattern_spacing = 0.01,
                        pattern_colour  = 'gray70') + 
        theme_minimal() +
        labs(fill='Relative change') +  
        facet_wrap(Quarter ~ ., labeller = as_labeller(c('1' = 'Quarter 1','2' = 'Quarter 2', '3' = 'Quarter 3'))) +
        labs(fill='Relative change (%)') +  
        #scale_color_identity(labels = c(white = "NA"), guide = "legend")+
        theme(axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              axis.title = element_blank(),
              legend.title = element_text(hjust = 0.5, size=20, face="bold"),
              legend.text = element_text(size = 20),
              strip.background =element_rect(fill="gray98"),
              strip.text = element_text(colour = 'gray40', size = 24, face ='bold'),
              legend.position = "bottom")


plot_ratio <- tmaptools::get_asp_ratio(adm)
ggsave('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Spatial_dynamics/Relative_change_map_quarter.jpg', dpi=300, width = 30*plot_ratio, height = 15, units = 'cm', bg = "white")



# ## Same info in barplot
# display.brewer.pal(9, 'RdBu')
# brewer.pal(9, 'RdBu')
# 
# cars_change2$Direction <- as.factor(ifelse(cars_change2$change_relative > 0, 'Increase', 'Decrease'))
# 
# cars_change2  %>% st_drop_geometry() %>%
#   filter(Quarter == '3') %>%
#   #filter(Quarter == '1' & !(Oblast == 'Vinnytsia')) %>%
#   #filter(Quarter == '2' & !(Oblast == 'Kharkiv')) %>%
#   #filter(Quarter == '3') %>%
#   #filter(Quarter == '1' & !(Oblast == 'Vinnytsia')) %>%
#   ggbarplot(y = "change_relative", x = "Oblast",
#             fill = "Direction",           # change fill color by mpg_level
#             color = "white",            # Set bar border colors to white
#             #palette = "jco",            # jco journal color palett. see ?ggpar
#             palette = c('#B2182B','#2166AC'),
#             alpha = 0.8,
#             sort.val = "desc",          # Sort the value in descending order
#             sort.by.groups = FALSE,     # Don't sort inside each group
#             #x.text.angle = 90,          # Rotate vertically x axis texts
#             xlab = "Oblast",
#             ylab = 'Relative change (%)',
#             legend.title = "Car density",
#             rotate = T,
#             ggtheme = theme_minimal()
#   ) +
#   geom_hline(yintercept = 0, linetype = 2, color = "gray30", lwd =0.8) +
#   theme(legend.position = c(0.81, 0.91),
#         legend.background = element_rect(fill = "white", color = "white"),
#         legend.text = element_text(size = 12),
#         legend.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         axis.title = element_text(size = 15))
# 
# ggsave('~/Desktop/Relative_change_barplot_year.jpg', dpi=300, width = 12, height = 20, units = 'cm', bg = "white")
# 



#~~~~~~~~~~~~~~~~~~~#
### plot all in one ###
#~~~~~~~~~~~~~~~~~~~#

# allp <- ggarrange(ggarrange(map, barplot, ncol = 2, labels = c("A", "B")), map2,
#           labels = c("","C"), nrow = 2)
# 
# gridExtra::grid.arrange(map, barplot,map2,
#              layout_matrix = rbind(c(1, 2),
#                                    c(3, 3)))
# 
# png('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Spatial_dynamics/All_in_one.png',
#      res = 300,
#      width = 70, 
#      height = 60,
#      unit='cm',
#      bg = 'white')
# 
# dev.off()
# 


#~~~~~~~~~~~~~~~~~~~#
### Monthly maps ###
#~~~~~~~~~~~~~~~~~~~#
# cars2 <- filter(cars, Year %in% c('2019', '2022') & Threshold == 'Th-15') %>%
#   group_by(Year, Month, Oblast) %>%
#   summarize(Dcar_min = min(Dcars), 
#             Dcar_max = max(Dcars), 
#             Dcar_median = median(Dcars, na.rm=T),
#             Dcar_mean = mean(Dcars, na.rm=T),
#             Dcar_sd = sd(Dcars, na.rm=T),
#             nobs = n()) 
# 
# 
# cars_change <- list()
# nocontrast <- list()
# nmonth <- nlevels(factor(cars2$Month))
# 
# for(i in 1:nmonth){
#   
#   MONTH <- levels(factor(cars2$Month))[i]
#   
#   cars_change[[i]] <- cars2 %>%
#     filter(Month == MONTH) %>%
#     arrange(Oblast, Year, as.numeric(as.character(Month))) %>%
#     group_by(Oblast) %>%
#     mutate(change_absolute = Dcar_median - Dcar_median[1L],
#            change_relative = (Dcar_median - Dcar_median[1L]) / Dcar_median[1L]) %>%
#     ungroup() %>%
#     data.frame()
#   
#   
#   ## There are sometimes some oblasts which cannot be contrasted (missing data for either 2019 or 2022)
#   ## We have do identify those oblasts and set to NA
#   nocontrast[[i]] = cars2 %>%
#     filter(Month == MONTH) %>%
#     arrange(Oblast, Year, as.numeric(as.character(Month))) %>%
#     group_by(Oblast) %>%
#     mutate(change_absolute = Dcar_median - Dcar_median[1L],
#            change_relative = (Dcar_median - Dcar_median[1L]) / Dcar_median[1L]) %>%
#     ungroup() %>%
#     data.frame() %>%
#     group_by(Oblast) %>%
#     summarize(nrow = n()) %>%
#     filter(nrow == 1) %>%
#     select(Oblast)
#   
# }
# 
# 
# ## Now set these cases do NA
# cars_change2 <- list()
# 
# for(i in 1:nmonth){
#   cars_change2[[i]] <- filter(cars_change[[i]], !(Oblast %in% nocontrast[[i]]$Oblast) & Year == '2022')
#   
# }
# 
# 
# ## Bind them into single row
# cars_change2 <- do.call('rbind',cars_change2 )
# 
# 
# ## Grouping on a yearly basis
# brks <- cartography::getBreaks(cars_change2$change_relative, method = "quantile", nclass = 9) # When on a yearly basis
# 
# 
# cars_change2$change_grouped <- cut(cars_change2$change_relative, brks,
#                                    # labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
#                                    include.lowest = TRUE)
# 
# 
# 
# ## Standardize oblast names
# cars_change2$Oblast <- as.factor(cars_change2$Oblast)
# setdiff(cars_change2$Oblast, adm$Oblast)
# cars_change2$geometry <- NULL
# cars_change2 <- merge(adm, cars_change2, by = 'Oblast')
# 
# 
# ## Now go for the plot
# ggplot() +
#   geom_sf(data = st_geometry(adm), color = "white",  fill = 'gray40') +
#   geom_sf(data=cars_change2, aes(fill = change_grouped ), color = "white") +
#   #scale_fill_brewer(palette = "BrBG") +
#   #scale_fill_brewer(palette = "RdYlBu") +
#   scale_fill_brewer(palette = "RdBu") +
#   #scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)+
#   #scale_fill_manual(values=mycolors)  +
#   #scale_fill_hp(discrete = TRUE, option = "Ravenclaw") +
#   #scale_fill_hp(discrete = TRUE, option = "LunaLovegood", direction = -1) +
#   #scale_fill_hp(discrete = TRUE, option = "NewtScamander", direction = -1) +
#   theme_minimal() +
#   labs(fill='Relative change') +  
#   facet_wrap(Month ~ .) +
#   theme(axis.text.y = element_text(size = 12),
#         axis.text.x = element_text(size = 12),
#         axis.title = element_text(size=13,face="bold"),
#         plot.title = element_text(hjust = 0.5, size=15, face="bold"),
#         strip.text.y = element_text(size = 14, face="bold"),
#         legend.position = "top")
# 
# 
# 




########################################

### Additional stuff
## Average car density per year (barplot)


# cars %>%
#   filter(Threshold == 'Th-45') %>%
#   #group_by(MonthYear, City, Threshold) %>%
#   group_by(Year,City, Threshold) %>%
#   summarize(Avg_cars = median(Dcars, na.rm=T),
#             sd = sd(Dcars, na.rm=T),
#             Region = paste(unique(Region))) %>%
#   
#   # Filter 5-top cities within each region
#   #filter(Region == 'West' & City %in% c('Chernivtsi','Ivano-Frankivsk', 'Lviv','Shehyni','Uzhhorod', 'Ternopil' )) %>%
#   #filter(Region == 'Central' & City %in% c('Kyiv','Odessa', 'Mykolaiv', 'Kherson', 'Chernihiv', 'Cherkasy')) %>%
#   #filter(Region == 'East'& City %in% c('Donetsk','Kharkiv','Luhansk','Mariupol','Alchevsk', 'Melitopol')) %>%
#   filter(City %in% c('Donetsk','Kharkiv','Mariupol')) %>% #Only use when aggregated on yearly level
#   
#   
#   #ggplot(aes(x=MonthYear, y = Avg_cars, fill = Threshold)) +
#   ggplot(aes(x=Year, y = Avg_cars, fill = Threshold)) +
#   
#   geom_bar(stat="identity",
#            position="dodge", alpha = 0.8) +
#   geom_vline(xintercept = 38, color = "darkred", size=0.8) +
#   scale_x_discrete(drop=FALSE) +
#   
#   facet_wrap(City ~ ., scales = 'free_y', ncol = 2) +
#   #scale_color_manual(values=c("cyan4", "#E7B800")) +
#   #scale_color_manual(values=c("#1d72b4", "#e89d41")) +
#   scale_fill_manual(values=c("cyan4", "#e89d41")) +
#   #scale_color_hp(house = "LunaLovegood", discrete=T) + 
#   theme_bw() +
#   ylab('Avergage Car Density') +
#   theme(legend.position = 'top',
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title = element_text(size = 13, face='bold'),
#         strip.text =  element_text(size = 14, face = 'bold'))
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~
# ## Check for statistical significance
# tmp <- filter(cars, City == 'Mariupol' & Threshold == 'Th-45')
# table(tmp$Year)
# 
# res <- glm(Dcars ~ Year, data = tmp, family = gaussian)
# summary(res)
# #plot(res)
# 
# # res2 <- aov(Dcars ~ Year, data = tmp)
# # tukey.test <- TukeyHSD(res2)
# #~~~~~~~~~~~~~~~~~~~~~~~
# 



