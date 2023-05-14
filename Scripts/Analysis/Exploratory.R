#######################################################################
#                                                                     #
#                     Data spatial & temporal coverage                #
#                                                                     #
#######################################################################




#~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(RColorBrewer)
library(pals)
library(ggh4x)
#library(plot.matrix)

#~~~~~~~~~~~~
# Functions
#~~~~~~~~~~~~
everysecond <- function(x){
  x <- unique(x)
  x[seq(2, length(x), 2)] <- ""
  x
}

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


#~~~~~~~~~~~
# 2) Plots
#~~~~~~~~~~~


# 2.1) No. images per hour of the day
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(cars, aes(x = as.factor(Hour))) +
  geom_bar(fill = 'cyan4', alpha = 0.7) +
  #scale_y_discrete(expand = c(0, 0)) +
  theme_bw() +
  ylab('No. images') + xlab('Hour of the day') +
  #geom_text(aes(label = nobs), nudge_y = -15,  size= 4.5, col = 'gray30') + 
  theme(axis.title.y  = element_text(size = 15, face = 'bold', margin = unit(c(0, 3, 0, 0), "mm")), 
        axis.title.x  = element_text(size = 15, face = 'bold', margin = unit(c(0, 3, 0, 0), "mm")), 
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold'),
        panel.border = element_blank(),
        #axis.line = element_line(color = 'black'),
        legend.position = "none")

ggsave('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Exploratory/hour_of_dat.jpg',
       dpi = 300, width = 20, height = 20, unit='cm',
       bg = 'white')



# 2.2) No. images per city
#~~~~~~~~~~~~~~~~~~~~~~~~~~
dobs <- cars %>% 
  filter(Threshold == "th_045_carclass_18") %>%
  group_by(City) %>% 
  dplyr::summarize(nobs = n()) %>%
  data.frame()


## Define color palette
#cols = rev(brewer.pal(n = 11, name = 'RdBu'))
nb.cols = 60 #60 obtasts
cols = rev(colorRampPalette(brewer.pal(11, "BrBG"))(nb.cols))

## Now go for the plot
ggplot(dobs, aes(x = reorder(City, -nobs), y = nobs,  fill= nobs)) +
  geom_col(alpha = 1, colour="gray95") + 
  #scale_fill_gradientn(colours = cols, limits=c(1, 36), na.value="gray95") + 
  scale_fill_viridis(option = 'G', direction = -1) +
  #coord_flip() +
  scale_y_discrete(expand = c(0, 0)) +
  theme_bw() +
  ylab('No. images') +
  geom_text(aes(label = nobs), nudge_y = 1.6, vjust = 1, size= 4.5, col = 'gray30') + 
  theme(axis.title.y  = element_text(size = 13, face = 'bold', margin = unit(c(0, 3, 0, 0), "mm")), 
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjus = 1, vjus =0.5, face = 'bold'),
        panel.border = element_blank(),
        #axis.line = element_line(color = 'black'),
        legend.position = "none")

ggsave('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Exploratory/No_images_per_city.jpg',
       dpi = 300, width = 30, height = 15, unit='cm')



# 2.3) Spatio-temporal data availability
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Get total no. of images per month for each city
dobs2 <- cars %>% 
  filter(Threshold == "th_045_carclass_18") %>%
  group_by(City, Year, Month) %>% 
  dplyr::summarize(nobs = n()) %>%
  mutate(YearMonth = factor(paste(Month, Year, sep="-"))) %>%
  data.frame()


## Set time levels to cronological order
colorder <- c(paste(1:12, 2019, sep="-"),
              paste(1:12, 2020, sep="-"),
              paste(1:12, 2021, sep="-"),
              paste(1:9, 2022, sep="-"))

dobs2$YearMonth <- factor(dobs2$YearMonth, 
                          levels=colorder)


## Transform to wide-format to retrieve time-stamps with zero observations
dobs_wide <- tidyr::spread(dobs2, City, nobs)

dobs_wide[is.na(dobs_wide)] <- 0 #Time stamps for which we don't have observation

## Now transform back to long format
#dobs_wide <- dobs_wide %>% arrange(YearMonth, descending = T) ## Arrange by increase month-year
dobs2b <- tidyr::gather(dobs_wide, City, nobs, Alchevsk:Zhytomyr)



## Reorder matrix with cities ranked from west-east

## Source script that classifies the city/oblast according to its geographical position
### Define to which spatial resolution the data should be evaluated
AGGREGATE <- c('City', 'Oblast')[1]

source('~/Library/CloudStorage/OneDrive-HamadbinKhalifaUniversity/Projects/Ukraine/Imagery_EDA/WorldPop_coverage/CityOblast_classification_west-east.R')

classification <- aoi_classified %>% 
  arrange(X, Y) %>%
  select(City, Region) %>%
  st_drop_geometry() %>%
  data.frame()

classification$City <- as.factor(classification$City)
setdiff(classification$City, dobs2b$City)

### Standadize city names
levels(classification$City)[levels(classification$City) == 'Velyki_Kopani'] <- 'Velyki-Kopani'
levels(classification$City)[levels(classification$City) == 'Bila_Tserkva'] <- 'Bila-Tserkva'
levels(classification$City)[levels(classification$City) == 'Zaporizhia'] <- 'Zaporizhzhia'
setdiff(classification$City, dobs2b$City)


### Remove non-matching cities
rmv <- setdiff(classification$City, dobs2b$City)
classification <- filter(classification, !(City %in% rmv))
dobs2b <- merge(dobs2b, classification, by ='City')


dobs2b$City <- factor(dobs2b$City, 
                          levels=classification$City)


west <- classification %>% filter(Region == 'West') %>% select(City)
central <- classification %>% filter(Region == 'Central') %>% select(City)
east <- classification %>% filter(Region == 'East') %>% select(City)

westr <- data.frame(xmin='Uzhhorod', xmax='Khmelnytskyi', ymin=-Inf, ymax=Inf)
centralr <- data.frame(xmin='Reni', xmax='Katerynivka', ymin=-Inf, ymax=Inf)
eastr <- data.frame(xmin='Poltava', xmax='Milove', ymin=-Inf, ymax=Inf)



dobs2b$Region <- factor(dobs2b$Region,
                          levels=c('West','Central', 'East'))

dobs2b <- dobs2b %>%
  filter(!(City %in% c('Berdychiv', 'Chortkiv', 'Hremiach', 'Kozyatyn', 'Udobne'))) #We remove these cities because they only have 1 image throghout the time series (see previous plot)

dobs2b[,c('City', 'Region')] <- lapply(dobs2b[,c('City', 'Region')], factor )

## Define color palette
cols = rev(brewer.pal(n = 7, name = 'RdBu'))

dobs2b %>%  
  ggplot( aes(x=interaction(City,Region), y=YearMonth, fill=nobs, label=nobs))+
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(col='white') +
  #scale_fill_gradientn(colours=rev(ocean.ice(7)), limits=c(1, 7), na.value="gray95") +
  scale_fill_gradientn(colours = cols, limits=c(1, 7), na.value="gray95") + 
  #scale_fill_gradientn(colours = c("gray95", "blue", "red"), values = c(0,0.2,0.8)) +
  #scale_fill_gradient(low="blue", high="red", limits=c(1, 7),na.value="white") +
  
  theme_bw() +
  ylab('Time') +  xlab('') + 
  labs(fill='No. \nImages') +  
  scale_x_discrete(guide = "axis_nested") +
  scale_y_discrete(expand = c(0, 0), limits=rev, labels = everysecond(rev(colorder)))  +
  # annotate(geom = "text", x = 10, y = 0, label = 'West', size = 6) +
  
  theme( axis.text.x = element_text(angle = 90,size = 12, vjust = 0.5, hjust = 1, color = c(rep('#2B9EB3', dim(west)[1]), rep('#EFAB1B', dim(central)[1]), rep('#A28377', dim(east)[1]))),
         axis.text.y = element_text(size = 12),
         axis.title.x = element_text(size = 13, face='bold', margin = unit(c(3, 0, 0, 0), "mm")),
         axis.title.y = element_text(size = 13, face='bold', margin = unit(c(0, 3, 0, 0), "mm")),
         axis.ticks = element_line(colour = "gray40"),
         legend.title.align=0.5,
         legend.text = element_text(size = 11),
         legend.title = element_text(size = 12, face = 'bold'),
         #legend.position = 'bottom',
         ggh4x.axis.nestline.x = element_line(linewidth = 0.5),
         
         ggh4x.axis.nesttext.x = element_text(colour = "gray40", angle = 360, hjust = 0.5, face ='bold',
                                              margin = unit(c(3, 0, 0, 0), "mm")))

ggsave('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Exploratory/No_images_per_time.jpg',
       dpi = 300, width = 25, height = 20, unit='cm')

