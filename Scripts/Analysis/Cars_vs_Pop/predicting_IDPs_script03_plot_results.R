#######################################################################
#                                                                     #
#      Evaluate relationship between No. people and No. of cars       #
#                                                                     #
#                     - Part 3: Plot the results -                    #
#                                                                     #
#######################################################################



# The following script evaluates the predicted population from both
# ratio and GAM method (see predicting_IDPs_script02a_ratio_method.R & predicting_IDPs_script02b_GAM_method.R)


## Please update input & output folder paths accordingly 

## Code written by: Marie-Christine Rufener < macrufener@gmail.com >
## Last update: June 2023


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~~
library(ggplot2)
library(ggpattern)
library(viridis)
library(dplyr)
library(scales)
library(ggsci)
library(sf)
library(mgcv)
library(mgcViz)
library(ggpubr)


## Standard working directory
#setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GitHub/IDP_UKR/") 
setwd("/Users/mcruf/Documents/IDP_UKR")

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Load the data set
#~~~~~~~~~~~~~~~~~~~~~~~~


# 1.1) Ratio prediction method
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ratio <- readRDS("Data/Predictions/PopPred_ratio_method.rds")

ratio <- do.call("rbind", ratio)
ratio$Method <- "Ratio"


# 1.2) GAM prediction method
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gam <- readRDS("Data/Predictions/PopPred_gam_method.rds")
gam <- do.call("rbind", gam)
gam$Method <- "GAM"


# 1.3) Bind the two datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
datall <- rbind(ratio,gam)

# Convert appropriate variables to factor 
datall$grid_id <- as.factor(datall$grid_id)
datall$City <- as.factor(datall$City)
datall$Year <- as.factor(datall$Year)
datall$Month <- as.factor(datall$Month)

# Standardize city names
levels(datall$City)[levels(datall$City) == "Bila_Tserkva"] <- "Bila-Tserkva"
levels(datall$City)[levels(datall$City) == "Zaporizhia"] <- "Zaporizhzhia"



# 1.4) AOI polygons of the cities
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## List all files in the folders
fp <- list.files(path = "GIS/SatelliteImage/Img_AOI",
                 recursive = TRUE,
                 pattern = "\\.shp$",
                 full.names = TRUE)


## Load the files & set them into a list
aoipoly <- list()
for(i in seq_along(fp)) {
  aoipoly[[i]] <- st_read(fp[i])
  names(aoipoly)[[i]] <- basename(dirname(fp))[i]
  aoipoly[[i]]$City <- as.factor(basename(dirname(fp))[i])
  aoipoly[[i]] <- st_transform(aoipoly[[i]],crs = 6381) # Project to CRS with units set in meters (to make the grid in the units of meters)
}


## Combine all polygons into a single shapefile 
aoipolyall <- do.call(what = sf:::rbind.sf, args=aoipoly)
aoipolyall$City <- factor(aoipolyall$City)


# Standardize city names
levels(aoipolyall$City)[levels(aoipolyall$City) == "Bila_Tserkva"] <- "Bila-Tserkva"
levels(aoipolyall$City)[levels(aoipolyall$City) == "Zaporizhia"] <- "Zaporizhzhia"




## Remove any non-matching cities
setdiff(aoipolyall$City, levels(datall$City))
rmv <- setdiff(aoipolyall$City, levels(datall$City))

aoipolyall <- filter(aoipolyall, !(City %in% rmv))
aoipolyall$City <- factor(aoipolyall$City)

rm(aoipoly)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Some additional data manipulation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.1) Convert data to UTM projection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CRS <- st_crs(aoipolyall)$epsg
datall <- st_transform(datall, CRS)


# 2.2) Attach AOI area to dataframe
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Specific to each city

# 2.2.1) Get total area of each city AOI
aoipolyall$AreaAOI_km2 <- as.numeric(round(st_area(aoipolyall)/1e6,1))

## Now merge info to dataframe
setdiff(levels(aoipolyall$City), levels(datall$City)) #Sanity check
aoipolyall2 <- st_drop_geometry(aoipolyall)
datall <- merge(datall, aoipolyall2[,c("City", "AreaAOI_km2")], by = "City")



# 2.3) Calculate area for each month-city
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
datalla <- datall %>% 
           filter(Year == "2019") #Only 2019 (baseline)

datallb <- datall %>% 
           group_by(City, Year, Month, Method) %>% 
           filter(!(Year == "2019")) %>% #All years but 2019 (baseline)
           mutate(AreaAOI_km2 = round(sum(st_area(geometry))/1e6,1))

datall <- rbind(datalla, datallb) #Put all back

rm(datalla); rm(datallb)


# 2.4) Calculate percentage of area covered
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dfg <- datall %>% 
       group_by(City, Year, Month, Method) %>%
       summarize(Totpop = sum(Npop),
                  AreaAOI_km2 = paste(unique(AreaAOI_km2))) 
dfg$AreaAOI_km2 <- as.numeric(as.character(dfg$AreaAOI_km2))
dfg$Method <- as.factor(dfg$Method)


dfg2 <- dfg %>%
        group_by(City) %>%
        slice(2:n()) %>% #Remove one of the baselines (there is a baseline for each method, with same results)
        #ungroup %>%     
        #group_by(City, Year, Month, Method) %>%  
        #arrange( City, Year, Month, Method)  %>% 
        mutate(AreaCovered = (AreaAOI_km2 * 100 / AreaAOI_km2[1])) 
        #%>%filter(City == "Kyiv") 


## Some additional manupulation
dfg2$Month <- stringr::str_to_title(dfg2$Month); dfg2$Month <- as.factor(dfg2$Month)

## Rearrange months by chronological order
dfg2$Month <- factor(dfg2$Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))

dfg2$Contrast <- ifelse(dfg2$Year == '2019', 'Pre-war',
                 ifelse(dfg2$Year == '2020', 'Covid', 'War'))


#~~~~~~~~~~~~~~~~~~~~
# 3) Go for the plot
#~~~~~~~~~~~~~~~~~~~~

tmp <- dfg2 %>%
  #filter(City %in% c("Uzhhorod","Kyiv", "Mariupol")) %>%
  #filter(City %in% levels(City)[1:8]) %>%
  #filter(City %in% levels(City)[9:16]) %>%
  #filter(City %in% levels(City)[c(17:18,20:23,25:26)]) %>% # Avoiding kyiv and Mariupol
  #filter(City %in% levels(City)[27:34]) %>%
  filter(City %in% levels(City)[c(35:38,40:43)]) %>%
  
  mutate(Method = if_else(Month == 'Baseline' & 
                            Method == 'Ratio', 'Base', Method)) %>%
  group_by(City, Month, Contrast) %>%
  mutate(Month = replace(as.character(Month), Contrast=='Pre-war', "Baseline")) %>%
  arrange(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
  mutate(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
  arrange(Contrast = factor(Contrast, levels = c('Pre-war', 'Covid', 'War'))) %>%
  mutate(Contrast = factor(Contrast, levels = c('', 'Covid', 'War'))) %>%
  #tidyverse::drop_na(Contrast) %>%
  as_tibble() %>%
  group_by(City) %>%
  mutate(
    Diff = Totpop - Totpop[1],
    Percent = round(Diff/Totpop[1]*100,3),
    Direction = ifelse(Percent >0, 'Increase', 'Decrease'))  %>%
  mutate(Direction = ifelse(Percent == 0, NA_integer_, Direction)) %>%
  mutate_at(c('Percent'), ~na_if(., 0)) %>%
  as_tibble() %>%
  group_by(City) %>%
  mutate(
    Diff = Totpop - Totpop[1],
    Percent = round(Diff/Totpop[1]*100,3),
    Direction = ifelse(Percent >0, 'Increase', 'Decrease'),
    Totpop2 = Totpop/1000)  %>%
  mutate(Direction = ifelse(Percent == 0, NA_integer_, Direction)) %>%
  mutate_at(c('Percent'), ~na_if(., 0))



tmp1 <- tmp %>% filter(Method == 'Base')
tmp2 <- tmp %>% filter(!(Method %in% 'Base'))





p1 <- tmp2 %>%
  filter(!is.na(Contrast)) %>%
  ggplot(aes(x=Month, y=Totpop2)) +
  #facet_wrap(~ factor(City, levels = c('Uzhhorod', 'Kyiv', 'Mariupol')), ncol=1, scales="free_y") +
  facet_wrap(~ City,  ncol=1, scales="free_y") +
  geom_hline(data = plyr::ddply(tmp1, 'City', summarize, baseline = unique(Totpop2)), aes(yintercept = baseline), col = 'darkorange', linetype='dashed', linewidth = 1.2)+
  geom_bar_pattern(aes(fill = Contrast, col = Contrast, pattern = Method),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.015,
                   pattern_key_scale_factor = 0.6,
                   stat="identity",
                   position=position_dodge2(preserve = 'single', padding = 2),
                   width = 1,
                   #position="dodge", 
                   alpha = 0.5,
                   lwd = 0.5
  ) +
  
  geom_text_repel(aes(label = round(Percent, 1),  group = Contrast, col = Direction),
                  position = position_dodge2(width = 1, preserve = "single"), vjust=-0.5,
                  size = 6, fontface = 'bold',
                  show.legend = F) +
  # geom_point(aes(x = Month, y = Totpop2, shape = Method),
  #            size = 5,
  #           position = position_dodge2(width = 1, preserve = "single"))+
  
  scale_pattern_manual(values = c(Ratio = "stripe", GAM = "none", Base = "none"),
                       breaks = c('Ratio', 'GAM')) +
  
  guides(pattern = guide_legend(override.aes = list(fill = "white", size=10)),
         fill = guide_legend(title = "Period", override.aes = list(pattern = "none", size=10, shape = 21))) +
  scale_y_continuous(expand=expansion(mult = c(0,0.1)))+
  # scale_y_continuous(labels = comma,
  #                    sec.axis = sec_axis(trans = ~(. / max(tmp2$Totpop2)), name = "AOI coverage", labels = scales::percent)) +
  scale_fill_manual(name = "Contrast", values=c("gray70",'cyan4')) +
  #scale_color_manual(name = "Contrast", values=c("cyan4",'gray70',"darkorange")) +
  scale_color_manual(name = "Direction", values=c("darkred",'dodgerblue4')) +
  theme_bw() +
  ylab('No. people (in thousands)') + xlab('') +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = 22,face = 'bold'),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 28, face = "bold", vjust = 5),
        panel.border = element_blank(),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.margin=margin(t=35),
        legend.title = element_text(face = 'bold', size = 20),
        legend.text = element_text(size = 18),
        legend.spacing.x = unit(2.5, "cm"),
        #legend.box="vertical", 
        plot.margin = unit(c(t=1,b=1,r=0,l=1), "cm"),
        panel.spacing = unit(1, units = "cm"), # removes space between panels
        strip.placement = "outside", # moves the states down
        strip.text = element_text(size=18, face = 'bold', colour = 'gray20'),
        strip.background = element_rect(fill = 'gray98', colour='gray70'),
        
        
        #panel.grid.major = element_line(colour = grey(0.4),linetype = 3 )
        #axis.line = element_line(color = 'black'),
        
  )

# OUTFILE <- "~/Downloads/test_plot.jpg"
# ggsave(filename = OUTFILE, 
#        plot = p1,
#        dpi = 300, width = 30, height = 45, unit='cm')


p2 <- tmp2 %>%
  filter(!is.na(Contrast)) %>%
  ggplot(aes(x=Month, y = AreaCovered)) +
  geom_point(aes(fill = Contrast), col = 'gray40', alpha = 0.5, size = 10, shape = 21, stroke = 2) +
  geom_segment(aes(x = Month, xend = Month, y = 0, yend= 100),
               linetype = 'dotted',
               size = 0.1) +
  #coord_flip()
  #facet_wrap(~ factor(City, levels = c('Uzhhorod', 'Kyiv', 'Mariupol')), ncol=1, scales="free_y") +
  facet_wrap(~ City,  ncol=1, scales="free_y") +
  scale_y_continuous(position = "right") +
  guides(fill = guide_legend(override.aes = list(pattern = "none", size=10))) +
  scale_fill_manual(name = "Contrast", values=c("gray70",'cyan4')) +
  scale_color_manual(name = "Contrast", values=c("gray70",'cyan4')) +
  ylab('Area covered (%)\n') + xlab('') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 22, face = 'bold'),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 28, face = "bold", vjust = 5),
        panel.border = element_blank(),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.margin=margin(t=35),
        legend.title = element_text(face = 'bold', size = 20),
        legend.text = element_text(size = 18),
        legend.spacing.x = unit(2.5, "cm"),
        #legend.box="vertical", 
        plot.margin = unit(c(t=1,b=1,r=0,l=0.8), "cm"),
        panel.spacing = unit(1, units = "cm"), # removes space between panels
        strip.placement = "outside", # moves the states down
        strip.text = element_text(size=18, face = 'bold', colour = 'gray20'),
        strip.background = element_rect(fill = 'gray98', colour='gray70'),
        panel.grid.major.x = element_line(linewidth = 0.5,
                                          linetype = 3,
                                          color = 'grey'),
        panel.grid.major.y = element_line(linewidth = 0.5,
                                          linetype = 3,
                                          color = 'grey'),
        panel.grid.minor.y = element_blank())



OUTFILE2 <- "~/Downloads/test_plot2.jpg"

ggsave(filename = OUTFILE2, 
       plot = p2,
       dpi = 300, width = 30, height = 45, unit='cm')




p3<-ggarrange(p1, p2, nrow = 1, common.legend = TRUE,
              legend = "bottom")
OUTFILE3 <- "~/Downloads/test_plot3.jpg"

### Main plot
# ggsave(filename = OUTFILE3, 
#        plot = p3,
#        dpi = 300, width = 40, height = 40, unit='cm')


### Suppl. plots
ggsave(filename = OUTFILE3, 
       plot = p3,
       dpi = 300, width = 50, height = 70, unit='cm')



## Deprecated
# tmp <- dfg2 %>%
#   filter(City == "Kyiv")
# 
# p1 <- tmp %>%
#       mutate(Method = if_else(Month == 'Baseline' & 
#                                 Method == 'Ratio', 'Base', Method)) %>%
#       group_by(Month, Contrast) %>%
#       mutate(Month = replace(as.character(Month), Contrast=='Pre-war', "Baseline")) %>%
#       arrange(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
#       mutate(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
#       arrange(Contrast = factor(Contrast, levels = c('Pre-war', 'Covid', 'War'))) %>%
#       mutate(Contrast = factor(Contrast, levels = c('Pre-war', 'Covid', 'War'))) %>%
#       
#       as_tibble() %>%
#       mutate(
#         Diff = Totpop - Totpop[1],
#         Percent = round(Diff/Totpop[1]*100,3),
#         Direction = ifelse(Percent >0, 'Increase', 'Decrease'))  %>%
#       mutate(Direction = ifelse(Percent == 0, NA_integer_, Direction)) %>%
#       mutate_at(c('Percent'), ~na_if(., 0)) %>%
#       
#       
#       ggplot(aes(x=Month, y=Totpop)) +
#       geom_bar_pattern(aes(fill = Contrast, col = Contrast, pattern = Method),
#                        color = "black", 
#                        pattern_fill = "black",
#                        pattern_angle = 45,
#                        pattern_density = 0.05,
#                        pattern_spacing = 0.015,
#                        pattern_key_scale_factor = 0.6,
#                        stat="identity",
#                        position=position_dodge2(preserve = 'single', padding = 2),
#                        #width = 2,
#                        #position="dodge", 
#                        alpha = 0.5,
#                        lwd = 0.5) +
#       
#       geom_text(aes(label = round(Percent, 1),  group = Contrast),
#                 position = position_dodge2(width = 1, preserve = "single"), vjust=-0.5,
#                 size = 4, fontface = 'bold', col = 'gray20') +
#       
#       scale_pattern_manual(values = c(Ratio = "stripe", GAM = "none", Base = "none"),
#                            breaks = c('Ratio', 'GAM')) +
#       
#       guides(pattern = guide_legend(override.aes = list(fill = "white")),
#              fill = guide_legend(override.aes = list(pattern = "none"))) +
#       
#       scale_y_continuous(labels = comma,
#                          sec.axis = sec_axis(trans = ~(. / max(tmp$Totpop)), name = "AOI coverage", labels = scales::percent)) +
#       scale_fill_manual(name = "Contrast", values=c("cyan4",'gray70',"darkorange")) +
#       scale_color_manual(name = "Contrast", values=c("cyan4",'gray70',"darkorange")) +
#       theme_bw() +
#       ylab('No. people') + xlab('') +
#       theme_bw() +
#       theme(axis.text.x = element_text(size = 25, face = 'bold'),
#             axis.text.y = element_text(size = 25),
#             axis.title.y = element_text(size = 28, face = "bold", vjust = 5),
#             panel.border = element_blank(),
#             plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
#             legend.position = "top",
#             plot.margin = unit(c(t=1,b=0,r=0,l=2), "cm"),
#             panel.spacing = unit(0, units = "cm"), # removes space between panels
#             strip.placement = "outside", # moves the states down
#             strip.background = element_rect(fill = "white"))
# 
# OUTFILE <- "~/Downloads/test.jpg"
# 
# ggsave(filename = OUTFILE, 
#        plot = p1,
#        dpi = 300, width = 30, height = 20, unit='cm')


# In order to evaluate the relative change in total population size,
# we have to do so by standardizing the spatial extents either between
# the matching grid cells between the baseline and the individual months of the year of interest, 
# or between matching grid cells between all periods under concern (2019 including all months of the year of interest).

# The first approach can be termed as 'dynamic baseline', because the total baseline population size will be
# calculated for each individual month of the year of interest. Because each individual month can cover different
# spatial extents of the AOI, total baseline population will also change because we match the 2019 grid cells to the
# grid cells of the given month of the interested year. This approach has the advantage of preserving the full spatial extent
# of the month-year of interest. However, it's interpretation is more difficult, because it shows different baseline population sizes, one for each month.
# It is more intutive to think the baseline population size as static, instead of dynamic.

# The second approach can be termed as 'static baseline', because in this case the total baseline population size
# will be cacluated based on matching grid cells between the baseline year and all months of the considered cells.
# As such, we will have a unique baseline total population size, whereby changes in monhtly poulation sizes will
# be calculated relative to the static baseline population. The advantage of this approach is that interpretation of the 
# results are more intutitive and easier to understand, as relative changes in population size will be based on the same
# spatial extent throught the entire evaluated period. The disadvantage is that whenever a given month from the year of interest
# (or even the baseline year) covers a much smaller extent than all the other period, we will be obliged to evaluate the result
# on the smallest overlapping spatial extent. Thus, we risk to loose important information for important areas og a given city.

# Here we will evaluate everyhing for the 'static' baseline approach.
# Code for the 'dynamic' baseline approach is provided at the very end of this script,
# in case it could be useful at some point.


# 2.1) Filter data for matching grid cells across all periods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I.e., 2019, 2020 (+ months therein), & 2022 (+ months therein).
# This is the 'static' baseline approach


ratio2 <- list()

for(i in seq_along(ratio)){
  ratio2[[i]] <- ratio[[i]] %>%
                 group_by(grid_id) %>%
                 mutate(duplicate_count = n(),
                         YearMonth = factor(paste(Year, Month, sep = ""))) %>% 
                  filter(duplicate_count == nlevels(YearMonth)) 
                #%>% distinct(grid_id, .keep_all = TRUE) 
}


## To see the progress
ratio2[[8]] %>% #Donetsk
  filter(Year == '2019') %>%
  #filter(Year == '2020' & Month == 'mar') %>%
  #filter(Year == '2022' & Month == 'aug') %>%
  mapview(zcol = 'Npop') 

ratio[[8]] %>% #Donetsk
  #filter(Year == '2019') %>%
  #filter(Year == '2020' & Month == 'jul') %>% #mar, apr, jul
  filter(Year == '2022' & Month == 'sep') %>% #feb, mar, apr, jun, jul, sep
  summarize(ncell = nlevels(factor(grid_id)))
  mapview(zcol = 'Npop')


  
donetsk <- ratio[[8]] %>% #Donetsk
    filter(Year == '2019')


donetsk2 <- ratio2[[8]] %>% #Donetsk
  filter(Year == '2019')


mapview(donetsk, alpha.regions = 0) +
  mapview(donetsk2, zcol = 'Npop')


summarize(ncell = nlevels(factor(grid_id)))

test <- ratio[[19]] %>%
  group_by(grid_id) %>%
  mutate(duplicate_count = n(),
         YearMonth = factor(paste(Year, Month, sep = ""))) %>% 
  filter(duplicate_count == nlevels(YearMonth)) 
#%>% distinct(grid_id, .keep_all = TRUE) 


test %>%
  filter(Year == '2019') %>%
  #filter(Year == '2020' & Month == 'mar') %>%
  #filter(Year == '2022' & Month == 'aug') %>%
  mapview(zcol = 'Npop')


test2 <- test %>%
         group_by(Year, Month) %>%
        st_drop_geometry() %>%
        summarize(Totpop = round(sum(Npop)),
                    Year = paste(unique(Year)),
                    Month = paste(unique(Month)),
                    City = paste(unique(City)))



test2$Month <- stringr::str_to_title(test2$Month); test2$Month <- as.factor(test2$Month)

## Rearrange months by chronological order
test2$Month <- factor(test2$Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))

test2$Contrast <- ifelse(test2$Year == '2019', 'Pre-war',
                       ifelse(test2$Year == '2020', 'Covid', 'War'))






test2 %>%
  group_by(Month, Contrast) %>%
  mutate(Month = replace(as.character(Month), Contrast=='Pre-war', "Baseline")) %>%
  arrange(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
  mutate(Month = factor(Month, levels=c('Baseline','Jan','Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov', 'Dec'))) %>%
  arrange(Contrast = factor(Contrast, levels = c('Pre-war', 'Covid', 'War'))) %>%
  mutate(Contrast = factor(Contrast, levels = c('Pre-war', 'Covid', 'War'))) %>%
  
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
  
  
  # geom_text(aes(label=Percent, col = Directipn),
  #           position = position_dodge(width = 1),
  #           vjust = -0.25,
  #           size = 12, fontface = 'bold') +
  #col = 'gray40') +
  
  scale_fill_manual(name = "Contrast", values=c("cyan4",'gray70',"darkorange")) +
  
  #scale_color_manual(name = "Contrast", values=c('#B2182B','#2166AC', "cyan4", "darkorange")) + #Increase & Decrease
  #scale_color_manual(name = "Contrast", values=c('#2166AC', "cyan4", "darkorange")) + #Increase
  scale_color_manual(name = "Contrast", values=c( "cyan4", 'gray70',"darkorange")) + #Decrease
  
  
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



kiev$density <- fields::interp.surface(
  MASS::kde2d(kiev$Ncars_avg, kiev$Npop), kiev[,c("Ncars_avg", "Npop")])

kiev %>%
  ggplot(aes(y = Npop, x = Ncars_avg)) +
  geom_point(aes( size = Ratio), alpha = 0.4, col = 'cyan4') +
  ylab("No. Population") + xlab("No. cars") + 
  theme_bw() +
 # scale_color_gradient(low = "cyan4", high = "#f0650e") +
  scale_size(breaks = c(50,100,500,1000,2000,3000), range = c(2, 10)) +
  geom_smooth(method = "gam", col="darkorange", fill = 'bisque',
              #method.args = list(family = "poisson"),
              method.args = list(family = "gaussian"),
              formula = y ~ s(x, bs = "cr", k=5))






###############################################################################################


# # 3.1) Plot with dynamic baseline 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # I.e., we don't care about matching grid_ids across year-specific months
# 
# ## Subet baseline year
# baseline <- NULL
# for(i in seq_along(ratio)){
#   baseline[[i]] <- filter(ratio[[i]], Year == '2019')
# }
# names(baseline) <- names(ratio)
# 
# 
# ## Split covid & war years by month within each city
# ### For covid period
# covid <- NULL
# for(i in seq_along(ratio)){
#   cy <- filter(ratio[[i]], Year == '2020')
#   covid[[i]] <- split(cy, cy$Month) #Split by month
# }
# names(covid) <- names(ratio)
# 
# 
# ## For war period
# war <- NULL
# for(i in seq_along(ratio)){
#   wy <- filter(ratio[[i]], Year == '2022')
#   war[[i]] <- split(wy, wy$Month) #Split by month
# }
# names(war) <- names(ratio)
# 
# 
# ## Now Calculate total population before/during war - Note, war images don't always cover the full
# ## AOI extent, and thereore the baseline population will differ. A workaround solution to this is to 
# ## Get only matching grid cells across the differnt months (of 2022), but here we rist at loosing considerable
# ## information if one month occupies only a smaller fraction of the city.
# 
# 
# war2 <- NULL
# prewar2 <- NULL
# 
# for(city in seq_along(war)){
#   
#   for(month in seq_along(war[[city]])){
#     
#     print(paste('Processing', names(war)[city], unique(war[[city]][[month]]$Month), sep = "_")) 
#     
#     CITY <- names(war)[city]
#     
#     ## Reset factor levels of grid_id
#     war[[city]][[month]]$grid_id <- factor(war[[city]][[month]]$grid_id)
#     baseline[[CITY]]$grid_id <- factor(baseline[[CITY]]$grid_id)
#     
#     
#     ## Get matching grid id
#     tmp <- intersect(war[[city]][[month]]$grid_id, baseline[[CITY]]$grid_id)
#     
#     
#     ## Filter for matching grid_id - war period
#     war[[city]][[month]] <- filter(war[[city]][[month]], grid_id %in% tmp)
#     
#     
#     ## Filter for matching grid_id - baseline
#     prewar <- filter(baseline[[CITY]], grid_id %in% tmp)
#     
#     
#     ## Calculate total population during war period
#     tmp <- war[[city]][[month]] %>%
#       st_drop_geometry() %>%
#       summarize(Totpop = round(sum(Npop)),
#                 Year = paste(unique(Year)),
#                 Month = paste(unique(Month)),
#                 City = paste(unique(City)))
#     
#     
#     war2 <- rbind(war2, tmp) #Bind results into data frame
#     
#     
#     ## Calculate total population for baseline period
#     tmp2 <- data.frame(Totpop = round(sum(prewar$Npop)))
#     tmp2$Year <- '2019'
#     tmp2$Month <- paste(unique(war[[city]][[month]]$Month))
#     tmp2$City <- paste(unique(war[[city]][[month]]$City))
#     
#     prewar2 <- rbind(prewar2, tmp2) #Bind results to data frame
#   }
#   
# }
# 
# dat1922 <- rbind(prewar2, war2)  


