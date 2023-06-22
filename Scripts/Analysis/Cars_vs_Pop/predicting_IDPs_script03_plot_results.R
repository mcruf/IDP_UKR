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
library(viridis)
library(dplyr)
library(scales)
library(ggsci)
library(sf)
library(mgcv)
library(mgcViz)
library(ggpubr)


## Standard working directory
setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GitHub/IDP_UKR/") 


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Load the data set
#~~~~~~~~~~~~~~~~~~~~~~~~


# 1.1) Ratio prediction method
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ratio <- readRDS("Data/Predictions/PopPred_ratio_method.rds")


# 1.2) GAM prediction method
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#INCLUDE!



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Some additional data manipulation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


