############################################################################
#                                                                          #
#           Plotting results from confidence threshold experiment          #
#                                                                          #
############################################################################


# The following script provides the plots to the confidence threshold experiment.
# The experiment was conducted on annotaded data from Syria, whereby F1.0 and F0.5 scores
# were evaluated. The overall aim of the experiment is to evaluate the optimal
# confidence threshold value.
# Here, we target the F0.5 as it is of our interest to provide more weight to precision than recall.
# F1.0 score equally weights precision and recall.

# Please adapt the folder name according to your own needs!

## Code written by: Marie-Christine Rufener < macrufener@gmail.com > 
## Last update: May 2023


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~
library(ggplot2)
library(dplyr)

#~~~~~~~~~
# Set WD
#~~~~~~~~~
## Set main working directory
setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/GitHub/IDP_UKR/") 


#~~~~~~~~~~~~
# Functions
#~~~~~~~~~~~~
## Will be used for the plots at the end of the script
everysecond <- function(x){
  x <- unique(x)
  x[seq(1, length(x), 2)] <- ""
  x
}


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Load the experiment results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- read.csv('Data/ThresholdExperiment/confidence_threshold_experiment.csv')



#~~~~~~~~~~~~~~~~~~~~~
# 2) Make the plot
#~~~~~~~~~~~~~~~~~~~~~

optimum <- df[which(df$F0.5.Score == max(df$F0.5.Score)), 'Confidence']

df %>% 
  ggplot(aes(x=Confidence, y = F0.5.Score)) +
  geom_line(linewidth = 1, col = 'navyblue') +
  geom_vline(xintercept = optimum, linetype = "dashed", linewidth = 1, col = 'darkorange') +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,1,0.05), labels = everysecond(df$Confidence)) +
  ylab('F0.5  Score') + xlab('Confidence threshold') +
  theme(axis.title  = element_text(size = 14, face = 'bold'), 
        axis.text.y = element_text(size = 13),
        #axis.text.x = element_text(size = 14,angle = 90, hjus = 1, vjus =0.5),
        axis.text.x = element_text(size = 13),
        panel.border = element_blank(),
        #axis.line = element_line(color = 'black'),
        legend.position = "none")

ggsave('../../Manuscript/Figures/Exploratory/Confidence_threshold_experiment.jpg',
       dpi = 300, width = 20, height = 10, unit='cm')
