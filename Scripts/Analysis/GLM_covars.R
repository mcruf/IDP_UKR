#######################################################################
#                                                                     #
#                   Imagery effect on car detection                   #
#                                                                     #
#######################################################################




#~~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(ggpubr)
library(jtools)


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

setwd("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine") ## Set appropriate WD


#~~~~~~~~~~~~~~~~~~~~~
# 1) Read data files
#~~~~~~~~~~~~~~~~~~~~~

# 1.1) List all files (from all the thresholds)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
files <- list.files(path = 'Data/Cars/Car_aggregated/POP_CLOUD_filtered/',
                    pattern = '.csv',
                    full.names = TRUE,
                    recursive = T)


# 1.2) Read the files
#~~~~~~~~~~~~~~~~~~~~~
cars <- list()
for(i in seq_along(files)) {
  cars[[i]] <- read.csv(files[i])
  cars[[i]]$Threshold <- basename(dirname(files))[i]
}


# 1.3) Bind the two data files into single data frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cars <- do.call('rbind', cars)

## To see the progress
table(cars$Threshold) #No. of images for each threshold


# 1.4) Subet data to 0.45 threshold
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cars <- filter(cars, Threshold == 'th_045_carclass_18')


# 1.5) Adjust variables
#~~~~~~~~~~~~~~~~~~~~~~~
cars$Image_resolution <- ifelse(cars$Image_resolution < 0.4, 0.3,
                                ifelse(cars$Image_resolution >= 0.4 & cars$Image_resolution < 0.5, 0.4,
                                       0.5))

cars$Image_resolution <- as.factor(cars$Image_resolution)


# 1.6) Append snow information to data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Import data
snow <- readxl::read_excel("Imagery_EDA/Image_cloud_snow_carDetection_evaluation.xlsx")

## Standardize column names
colnames(snow)[1:2] <- c('City', 'Date')

setdiff(cars$City, snow$City)
snow$City <- as.factor(snow$City)
levels(snow$City)[levels(snow$City) == "Bila_Tserkva"] <- "Bila-Tserkva"
levels(snow$City)[levels(snow$City) == "Velyki_Kopani"] <- "Velyki-Kopani"


## Merge snow data to cars data
snow$Image <- as.factor(paste(snow$City, snow$Date, sep = "_"))
cars$Image <- as.factor(cars$Image)

cars <- merge(cars, snow, by = 'Image')

cars[,c('Snow_presence', 'Cloud_presence')] <- lapply(cars[,c('Snow_presence', 'Cloud_presence')], factor)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Plot of covariates & ANCOVA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 2.1) Calculate car density
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cars$Dcars <- cars$Ncars / cars$AOI_area_covered_sqkm


## 2.2) Plot covariates
#~~~~~~~~~~~~~~~~~~~~~~~~

## Image resolution
res <- cars %>%
        ggplot(aes(x=Image_resolution, y = Dcars)) +
        geom_boxplot(fill = 'cyan4', alpha = 0.7) +
        theme_bw() +
        ylab(expression(bold(paste("Car Density (No. cars/km" ^ "2", ")")))) +
        xlab('Image resolution (m)') +
        geom_text(data=data.frame(), aes(label = '(a)', x = -Inf, y = Inf),
                  hjust = 0, vjust = 1, fontface = 'bold', size = 5) +
        #stat_summary(fun=mean, geom="point", shape=17, size=3, col = 'black') +
        theme(axis.title.y  = element_text(size = 13, face = 'bold', margin = unit(c(0, 3, 0, 0), "mm")), 
              axis.title.x = element_text(size = 13, face = 'bold', margin = unit(c(3, 0, 0, 0), "mm")), 
              plot.margin = margin(t=0, r=0, b=1.5, l=0, "cm"),
              axis.text = element_text(size = 12, face = 'bold'),
              panel.border =  element_blank(),
              #panel.grid.major = element_line(colour = grey(0.4),linetype = 3 ),
              strip.background = element_blank(),
              #axis.line = element_line(color = 'black'),
              legend.position = "none")


## Snow
snow <- cars %>%
  ggplot(aes(x=Snow_presence, y = Dcars)) +
  geom_boxplot(fill = 'cyan4', alpha = 0.7) +
  theme_bw() +
  ylab(expression(bold(paste("Car Density (No. cars/km" ^ "2", ")")))) +
  xlab('Snow presence') +
  #stat_summary(fun=mean, geom="point", shape=17, size=3, col = 'black') +
  geom_text(data=data.frame(), aes(label = '(b)', x = -Inf, y = Inf),
            hjust = 0, vjust = 1, fontface = 'bold', size = 5) +
  theme(axis.title.y  = element_text(size = 13, face = 'bold', margin = unit(c(0, 3, 0, 0), "mm")), 
        axis.title.x = element_text(size = 13, face = 'bold', margin = unit(c(3, 0, 0, 0), "mm")), 
        plot.margin = margin(t=0, r=0, b=1.5, l=0, "cm"),
        axis.text = element_text(size = 12, face = 'bold'),
        panel.border = element_blank(),
        #panel.grid.major = element_line(colour = grey(0.4),linetype = 3 ),
        strip.background = element_blank(),
        #axis.line = element_line(color = 'black'),
        legend.position = "none")



## off-Nadir
nadir <- cars %>%
  ggplot(aes(x=offNadirAngle, y = Dcars)) +
  geom_point(col = 'cyan4', alpha = 0.7, size = 3) +
  theme_bw() +
  ylab(expression(bold(paste("Car Density (No. cars/km" ^ "2", ")")))) +
  xlab('Off-Nadir (°)') +
  #stat_summary(fun=mean, geom="point", shape=17, size=3, col = 'black') +
  geom_text(data=data.frame(), aes(label = '(c)', x = -Inf, y = Inf),
            hjust = 0, vjust = 1, fontface = 'bold', size = 5) +
  theme(axis.title.y  = element_text(size = 13, face = 'bold', margin = unit(c(0, 3, 0, 0), "mm")), 
        axis.title.x = element_text(size = 13, face = 'bold', margin = unit(c(3, 0, 0, 0), "mm")),
        plot.margin = margin(t=0, r=0, b=1.5, l=0, "cm"),
        axis.text = element_text(size = 12, face = 'bold'),
        panel.border = element_blank(),
        #panel.grid.major = element_line(colour = grey(0.4),linetype = 3 ),
        strip.background = element_blank(),
        #axis.line = element_line(color = 'black'),
        legend.position = "none")


## sun elevation
sun <- cars %>%
  ggplot(aes(x=sunElevation, y = Dcars)) +
  geom_point(col = 'cyan4', alpha = 0.7, size = 3) +
  theme_bw() +
  ylab(expression(bold(paste("Car Density (No. cars/km" ^ "2", ")")))) +
  xlab('Sun elevation (°)') +
  #stat_summary(fun=mean, geom="point", shape=17, size=3, col = 'black') +
  geom_text(data=data.frame(), aes(label = '(d)', x = -Inf, y = Inf),
            hjust = 0, vjust = 1, fontface = 'bold', size = 5) +
  theme(axis.title.y  = element_text(size = 13, face = 'bold', margin = unit(c(0, 3, 0, 0), "mm")), 
        axis.title.x = element_text(size = 13, face = 'bold', margin = unit(c(3, 0, 0, 0), "mm")), 
        plot.margin = margin(t=0, r=0, b=1.5, l=0, "cm"),
        axis.text = element_text(size = 12, face = 'bold'),
        panel.border = element_blank(),
        #panel.grid.major = element_line(colour = grey(0.4),linetype = 3 ),
        strip.background = element_blank(),
        #axis.line = element_line(color = 'black'),
        legend.position = "none")



## cloud cover
cloud <- cars %>%
  filter(cloudCover < 0.48) %>%
  ggplot(aes(x=cloudCover, y = Dcars)) +
  geom_point(col = 'cyan4', alpha = 0.7, size = 3) +
  theme_bw() +
  ylab(expression(bold(paste("Car Density (No. cars/km" ^ "2", ")")))) +
  xlab('Cloud coverage (%)') +
  geom_text(data=data.frame(), aes(label = '(e)', x = -Inf, y = Inf),
            hjust = 0, vjust = 1, fontface = 'bold', size = 5) +
  #stat_summary(fun=mean, geom="point", shape=17, size=3, col = 'black') +
  theme(axis.title.y  = element_text(size = 13, face = 'bold', margin = unit(c(0, 3, 0, 0), "mm")), 
        axis.title.x = element_text(size = 13, face = 'bold', margin = unit(c(3, 0, 0, 0), "mm")), 
        plot.margin = margin(t=0, r=0, b=1.5, l=0, "cm"),
        axis.text = element_text(size = 12, face = 'bold'),
        panel.border = element_blank(),
        #panel.grid.major = element_line(colour = grey(0.4),linetype = 3 ),
        strip.background = element_blank(),
        #axis.line = element_line(color = 'black'),
        legend.position = "none")



allp <- ggarrange(res, snow, nadir, sun, cloud,
                  nrow = 2, ncol = 3)

ggsave('~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Exploratory/covariates.jpg',
       dpi = 300, width = 30, height = 20, unit='cm',
       bg = 'white')






# p<- ggboxplot(cars, x = "Image_resolution", y = "Dcars",
#           color = "cyan4")
# my_comparisons <- list( c("0.3", "0.4"), c("0.3", "0.5"), c("0.4", "0.5") )
# 
# p + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
#   stat_compare_means(label.y = 50)                   # Add global p-value




## 2.3) Go for the ANCOVA
#~~~~~~~~~~~~~~~~~~~~~~~~~
# mod <- aov(log(Dcars) ~ Image_resolution + Snow_presence + Cloud_presence + offNadirAngle + sunElevation,data = cars)
# summary(mod)
cars2 <- cars[-380,] #Remove observaion with very large cloud coverage value - constitutes an outlier
mod <- glm(log(Dcars) ~ Image_resolution + Snow_presence + offNadirAngle + sunElevation + cloudCover, data = cars2)

summ(mod, confint = TRUE, digits = 3) #Include the confidence interval

# p <- autoplot(mod, which = 1:6, ncol = 2, label.size = 3, 
#                    colour = "honeydew3", alpha =0.7) + theme_bw() 

p1 <- autoplot(mod, which = 1, ncol = 1, label.size = 3, 
         colour = "honeydew3", alpha =0.7) + theme_bw() 

p2 <- autoplot(mod, which = 2, ncol = 1, label.size = 3, 
              colour = "honeydew3", alpha =0.7) + theme_bw() 

p3 <- autoplot(mod, which = 3, ncol = 1, label.size = 3, 
              colour = "honeydew3", alpha =0.7) + theme_bw() 


p4 <- forecast::ggAcf(mod$residuals) +
      geom_segment(color = "honeydew3", lwd =1) +
      geom_hline(yintercept = 0, color = "honeydew3", lwd = 1) +
      labs(title = 'ACF') +
      #geom_hline(yintercept = c(0.08489546, -0.08489546), linetype = "dashed") + 
      theme_bw() 

allp2 <- ggarrange(p1[[1]], p2[[1]], p3[[1]], p4,
                  nrow = 2, ncol = 2)


jpeg("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Exploratory/GLM_diagnostics.jpg",
     bg = "white", res = 300, 
     width = 20, height = 15, units = 'cm')
print(allp2)
dev.off()




## 2.3.1) Plot predicted vs. observed
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cars2$Predicted <- exp(predict(mod))

pp <- cars2 %>%
  ggplot(aes(x=Predicted, y = Dcars)) +
  geom_point(col = 'cyan4', alpha = 0.7, size = 3) +
  theme_bw() +
  geom_smooth(method = 'lm', col = 'gray40') +
  ylab('Observed') +
  xlab('Predicted') +
  # geom_text(data=data.frame(), aes(label = '(e)', x = -Inf, y = Inf),
  #           hjust = 0, vjust = 1, fontface = 'bold', size = 5) +
  #stat_summary(fun=mean, geom="point", shape=17, size=3, col = 'black') +
  theme(axis.title.y  = element_text(size = 13, face = 'bold', margin = unit(c(0, 3, 0, 0), "mm")), 
        axis.title.x = element_text(size = 13, face = 'bold', margin = unit(c(3, 0, 0, 0), "mm")), 
        plot.margin = margin(t=0, r=0, b=1.5, l=0, "cm"),
        axis.text = element_text(size = 12, face = 'bold'),
        panel.border = element_blank(),
        #panel.grid.major = element_line(colour = grey(0.4),linetype = 3 ),
        strip.background = element_blank(),
        #axis.line = element_line(color = 'black'),
        legend.position = "none")

jpeg("~/OneDrive - Hamad bin Khalifa University/Projects/Ukraine/Manuscript/Figures/Exploratory/GLM_pred_vs_obs.jpg",
     bg = "white", res = 300, 
     width = 15, height = 15, units = 'cm')
print(pp)
dev.off()


#export_summs(mod)

# library(rstanarm)
# library(broom)
# library(coda)
# mod3 = stan_glm(log(Dcars) ~ Image_resolution + Snow_presence + cloudCover + offNadirAngle + sunElevation,
#                 data = cars, 
#                 iter = 2000, 
#                 warmup = 500,
#                 chains = 3, 
#                 thin = 2, 
#                 refresh = 0, 
#                 prior_intercept = normal(0, 100),
#                 prior = normal(0, 100), 
#                 prior_aux = cauchy(0, 2))
# 
# 
# mcmc_areas(as.matrix(mod3),
#            #pars = c("cyl", "drat", "am", "wt"),
#            prob = 0.8)
