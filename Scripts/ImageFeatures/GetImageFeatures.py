#######################################################################
#                                                                     #
#            Get satelltile image features as data frame              #
#                                                                     #
#######################################################################

# The following script allows to retrieve features from the satellite meta-data
# (eg., offNadir angle, etc.)
# The output file is a data frame, where each column is a feature from the image.


## Code written & maintained by Marie-Christine Rufener.


## Last update: 18/01/2023


#~~~~~~~~~~~~~~~~~~~
# Import libraries
#~~~~~~~~~~~~~~~~~~~

import argparse
import json
from osgeo import gdal
import numpy as np
import pandas as pd
import os
import datetime
#import difflib
from shapely.geometry import shape, Point, Polygon
import sys
import glob
from datetime import datetime
os.environ["OPENCV_IO_MAX_IMAGE_PIXELS"] = str(pow(2,40))
import cv2
import glob
rgb = True




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define standard directories
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Standard directory
inpdir = "/export/sc1/mfatehkia/ukraine_sat_img/downloads"

## Lists all directories and subdirectories from the input directory
alldir = glob.glob(inpdir + "/*/*/" + os.path.sep)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Get city, date and geojson inormation from listed directories
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# To be used lated


## Get city info
city_list = []
for i in range(len(alldir)):
	city = alldir[i].split("/")[6]
	city_list.append(city)


## Get date info
date_list = []
for i in range(len(alldir)):
	date = alldir[i].split("/")[7]
	date_list.append(date)


## Get geojson files
geojson_file_list = []
for i in range(len(city_list)):
	tmp3 = inpdir + '/' + city_list[i] + '/' + date_list[i] + '/SEAMLINES_shape.geojson'
	geojson_file_list.append(tmp3)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Get geojson feature information from the listed files 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Set-up data frame with columns we wish to retrieve information from the geojson file
img_data = pd.DataFrame(columns=['cloudCover', 'offNadirAngle', 'sunElevation', 'sunAzimuth', 'legacyId', 'city', 'acquisitionDate', 'date', 'productType','colorBandOrder'])


## Now get the feature information and append it to the data frame
for index in range(len((geojson_file_list))):
    
    with open(geojson_file_list[index]) as file:
        j = json.load(file)
        
        cloud_list = []
        nadir_list = []
        sunel_list = []
        sunazi_list = []
        img_list = []
        dat_list = []
        product_list = []
        color_list = []


        if len(j['features']) > 1:
            
            for i in range(len((j['features']))):
                                
                ## OffNadir angle
                #tmlist = []
                tmp_nadir = j['features'][i]['properties']['offNadirAngle']
                nadir_list.append(tmp_nadir)
                offNadirAngle = ",".join(map(str, nadir_list))

                ## Cloud cover
                tmp_cloud = j['features'][i]['properties']['cloudCover']
                cloud_list.append(tmp_cloud)
                cloudCover = ",".join(map(str, cloud_list))
                
                ## Sun elevation
                #tmlist = []
                tmp_sune = j['features'][i]['properties']['sunElevation']
                sunel_list.append(tmp_sune)
                sunElevation = ",".join(map(str, sunel_list))
                
                ## Sun Azimuth
                #tmlist = []
                tmp_sunazi = j['features'][i]['properties']['sunAzimuth']
                sunazi_list.append(tmp_sunazi)
                sunAzimuth = ",".join(map(str, sunazi_list))
                
                ## Image ID
                #tmlist = []
                tmp_img = j['features'][i]['properties']['legacyId']
                img_list.append(tmp_img)
                legacyId = ",".join(map(str, img_list))
                
                ## Acquisition date
                #tmlist = []
                tmp_date = j['features'][i]['properties']['acquisitionDate']
                dat_list.append(tmp_date)
                acquisitionDate = ",".join(map(str, dat_list))

                ## Product Type
                #tmlist = []
                tmp_product = j['features'][i]['properties']['productType']
                product_list.append(tmp_product)
                productType = ",".join(map(str, product_list))

                ## ColorBand
                #tmlist = []
                tmp_color = j['features'][i]['properties']['colorBandOrder']
                color_list.append(tmp_color)
                colorBandOrder = ",".join(map(str, color_list))
                

                
        else:
            # Note: The sturcutre below might change for other types of geojson files!!!
            cloudCover = j['features'][0]['properties']['cloudCover']
            offNadirAngle = j['features'][0]['properties']['offNadirAngle']
            sunElevation = j['features'][0]['properties']['sunElevation']
            sunAzimuth = j['features'][0]['properties']['sunAzimuth']
            legacyId = j['features'][0]['properties']['legacyId']
            acquisitionDate = j['features'][0]['properties']['acquisitionDate']
            productType = j['features'][0]['properties']['productType']
            colorBandOrder = j['features'][0]['properties']['colorBandOrder']

        
    city = city_list[index]
    date = date_list[index]
        
    # Append feature info to the data frame
    img_data.loc[index] = [cloudCover, offNadirAngle, sunElevation, sunAzimuth, legacyId, city, acquisitionDate, date, productType, colorBandOrder]


#~~~~~~~~~~~~~~~~~~~~~
#   Save data frame 
#~~~~~~~~~~~~~~~~~~~~~

outpath = "/export/home/mrufener/Ukraine/ImageFeatures"
outfile = "image_features.csv"

img_data.to_csv(outpath + outfile, index=False)

print("Data frame saved at ---> {}".format(outpath+outfile))
