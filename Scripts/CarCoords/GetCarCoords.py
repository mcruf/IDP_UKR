###################################################
#                                                 #
#   Get geolocations of detected objects - cars   #
#                                                 #
###################################################


# The following script provides an output file (.txt) containing
# the spatial coordinates of the detected cars.
# The output file is specific to each image.

# There are two main inputs that needs to be defined by the user:
# @confidence threshold - e.g, 0, 0.10, 0.15, 0.45, ....
# @object class - refers to category of objects (small vehicles, large vehicles,...)

# The current script uses 0.45 confidence threshold, and focuses only on
# small cars (class 18).

# For a list of the different categories, refer to:
# https://github.com/DIUx-xView/xView1_baseline/blob/master/xview_class_labels.txt


## Script largely developed by Ferda Ofli, and further adapted by Marie-Christine Rufener.

## Last update: March 2023

########################


#~~~~~~~~~~~~~~~~~~
# Import libraries
#~~~~~~~~~~~~~~~~~~

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get coordinates function
#~~~~~~~~~~~~~~~~~~~~~~~~~~
def process_data(model_file, tif_file, geojson_file, classes, confidence):
    
    model_data = np.loadtxt(model_file)
    
    if len(model_data) == 0:
        return []
    
    tif = gdal.Open(tif_file)
    gt = tif.GetGeoTransform()
    x_min = gt[0]
    x_size = gt[1]
    y_min = gt[3]
    y_size = gt[5]

    with open(geojson_file) as f:
        j = json.load(f)

    tc = 0 # true car
    fc = 0 # false car
    nc = 0 # not a car
    lngs = []
    lats = []
    
    for index in range(len(model_data)):
        if (model_data[index][4] in classes) and (model_data[index][5] > confidence):
            dx = (model_data[index][0] + model_data[index][2])/2
            dy = (model_data[index][1] + model_data[index][3])/2
            px = dx * x_size + x_min
            py = dy * y_size + y_min
            point = Point(px, py)
            found = False
            for feature in j['features']:
                polygon = shape(feature['geometry'])
                if polygon.contains(point):
                    lngs.append(px)
                    lats.append(py)
                    tc+=1
                    found = True
                    break
            if not found:
                fc += 1
        else:
            nc+=1
    print('Found {} cars'.format(tc))
    return lngs,lats



#~~~~~~~~~~~~~~~~~~~~~~
# Extract coordinates
#~~~~~~~~~~~~~~~~~~~~~~

# Input/output directories
basedir = "/export/sc1/mfatehkia/ukraine_sat_img/downloads/*/*"
inpdir = "/export/sc1/mfatehkia/ukraine_sat_img/downloads"
outdir = "/export/sc2/fofli/Projects/SatelliteCarDetection/temp/covid19-satellite-analysis/output_ukraine_new"


# Input parameters
confidence = 0.45 # could also be 0, 0.10, 0.15, 0.20,...


## We'll do a short test - REMOVE after everything works
#test = glob.glob(basedir)[1]
#city = test.split("/")[6] #sixth argument returns the city
#date = test.split("/")[7] #seventh argument returns the city


## List of cities and dates
dirs = glob.glob(basedir + os.path.sep) #Include all directories and subdirectories

alldir = []
alldir = [directory for directory in dirs if 'cancelled' not in directory] # Remove obsolete directory (Kyiv/b_s_cancelled_due_toclouds)



city_list = []
for i in range(len(alldir)):
	city = alldir[i].split("/")[6]
	city_list.append(city)


date_list = []
for i in range(len(alldir)):
	date = alldir[i].split("/")[7]
	date_list.append(date)


outfile = ""

#vehicles = [17, 18, 19, 20, 34, 36, 37]
vehicles = [18] #small cars

fdata = np.array(['City', 'Label', 'Month', 'Year', 'Date', 'Day of Week', 'Area', 'SQKM', 'Count']) 


# Input files
#txt_file = outdir + '/' + city + '_' + date + '.txt'
#tif_file = inpdir + '/' + city + '/' + date + '/stitched_img.tif'
#geojson_file = inpdir + '/' + city + '/' + date + '/SEAMLINES_shape.geojson'


#txt_file_list = []
#for i in range(len(city_list)):
#	tmp = outdir + '/' + city_list[i] + '_' + date_list[i] + '.txt'
#	txt_file_list.append(tmp)

## There are some files stored in a different folder
txt_file_list = []
for i in range(len(city_list)):
    if(os.path.isfile(outdir + '/' + city_list[i] + '_' + date_list[i] + '.txt')):
        tmp = outdir + '/' + city_list[i] + '_' + date_list[i] + '.txt'
    else:
        tmp =  '/export/sc2/fofli/Projects/SatelliteCarDetection/temp/covid19-satellite-analysis/' + 'ukraine_extra_large' + '/' + 'output' + '/' +  city_list[i] + '_' + date_list[i] + '.txt'
    txt_file_list.append(tmp)



tif_file_list = []
for i in range(len(city_list)):
	tmp2 = inpdir + '/' + city_list[i] + '/' + date_list[i] + '/stitched_img.tif'
	tif_file_list.append(tmp2)


geojson_file_list = []
for i in range(len(city_list)):
	tmp3 = inpdir + '/' + city_list[i] + '/' + date_list[i] + '/SEAMLINES_shape.geojson'
	geojson_file_list.append(tmp3)


for i in range(len(txt_file_list)):
    print('Processing {} ...'.format(txt_file_list[i]), flush=True)
    lngs, lats = process_data(txt_file_list[i], tif_file_list[i], geojson_file_list[i], vehicles, confidence)
    if lngs != []:
        df = pd.DataFrame(list(zip(lngs, lats)),columns=['Longitude','Latitude'])
        #if outfile == "":
        outfile = 'car_coords_{}_{}_{:.02}.csv'.format(city_list[i],date_list[i],confidence)
        with open(outfile,'w') as f:
            df.to_csv(f, index = False, sep='\t')
        print('Output file saved: {}'.format(outfile))
    else:
        print('No cars detected for the input criteria!')



##print('Processing {} ...'.format(txt_file), flush=True) #Original code
##lngs,lats = process_data(txt_file, tif_file, geojson_file, vehicles, confidence) #Original code

# if lngs != []:
#     df = pd.DataFrame(list(zip(lngs, lats)),columns=['Longitude','Latitude'])
#     if outfile == "":
#         outfile = 'car_coords_{}_{}_{:.02}.csv'.format(city,date,confidence)
#     with open(outfile,'w') as f:
#         df.to_csv(f, index = False, sep='\t')
#     print('Output file saved: {}'.format(outfile))
# else:
#     print('No cars detected for the input criteria!')
