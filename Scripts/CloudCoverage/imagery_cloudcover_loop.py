#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov  7 10:44:58 2022

@author: marie-christinerufener
"""

import requests
import json
import geopandas as gpd
import pandas as pd
import numpy as np
import time 
import glob
import os
from osgeo import ogr
import geojson
from datetime import datetime

#import helper_func_cloudcover
#execfile("helper_func_cloudcover.py")
exec(open("helper_func_cloudcover.py").read())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Retrieve cloud coverage for AOI
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Maxar provides cloud coverage for the entire image tile that has been downloaded.
# We need cloud coverage only for the AOI, i.e., part of the image tile.
# This scrip therefore aims to recover imagery info from Maxar's backend
# and calculate the cloud percentage for the AOI.


# Script was initially written by Masoomali Fatehkia, and further adapted and extended
# by me.

## Last update: 11/07/2022

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><


# 1) Define inputs and outputs directories
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Set input directory containing downloaded imagery
img_folder_path = "/export/sc1/mfatehkia/ukraine_sat_img/downloads"
#img_folder_path = "/Users/marie-christinerufener/Desktop/img_test/Geometry_problem"
#img_folder_path = "/Users/marie-christinerufener/Desktop/img_test/Working"
#img_folder_path = "/Users/marie-christinerufener/Desktop/img_test/Chernivtsi/"


## Get dirs and subdirs from standard dir
alldir = glob.glob(img_folder_path + "/*/*/" + os.path.sep)
#alldir = glob.glob(img_folder_path + "/*/" + os.path.sep)


## Set output directory to which files should be saved
mypath = glob.glob('/export/home/mrufener/Ukraine/CloudCover/*/*/')
#mypath = glob.glob('/Users/marie-christinerufener/Desktop/img_test/Output/*/*/')



# 2)  Set-up data frames to retrieve result information
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Set-up data frame to retrieve information for failed imagery
fail_data = pd.DataFrame(columns=['City_date', 'Loop_index', 'Reason'])
faildir = '/export/home/mrufener/Ukraine/CloudCover/' #Output directory of fail log


## Set-up data frame for fail image feature (API call)
#fail_api = pd.DataFrame(columns=['City_date', 'Loop_Index'])



## Set-up data frame to retrieve information from cloud coverage
cloud_data =  pd.DataFrame(columns=['City','Date','Clouded_Area_Perc','Loop_Index'])



# 3) Get cloud coverage
#~~~~~~~~~~~~~~~~~~~~~~~~

## Set temporary lists
seam_lines_path_list = []
seams_res_list = []


for i in range(len(alldir)):
    
    print(alldir[i])
    
    
    if __name__ == '__main__':
        
        
        ## Get the exact cloud coverage from the API
        tmp = alldir[i] + r'SEAMLINES_shape.geojson'
        seam_lines_path_list.append(tmp)
        
        
        try:
            tmp2 = get_feature_info(seamlines_file=seam_lines_path_list[i])
            seams_res_list.append(tmp2)
        except KeyError:
            print('Cannot get feature info from: {}'.format(alldir[i]))
            print('Please check fail log in:{}'.format(faildir))
            fail = alldir[i].split("/")[6] + "/" + alldir[i].split("/")[7]
            fail_data.loc[i] = [fail, i, 'Geometry_problem']
            fail_data.to_csv(faildir + "/" + "fail_data_log.csv", index=False)
            tmp2 = []
            seams_res_list.append(tmp2)
            
        
        if len(seams_res_list[i]) == 0:
            City =  alldir[i].split("/")[6]
            Date = alldir[i].split("/")[7]
            cloud_data.loc[i] = [City, Date, None, i]
            #cloud_data.to_csv('/export/home/mrufener/Ukraine/CloudCover/cloud_img_results.csv', index=False)
            time.sleep(60)
            continue
            
        with open(mypath[i] + r'cloudcover_results.json', 'w') as f:
            json.dump(seams_res_list[i],f,indent=2)
        
        for sm in range(len(seams_res_list[i])):
            if len(seams_res_list[i][sm]) > 1:
                
                fail = alldir[i].split("/")[6] + "/" + alldir[i].split("/")[7]
                fail_data.loc[i] = [fail, len(seams_res_list[i][sm]), 'Multiple_result_entry']
                fail_data.to_csv('/export/home/mrufener/Ukraine/CloudCover/fail_data_log.csv', index=False)
                print('More than one result entry found for image id of: {}'.format(fail))
                print('Not sure what to do. Please check fail log in: {}'.format(faildir))
                #print('Exiting...')
                break
                #exit()
                #print('More than one result entry found for an image id.')
                #print('Not sure what to do! Exiting!')
                #exit()
                
        else:
            ## Convert Multipolygon to polygon (whenever applicable)
            geom_converted = json.dumps(multipoly_to_poly(seamlines_file=seam_lines_path_list[i]))
            
            ## Calculate polygon areas
            seams_areas = np.array(calculate_polygon_area(gpd.read_file(geom_converted)))
            
            try:
                seams_cloudperc = np.array([sp[0]['attributes']['area_cloud_cover_percentage'] for sp in seams_res_list[i]])/100
            except IndexError:
                print('Cannot compute cloud percentage: {}'.format(alldir[i]))
                print('Please check fail log in:{}'.format(faildir))
                fail = alldir[i].split("/")[6] + "/" + alldir[i].split("/")[7]
                fail_data.loc[i] = [fail, i, 'Cloud_computation']
                fail_data.to_csv(faildir + "/" + "fail_data_log.csv", index=False)
                time.sleep(60)
                continue
            
            clouded_area = sum(seams_areas * seams_cloudperc)
            clouded_area_perc = 100*clouded_area/sum(seams_areas)
            
            City = alldir[i].split("/")[6]
            Date = alldir[i].split("/")[7]
            cloud_data.loc[i] = [City, Date, clouded_area_perc, i]
            cloud_data.to_csv('/export/home/mrufener/Ukraine/CloudCover/cloud_img_results.csv', index=False)
            
            print('+ % AOI area clouded:', clouded_area_perc)
            time.sleep(60) #To launch the API call once every 60s


