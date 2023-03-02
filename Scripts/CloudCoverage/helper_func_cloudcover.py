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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Functions to support the imagery_cloudcover calculation script
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Functions initially developed by Masoomali, and further extended by me.
## Last update: 27/09/22



## ----------------- ##
## Parameters
## ----------------- ##
QUERY_URL = "https://api.discover.digitalglobe.com/v1/services/ImageServer/query"

QUERY_HEADERS = {'Content-Type': 'application/x-www-form-urlencoded',
                 'Accept': 'application/json',
                 'x-api-key': 'iSar7CX37j2hb3Apxp7Po6i5ZDlicfkGa8voURju'}


## ----------------- ##
## Functions 
## ----------------- ##


## Polygon area calculation
def calculate_polygon_area(polygon):
    polygon_proj = polygon['geometry'].to_crs({'proj':'cea'})
    poly_areas = [area/10**6 for area in polygon_proj.area]
    
    return(poly_areas)


## Convert geojson to esriPolygon (does not accept Multipolygon!)
def convert_geojson_to_esriPolygon(geom, spatialReference=4326):
    return {"rings": geom['geometry']['coordinates'],
           "spatialReference": {"wkid":spatialReference}}



## Launch API call
def issue_api_call(geom, start_time, end_time, sun_elevation_avg_min=0, 
                   image_band_names = ('4-BANDS','8-BANDS'), off_nadir_max=100,
                   area_cloud_cover_percentage_max=100,
                   max_results_count=500):
    # construct URL
    request_data = """outFields=*&inSR=4326&outSR=4326&spatialRel=esriSpatialRelIntersects&where=""" +\
        'sun_elevation_avg >= ' + str(sun_elevation_avg_min) + ' AND ' +\
        'image_band_name in' + str(image_band_names) + ' AND ' +\
        'collect_time_start >= ' + "'" + start_time + "'"+ ' AND ' +\
        'collect_time_start <= ' + "'"+ end_time + "'"+ ' AND ' +\
        'off_nadir_max <= ' + str(off_nadir_max) + '&' +\
        """returnCountOnly=false&f=json&geometryBasedFilters=""" +\
        'area_cloud_cover_percentage <= ' + str(area_cloud_cover_percentage_max) + '&' +\
        """geometryType=esriGeometryPolygon&geometry=""" +\
        json.dumps(convert_geojson_to_esriPolygon(geom)) + '&' +\
        """resultRecordCount=""" + str(max_results_count)
    
    # issue API call
    try:
        call_res = requests.post(QUERY_URL, data=request_data, headers=QUERY_HEADERS)
    except:
        print('API call failed! Try again later!')
        return None
    
    return call_res




def multipoly_to_poly(seamlines_file):
    
    
    # read in the file
    with open(seamlines_file,'r') as f:
        seams_js = json.load(f)
    
    df = gpd.read_file(seamlines_file)
    
    #df['geometry'].geom_type.values # We have one image as normal polygon and another as MultiPolygon
    
    
    ## Convert Multipolygon to Polygon (esri doesn't accept multipolygons!)
    if ('MultiPolygon' in df['geometry'].geom_type.values) == True:
        
        #idx = np.where(df['geometry'].geom_type.values == 'MultiPolygon') # Check for cases where we have Multipolygon
        #idx = int(idx[0])
        
        # for geom in seams_js['features']:
        
        #     geom2 = geom['geometry']
        #     g = ogr.CreateGeometryFromJson(json.dumps(geom2))
            
            
        #     # Save individual polygons into list
        #     ggeoms = []
        #     for idx, f in enumerate(g):
        #         tmp = (idx, f.ExportToJson())
        #         ggeoms.append(tmp)
            
            
        #     # Make a new json file
            
        #     properties = geom['properties']
        #     geo_dict = {}
            
        #     geo_dict["type"] = seams_js['type']
        #     geo_dict["crs"] = seams_js['crs']
        #     geo_dict["features"] = []
            
        #     for i in range(len(ggeoms)):
        #         tmp = {"type": "Feature", "properties":properties, "geometry": json.loads(ggeoms[i][1])}
        #         geo_dict["features"].append(tmp)
                
        #     seams_js = geo_dict
        
        
        ## Explode multipolygon geometries 
        geom_exploded = df.explode(index_parts=True)
        
        
        
        ## Convert geopandas df back to json
        colnames = ['acquisitionDate','ingestDate','earliestAcquisitionDate','latestAcquisitionDate']
        
        for col in colnames:
         
            if not isinstance(geom_exploded[col].iloc[0], str):
                geom_exploded[col] = geom_exploded.apply(
                    lambda x: (x[col]).strftime("%Y-%m-%d, %H:%M:%S"),
                    axis=1)
                
        ### Convert to json
        tmp = geom_exploded.to_json()
        
        ### Set string to dictionary (json standard format)
        seams_js = json.loads(tmp)
        
    return seams_js




## Get feature info from geojson file
def get_feature_info(seamlines_file):
    
    
    ## Convert multipolygon to polygon (whenever applicable)
    seams_js =  multipoly_to_poly(seamlines_file)
    
    # make api calls for each feature
    # and save the results
    
    api_res = []
    for geom in seams_js['features']:
        #print(geom)
        #print('\n\n\n\n')
        acquisitionDate = geom['properties']['acquisitionDate']
        start_time = acquisitionDate.split(' ')[0] + ' 00:00:00'
        end_time = acquisitionDate.split(' ')[0] + ' 23:59:59'
        
        res = issue_api_call(geom=geom,
                             start_time=start_time,
                             end_time=end_time)
        if res is None:
            api_res.append[res]
            continue
            
        #print(res.json())
        
        legacyId = geom['properties']['legacyId']
        legacyId_des = geom['properties']['legacyDescription'] #When actual legacy Id is in legacyDescription
        
        # imginfo = res.json()['features']
        # if imginfo[0]['attributes']['image_identifier'] == legacyId:
        #     res_list = imginfo
        # elif imginfo[0]['attributes']['image_identifier'] == legacyId_des:
        #     res_list = imginfo
        #     print('legacyId is part of legacyDescription')
        # else:
        #     print('Please check legacyId between geojson and API call. Not matching with either legacyId nor legacyDescription')
        
        res_list = [imginfo for imginfo in res.json()['features'] if imginfo['attributes']['image_identifier'] == legacyId or imginfo['attributes']['image_identifier'] == legacyId_des]
        api_res.append(res_list)
        
    return api_res
