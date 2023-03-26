# This folder stores the aggreagted car data versions.
# I.e., we have the total no. of cars per image.


# There are three main folders: (i) original, (ii) OSM_filtered, and (iii) POP_and_CLOUD_filtered. The folders stores the results sequentially, i.e. OSM_filtered is built upon data from original, and POP_and_CLOUD_filtered is built upon data from OSM_filtered. Data stored in /POP_and_CLOUD_filtered represent the final stage of the data cleaning approach.



## The /original folder ##
> Stores the raw data aggregated by Ferda through his script obtain_metadata_ukraine3.py.
> It is 'raw' because it includes all detected cars, i.e., false-positives were not cleaned by the OSM_Filtering_script_01.R pipeline.
> The aggregation is conducted individually for each confidence threshold 
(0.15, 0.45), and considered car classes (18 or multiple car classes, as 
done in the past).


## The /OSM_filtered folder ##
> Stores the OSM-filtered aggregated data.
> False-positive are first removed through the OSM_Filtering_script_01.R. Then, the Aggregation_script_02.R is ran to generate the aggregated data version.


## The /POP_CLOUD_filtered folder ##
> Build on top of the OSM-filtered aggregated data.
> Includes the final data, i.e., the ones used for the patio-temporal dynamic analysis.
> First, we classify each image in accordance to its population distribution fraction (the distribution index) -- see script in Scripts/PostProcessing/POP_Filtering_script_03.R. This file is then used to filter out any image whose distribution index was inferior to 50%.
> Finally, we also remove images in which zero to very low car numbers were detected, and which were strictly due to images obstructed by dense cloud or haze layers.



### NOTE
Within the three folders, there are subfolders specific to the confidence threshold (0.15 or 0.45) and the considered car class(es) (only car class 18, or multiple car classes)


# th_015 - outputs for 0.15 confidence threshold with multiple car classes
# th_015_carclass_18 - same as above, but only car class 18
# th_045_carclass_18 - same as above, but with confidence threshold set to 0.45
