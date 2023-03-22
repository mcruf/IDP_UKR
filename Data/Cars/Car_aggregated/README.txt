# This folder stores the aggreagted car data versions.
# I.e., we have the total no. of cars per image.


# There are two main folders: (i) original, (ii) OSM_filtered

## The /original folder ##
> Stores the raw data aggregated by Ferda through his script obtain_metadata_ukraine3.py.
> It is 'raw' because it includes all detected cars, i.e., false-positives were not cleaned by the OSM_Filtering_script_01.R pipeline.
> The aggregation is conducted individually for each confidence threshold 
(0.15, 0.45), and considered car classes (18 or multiple car classes, as 
done in the past).


## The /OSM_filtered folder ##
> Stores the OSM-filtered aggregated data.
> False-positive are first removed through the OSM_Filtering_script_01.R. Then, the Aggregation_script_02.R is ran to generate the aggregated data version.


### NOTE
Within the two folders, there are subfolders specific to the confidence threshold (0.15 or 0.45) and the considered car class(es) (only car class 18, or multiple car classes)


# th_015 - outputs for 0.15 confidence threshold with multiple car classes
# th_015_carclass_18 - same as above, but only car class 18
# th_045_carclass_18 - same as above, but with confidence threshold set to 0.45
