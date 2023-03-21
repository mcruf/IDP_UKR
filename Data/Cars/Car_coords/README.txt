# This folder stores CNN outputs with the geolocated cars.

# There is one .txt file for each individual image.
# These files were generated in accordance to the considered confidence 
threshold (0.15 or 0.45) and car class (18 or multiple car classes).

# Files were generated through the script:
GetCarCoords.py

# Note: this scrip does NOT provide output file for images in which no 
cars were detected. This information has to be retrieved at a later stage 
from the overall aggregated database provided in 
the folder:

Data/Cars/Car_aggregated 

# th_015 - outputs for 0.15 confidence threshold with multiple car classes
# th_015_carclass_18 - same as above, but only car class 18
# th_045_carclass_18 - same as above, but with confidence threshold set to 0.45
