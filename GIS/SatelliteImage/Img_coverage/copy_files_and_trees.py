#~~~~~~~~~~~~~~~~~
# Import libraries
#~~~~~~~~~~~~~~~~~~
import glob
import os
import shutil


# The following script copies a given file from a source directory to a destination directory while keeping the path structure of the source directory.
# This might be useful when the sourced file has the same name in the given directories, and we therefore want to keep the identity of the original path structure. 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define source file(s) and destination directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Search for all SEAMLINES_shape.geojson files in given directory and subdirectory
srcfiles = []
srcfiles = glob.glob('/export/sc1/mfatehkia/ukraine_sat_img/downloads/*/*/SEAMLINES_shape.geojson*', recursive=True)


## Define destination root to copy folders + files
dstroot = '/export/home/mrufener/Ukraine/GeoJson'


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Copy source files to destination dirs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dstdir = []

for i in range(len(srcfiles)):
    tmp = dstroot + "/" + srcfiles[i].split("/")[6] + "/"+ srcfiles[i].split("/")[7] #Keep only relevant part of the absolute path of the source directory
    dstdir.append(tmp)
    os.makedirs(dstdir[i]) # Build directory in destination path as in source path
    shutil.copy(srcfiles[i], dstdir[i]) #Copy files in given directory
