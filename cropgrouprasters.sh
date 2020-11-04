# Script to create a single raster for each crop group

# Unique crop groups
sed '1d' earthstat_crops.tsv | cut -f 3 | sort | uniq > crop_groups.txt

# Loop through the crop groups text file, each time find the names of all crops belonging to that group. (harvested area)
for crop_group in $(cat crop_groups.txt); do
  # Find the names of all crops belonging to that group
  # Extract the file path for the harvested area fraction raster.
  grep $crop_group earthstat_crops.tsv | 
    awk '{print "/nfs/qread-data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/HarvestedAreaYield175Crops_Geotiff/"$1"_HarvAreaYield_Geotiff/"$1"_HarvestedAreaHectares.tif"}' > crop_names.txt
  # Create a VRT from the raster name text file.
  echo $crop_group
  gdalbuildvrt -separate -input_file_list crop_names.txt /nfs/qread-data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/${crop_group}.vrt
done 

# Now do the production rasters
for crop_group in $(cat crop_groups.txt); do
  # Find the names of all crops belonging to that group
  # Extract the file path for the harvested area fraction raster.
  grep $crop_group earthstat_crops.tsv | 
    awk '{print "/nfs/qread-data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/HarvestedAreaYield175Crops_Geotiff/"$1"_HarvAreaYield_Geotiff/"$1"_Production.tif"}' > crop_names.txt
  # Create a VRT from the raster name text file.
  echo $crop_group
  gdalbuildvrt -separate -input_file_list crop_names.txt /nfs/qread-data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/production_${crop_group}.vrt
done 
