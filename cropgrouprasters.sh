# Script to create a single raster for each crop group

# Unique crop groups
sed '1d;$d' earthstat_crops.tsv | cut -f 3 | sort | uniq > crop_groups.txt

# Loop through the crop groups text file, each time find the names of all crops belonging to that group.
while read crop_group; do
  # Find the names of all crops belonging to that group
  grep $crop_group earthstat_crops.tsv | 
    awk '{print "/nfs/qread-data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/HarvestedAreaYield175Crops_Geotiff/"$1"_HarvAreaYield_Geotiff/"$1"_HarvestedAreaHectares.tif"}' > crop_names.txt
  # Create a VRT from the raster name text file.
  gdalbuildvrt -separate -input_file_list crop_names.txt /nfs/qread-data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/${crop_group}.vrt
done < crop_groups.txt

