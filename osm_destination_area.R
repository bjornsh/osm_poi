##################################################################################
### Syfte
##################################################################################

# Identify areas containing multiple POIs of different categories within
# a certain radius. Merge overlapping polygons to create destination areas.
# Script builds on data created by https://github.com/bjornsh/osm_poi/blob/main/osm_poi.R



##################################################################################
### Clean start
##################################################################################

rm(list = ls())
gc()


##################################################################################
### Libraries etc
##################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, osmdata, mapview, leaflet)


# avoid scientific notation
options(scipen=999)

# switch off fail with spherical geometry problems
sf::sf_use_s2(FALSE)



##################################################################################
### Define paths and create data directories (if not already exist)
##################################################################################

# create local directories
dir.create(paste0(getwd(), "/shapefile"))
dir.create(paste0(getwd(), "/output"))

# define local paths
folder_shapefile = paste0(getwd(), "/shapefile")
folder_output = paste0(getwd(), "/output")

# define path to Github folder containing geodata
folder_github = "https://github.com/bjornsh/gis_data/raw/main/" 


##################################################################################
### Functions
##################################################################################

`%notin%` <- Negate(`%in%`)


### download and unzip shapefiles from Github unless done previously
get_shapefile <- function(path){
  file_name = str_extract(url_shp, "[^/]+(?=\\.zip$)")
  file_name_full = paste0(file_name, ".zip")
  
  if(!file.exists(paste0(folder_shapefile, "/", file_name_full))){
    file_name = str_extract(url_shp, "[^/]+(?=\\.zip$)")
    file_name_full = paste0(file_name, ".zip")
    download.file(url_shp, destfile = paste0(folder_shapefile, "/", file_name_full))
    fname = unzip(paste0(folder_shapefile, "/", file_name_full), list=TRUE)$Name[1:5]
    unzip(paste0(folder_shapefile, "/", file_name_full), 
          exdir=folder_shapefile, 
          overwrite=TRUE)
  }
  file_name <<- file_name
}



##################################################################################
### Download data
##################################################################################

### tätort
url_shp = paste0(folder_github, "mb_riks.zip")
get_shapefile(url_shp)
tatort = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
                 options = "ENCODING=WINDOWS-1252")


### OSM POIs created by https://github.com/bjornsh/osm_poi/blob/main/osm_poi.R
resultat_sf = st_read(paste0(folder_output, "/osm_poi.shp"))



##################################################################################
### Identify clusters with points of at least X categories within X meter of each other
##################################################################################

# define variables
number_categories = 2 # min number of points of different categories present within radius of a point 
radius = 500 # in meter

### Point SF
point_sf = resultat_sf %>% 
  # create id
  mutate(id = seq(1:nrow(resultat_sf))) %>% 
  # create category
  mutate(typ = sub(" .*", "", display)) %>% 
  # convert to sweref to use metric unit in buffer
  st_transform(3006)

### Buffer SF
buffer_sf = resultat_sf %>% 
  st_transform(3006) %>% 
  # create of buffer with "radius" around all points 
  st_buffer(., radius) %>%
  # create buffer id
  mutate(id_buffer = seq(1:nrow(resultat_sf)))
  
### join points and polygons, ie which points are within which buffer
merged = point_sf %>% 
  # for testing only
  # head(., n = 1000) %>%  
  st_join(., buffer_sf)


# identify all buffer with at least 2 points of different categories
buffer_keep = merged %>% 
  as.data.frame() %>% 
  group_by(id_buffer, typ.x) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(id_buffer) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  ungroup()

# remove all buffer areas not identified above
buffer_tobekept = buffer_sf %>% 
  filter(id_buffer %in% buffer_keep$id_buffer)

# create complementing sf with all removed areas
buffer_out = buffer_sf %>% 
  filter(id_buffer %notin% buffer_keep$id_buffer)


# dissolve boundaries between overlapping polygons 
destination_area = st_cast(st_union(buffer_tobekept),"POLYGON")



##################################################################################
### Visualise data
##################################################################################

karta_destination_area = destination_area %>% 
  st_transform(4326) %>% 
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillOpacity = 0)

print(karta_destination_area)



karta_kontroll = karta_destination_area + 
  mapview(buffer_out, col.regions = "red") + 
  mapview(point_sf)

print(karta_kontroll)


##################################################################################
### Store data
##################################################################################

# html karta
mapshot(karta_destination_area, url = paste0(folder_output, "/karta_destination_area.html"))


# shapefile
st_write(destination_area, 
         paste0(folder_output, "/destination_area.shp"), 
         layer_options = "ENCODING=UTF-8")


