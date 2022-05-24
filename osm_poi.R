##################################################################################
### Syfte
##################################################################################

# Download OSM points of interest (POI) data for a whole län
# filter relevant categories
# visualise and store data



##################################################################################
### Clean start
##################################################################################

rm(list = ls())
gc()


##################################################################################
### Libraries etc
##################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, osmdata, mapview)


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


### Download POI from OSM using key attribute
get_poi1 = function(key) {
  steg1 = opq("Uppsala län sweden") %>%
    add_osm_feature(key = key) %>%
    osmdata_sf() 
  
  #### manipulate OSM data that comes in different data types
  # OSM point features
  if (!is.null(steg1$osm_points) & # something exists
      length(steg1$osm_points) > 2) { # just OSM ID and geometry is not enough
    steg2_point = steg1$osm_points %>% 
      # remove apparently wrong points 
      filter(!is.na(name)) %>% 
      # remove points outside län
      st_join(., lan) %>% 
      filter(!is.na(LANSNAMN)) %>%
      #   select(name, key) %>% 
      mutate(kat = key) 
  }
  
  
  # OSM polygon features
  if (!is.null(steg1$osm_polygons) & 
      length(steg1$osm_polygons) > 2) {
    steg2_poly = steg1$osm_polygons %>% 
      filter(!is.na(name)) %>% 
      # remove points outside län
      st_join(., lan) %>% 
      filter(!is.na(LANSNAMN)) %>%
      mutate(kat = key) %>% 
      # turn polygon into point
      st_centroid()
  }
  
  # OSM multipolygon features
  if (!is.null(steg1$osm_multipolygons) & 
      length(steg1$osm_multipolygons) > 2) { 
    steg2_multipoly = steg1$osm_multipolygons %>% 
      filter(!is.na(name)) %>% 
      # remove points outside län
      st_join(., lan) %>% 
      filter(!is.na(LANSNAMN)) %>%
      mutate(kat = key) %>%
      # turn polygon into point
      st_centroid()
  }
  
  # append data into same SF frame
  #steg3 = bind_rows(steg2_point, steg2_poly, steg2_multipoly)
  steg3 = bind_rows(mget(ls(pattern = "steg2")))
  
  return(steg3)
}



##################################################################################
### Download data
##################################################################################

### Län shapefile to filter OSM data

url_shp = paste0(folder_github, "an_riks.zip")
get_shapefile(url_shp)
lan = st_read(paste0(folder_shapefile, "/", file_name, ".shp"),
              options = "ENCODING=WINDOWS-1252") %>% 
  filter(LANSNAMN == "Uppsala län") %>% 
  st_transform(4326)


### OSM Data
amenity = get_poi1(key = "amenity") %>% 
  select(kat, name, typ = amenity) %>% 
  mutate(display = paste0(kat, " - ", typ))

shop = get_poi1(key = "shop") %>% 
  select(kat, name, typ = shop) %>% 
  mutate(display = paste0(kat, " - ", typ))

leisure = get_poi1(key = "leisure") %>% 
  select(kat, name, typ = leisure) %>% 
  mutate(display = paste0(kat, " - ", typ))


##################################################################################
### Filter OSM data
##################################################################################

osm_sf_kultur = amenity %>% 
  filter(typ %in% c("arts_centre", "concert_hall", "theatre", "library", "place_of_worship")) %>% 
  mutate(display = paste0("kultur - ", typ))

osm_sf_skola = amenity %>% 
  filter(typ %in% c("kindergarten", "school", "university")) %>% 
  mutate(display = paste0("skola - ", typ))

osm_sf_health = amenity %>% 
  filter(typ %in% c("clinic", "dentist", "doctors", "hospital", "pharmacy")) %>% 
  mutate(display = paste0("health - ", typ))

osm_sf_shop = shop %>% 
  filter(typ %in% c("mall", "supermarket", "department_store"))

osm_sf_leisure = leisure %>% 
  filter(typ %in% c("golf_course", "nature_reserve", "park", "sports_centre", "stadium"))



### find and append relevant df in environment 
resultat_sf = bind_rows(mget(ls(pattern = "osm_sf_")))


### Fix encoding
resultat_sf$name = gsub("Ã¤", "ä", resultat_sf$name)
resultat_sf$name = gsub("Ã¶", "ö", resultat_sf$name)
resultat_sf$name = gsub("Ã–", "Ö", resultat_sf$name)
resultat_sf$name = gsub("Ã¥", "å", resultat_sf$name)
resultat_sf$name = gsub("Ã…", "Å", resultat_sf$name)
resultat_sf$name = gsub("Ã©", "é", resultat_sf$name)
resultat_sf$name = gsub("Ã¼", "ü", resultat_sf$name)


##################################################################################
### Visualise data
##################################################################################

karta = mapview(resultat_sf, zcol = "display")

print(karta)



##################################################################################
### Store data
##################################################################################

# karta
mapshot(karta, url = paste0(folder_output, "/map.html"))

# shapefile
st_write(resultat_sf, 
         paste0(folder_output, "/osm_poi.shp"), 
         layer_options = "ENCODING=UTF-8")


# dataframe
resultat_df = resultat_sf %>% bind_cols(resultat_sf %>% 
                                          st_coordinates() %>% 
                                          as.data.frame()) %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  remove_rownames()

write.csv2(resultat_df, paste0(folder_output, "/osm_poi.csv"), row.names = FALSE)