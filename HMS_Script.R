
# Load packages: 
library(pacman)
p_load(tidyverse,sf,lubridate,progress,httr,plyr)

#data from 2023-01-01 to current (2023-05-28)
#construct df to cycle through
date_list <- seq.Date(as_date("2023-01-01"),as_date("2023-05-29"),by=1) %>%
  enframe(name=NULL) %>%
  mutate(year=str_sub(value,1,4),
         month=str_sub(value,6,7),
         fn_date=str_remove_all(value,"-"))

if(!dir.exists("cache")) dir.create("cache")
x=date_list[6924,] #for testing

# Apply the function to downlaod across all dates over the time period of interest
# Function to check if URL exists
urlExists <- function(web_addr) {
  response <- HEAD(web_addr)
  status <- response$status_code
  return(status == 200)
}

# Iterate over the URLs and download files
date_list %>%
  group_split(rn = row_number()) %>%
  map(function(x) {
    web_addr = str_c("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/",
                     x$year, "/",
                     x$month, "/hms_smoke", x$fn_date, ".zip")
    
    if (urlExists(web_addr)) {
      download.file(url = web_addr, destfile = str_c("cache/", basename(web_addr)))
      message("File downloaded:", web_addr)
    } else {
      message(paste("URL does not seem to exist:", web_addr))
    }
  })

# Unzip the files: 
cache = "/Users/joshvirene/Desktop/Desktop/NOAA/cache"
if(!dir.exists("unzipped")) dir.create("unzipped")
outDir = "/Users/joshvirene/Desktop/Desktop/NOAA/unzipped"
zipfiles = list.files(path = cache, pattern = '*.zip', full.names = TRUE)
ldply(.data = zipfiles, .fun = unzip, exdir = outDir)

# Put the shapefiles into their own folder: 
# Particularly, we want the shapefiles, so we isolate these into their own folder "shapefiles"
if(!dir.exists("shapefiles")) dir.create("shapefiles")
shapefile_path = "/Users/joshvirene/Desktop/Desktop/NOAA/shapefiles"
shps = list.files(path = outDir, pattern = ".shp", full.names = TRUE)
copy_results <- file.copy(from = shps, to = shapefile_path)

# End of the script to download files: 

# Analysis: 
AOI = st_read(dsn = "ca-state-boundary/CA_State_Tiger2016.shp")
AOI = AOI$geometry

# Set crs for AOI
standard_crs = "+proj=longlat +datum=WGS84"
AOI = st_transform(AOI, st_crs(standard_crs))

# Read in polygon geometries into a large list 
poly_geometries <- list()
for (i in shps){
  sf_data <- read_sf(i)
  poly_geometries[[i]] <- st_geometry(sf_data)
}

# Set crs for the smoke plumes: 
poly_geometries <- lapply(poly_geometries, st_transform, crs = standard_crs)

#
intersection_check_list = list() # create an empty list to be populated with sgbp lists
for (i in poly_geometries){
  intersection_check <- st_intersects(AOI, i)
  intersection_check_list <- append(intersection_check_list, intersection_check)
}

