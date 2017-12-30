
if(!dir.exists("cache"))dir.create("cache")

# xlsx downloaded from:
# https://data.nsw.gov.au/data/dataset/fuel-check
# and saved in /rawdata

# ---- load_packages ----
pacman::p_load(readxl, dplyr, lubridate, ggmap,
               SDraw, deldir, sp, rgeos, geosphere,
               janitor, magicaxis)

# ---- read_fuel_raw ----

# Don't have to run this, since the clean raw data is bundled
# in the fuelpricensw package (github: remkoduursma/fuelpricensw)
if(FALSE){
  readx <- function(x){
    res <-  read_excel(x)
    if(names(res)[2] == "X__1"){
      res <-  read_excel(x, skip=1)   
    }
    
    res <- mutate(res, Postcode  = as.numeric(Postcode))
    if("FuelType" %in% names(res)){
      res <- rename(res, FuelCode = FuelType)
    }
    
  return(res)
  }
  
  fuel <- lapply(dir("rawdata", pattern="xlsx", full.names = TRUE), readx) %>%
    bind_rows %>%
    mutate(DateTime = ymd_hms(PriceUpdatedDate),
           Date = as.Date(DateTime)) %>%
    dplyr::select(-PriceUpdatedDate) %>%
    filter(Price < 500)

}



# ---- get_lat_long ----
latcache <- "data/Fuel_NSW_addresses_latlon.csv"
if(!file.exists(latcache)){

  # Unique addresses to look up.
  addr <- unique(fuel$Address)
  
  # After some failures, I found that extra info between () messes with 
  # the geocode service. Remove them.
  addr_re <- gsub("\\(.+\\)", "", addr)
  
  # Get rid of "Cnr of ...,", but not when address starts with it.
  addr_re <- gsub("(.+)(Cnr.+,)", "\\1", addr_re)
  
  # Add Australia though it seems unnecessary
  addr_re <- paste(addr_re, "Australia")
  
  # Now run the service.
  gcres <- geocode(addr_re, output="latlon")
  
  # Code not shown: run code twice on separate days,
  # since we go over the API use limit.
  write.csv(gcres, latcache, row.names=FALSE) 
  
}


# ---- filter_locs ----
locs <- read.csv(latcache, stringsAsFactors = FALSE) %>%
  filter(lon > 120) %>%
  dplyr::select(-Address_geo)


# ---- dontshow ----
library(fuelpricensw)
data(fuel)


# ---- nearest_neighbours ----
library(sp)
library(rgeos)
library(geosphere)

# Copy of simple dataframe 'locs', to a SpatialPointsDataframe.
# Note how we assign new variables to 'locs', which is a simple 
# dataframe.
locs_sp <- locs
coordinates(locs_sp) <- ~lon+lat

# From geosphere, the correct way to calculate distances between spatial coordinates.
geo_dist <- distm(locs_sp)

# How many other service stations <5km away
countd <- function(x, dist)length(x[x < dist])
locs$nr_5km <- apply(geo_dist, 1, countd, d = 5000)

# Dist to nearest service station
min2 <- function(x)min(x[x > 0])  # exclude self; x > 0 
locs$dist_1 <- apply(geo_dist, 1, min2)


# ---- plain_voronoi ----
library(deldir)
voro_plain <- deldir(locs$lon, locs$lat)


# ---- buffer_voronoi ----

# Make NSW polygon
# Similar to oz::oz(), but coordinates are ordered by state.
oz2 <- read.csv("http://www.remkoduursma.com/files/ozdata.csv")
nsw <- filter(oz2, state == "NSW") %>%
  dplyr::select(long, lat) %>% as.data.frame

# Convert to SpatialPolygonsDataframe
coordinates(nsw) <- ~long + lat
nswp <- Polygon(nsw)
nswpg <- SpatialPolygons(list(Polygons(list(NSW=nswp), "NSW")))

# Using a zero-width buffer cleans up many topology problems in R.
nswpg <- rgeos::gBuffer(nswpg, byid=TRUE, width=0)

# We use the coordinates returned by voronoi to assign voronoi
# areas for each service station, because for some reason a few dozen 
# polygons cannot be computed (so simple cbind is not possible).
coorsx <- voro_plain$summary[,c("x","y")]
coordinates(coorsx) <- ~x+y

# Voronoi polygons with a 
library(SDraw)
vp <- voronoi.polygons(coorsx)
voro_buffer <- gIntersection(vp, nswpg, byid=TRUE)

# Now lookup area of each polygon
# We have to do this the hard way, because not all polygons
# are returned (perhaps some failed?).
get_area <- function(point, spoly){
  g <- gContains(spoly, point, byid=TRUE)
  if(any(g)){
    pol <- spoly[which(g)]
    if(!is.null(pol)){
      geosphere::areaPolygon(pol)
    }
  } else {
    NA
  }
}

# Loop through points, look up Voronoi polygon areas.
# Note that `apply` won't work with a SpatialPointsDataframe,
# at least not like this.
area <- c()
for(i in 1:length(locs_sp))area[i] <- get_area(locs_sp[i,], spoly=voro_buffer)

# Add to 'locs' dataframe with locations of Service Stations.
locs$area_voronoi <- area





# ---- remoteness ----
library(dplyr)
library(janitor)

# Subset of one date, to get unique locations only.
# The ALA service wants a Date added to the dataframe,
# since remoteness is a time-dependent variable. 
locsub <- filter(locs, lon, lat) %>%
  mutate(eventDate = "2016-6-1")

# Save to disk...
write.csv(locsub, "data/NSW_fuel_locations_lonlatonly.csv", row.names=FALSE)

# ... so that it can be uploaded at the ALA:
# http://spatial.ala.org.au/webportal//#
# See that page for help on how to select variables. I just picked
# 'remoteness', and distance to coast, which I saved again locally:
remo <- read.csv("data/NSW_fuel_ALA_remoteness_locations.csv", stringsAsFactors = FALSE) %>%
  clean_names %>%
  dplyr::select(locality, latitude_original, longitude_original,
                remoteness_index, distance_to_coast) %>%
  rename(Address = locality,
         lat = latitude_original,
         lon = longitude_original,
         remoteness = remoteness_index,
         dist_to_coast = distance_to_coast) %>%
  mutate(dist_to_coast = 100 * dist_to_coast)
write.csv(remo, "data/NSW_fuel_ALA_remoteness_locations_cleaned.csv", row.names=F)

# And merge onto 'locs'.
remo_m <- dplyr::select(remo, Address, remoteness, dist_to_coast)
locs <- left_join(locs, remo_m, by="Address")


#---- final_merge ----
fuel <- left_join(fuel, locs, by="Address")


