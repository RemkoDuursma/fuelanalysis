
# xlsx downloaded from:
# https://data.nsw.gov.au/data/dataset/fuel-check
# and saved in /rawdata

# ---- load_packages ----
pacman::p_load(readxl, dplyr, lubridate, ggmap)
if(!dir.exists("cache"))dir.create("cache")

# ---- read_fuel_raw ----
fuelrds <- "cache/fuel.rds"
if(!file.exists(fuelrds)){
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
  saveRDS(fuel, fuelrds)
} else {
  fuel <- readRDS(fuelrds)
}



#----- Get lat-long

latcache <- "cache/station_address_latlong.rds"
if(!file.exists(latcache)){

  addr <- unique(fuel$Address)
  
  # Get rid of anything in ()
  addr_re <- gsub("\\(.+\\)", "", addr)
  
  # Get rid of "Cnr of ...,", but not when address starts with that!
  addr_re <- gsub("(.+)(Cnr.+,)", "\\1", addr_re)
  
  # Add Australia though it seems unnecessary
  addr_re <- paste(addr_re, "Australia")
  
  # Hopefully don't go over API limit
  gcres <- geocode(addr_re, output="latlon")
  
  # 
  saveRDS(data.frame(Address = addr, Address_geo = addr_re, gcres),
          latcache)
  
  # Extra: done after two step-geolookingup
  #write.csv(gcres, "data/Fuel_NSW_addresses_latlon.csv", row.names=FALSE)
}



# Get rid of USA addresses.
# could also do
# options(geonamesUsername="remkoduursma")
# # and enable on geonames.org/manageaccounts
# GNcountryCode(g[1], g[2])
locs <- read.csv("data/Fuel_NSW_addresses_latlon.csv", stringsAsFactors = FALSE) %>%
  filter(lon > 120) %>%
  dplyr::select(-Address_geo)


#----- Nearest neighbours.
library(sp)
library(rgeos)
library(geosphere)

# Copy of simple dataframe 'locs', to a SpatialPointsDataframe
locs_sp <- locs
coordinates(locs_sp) <- ~lon+lat

# From geosphere, the correct way to calculate distances between spatial coordinates.
d <- distm(locs_sp)

# nxn distance matrix
mdist <- gDistance(locs_sp, byid=TRUE)

# Dist to nearest service station
min2 <- function(x)min(x[x > 0])  # exclude self; x > 0 
locs_sp$dist_1 <- apply(mdist, 1, min2)

# # Figure
# hist(log10(locs$dist_1), breaks=100, axes=F,
#      xlab="Distance to nearest service station (some degree unit)")
# library(magicaxis)
# magaxis(side=1, unlog=1)
# axis(2)

# how many other service stations <5km away (i.e. circle with diam=10km)
countd <- function(x, d)length(x[x < d])
locs_sp$nr_5km <- apply(d, 1, countd, d=5000)



#----- Voronois
library(deldir)
v <- deldir(locs$lon, locs$lat)

# Edge effect:
# windows(12,10)
# with(locs, plot(lon, lat, pch=16, col="red"))
# plot(v, wlines="tess", wpoints="none", lty=1, col="grey", add=TRUE)
# with(locs, points(lon, lat, pch=16, col="red"))

# Make NSW polygon
oz2 <- read.csv("http://www.remkoduursma.com/files/ozdata.csv")
nsw <- filter(oz2, state == "NSW") %>%
  dplyr::select(long, lat) %>% as.data.frame

coordinates(nsw) <- ~long + lat

nswp <- Polygon(nsw)
nswpg <- SpatialPolygons(list(Polygons(list(NSW=nswp), "NSW")))

# Using a zero-width buffer cleans up many topology problems in R.
nswpg <- gBuffer(nswpg, byid=TRUE, width=0)


# we use the coordinates returned by voronoi,
# because for some reason a few dozen polygons cannot be computed,
# perhaps some coordinates are identical??
coorsx <- v$summary[,c("x","y")]
coordinates(coorsx) <- ~x+y

library(SDraw)
vp <- voronoi.polygons(coorsx)

z <- gIntersection(vp, nswpg, byid=TRUE)

# Edge effect solved
# par(mar=c(0,0,0,0))
# plot(z)

# Now lookup area of each polygon
# We have to do this the hard way, because not all polygons
# are returned (perhaps some failed?).
get_area <- function(point, spoly){
  g <- gContains(spoly, point, byid=TRUE)
  if(any(g)){
    pol <- spoly[which(g)]
    if(!length(which(g)))browser()
    if(!is.null(pol))gArea(pol)
  } else {
    NA
  }
}

# Loop through points, look up Voronoi polygon areas.
# Note that `apply` won't work with a SpatialPointsDataframe,
# at least not like this.
area <- c()
for(i in 1:length(locs_sp))area[i] <- get_area(locs_sp[i,], spoly=z)

# Add to 'locs' dataframe with locations of Service Stations.
locs$area_voronoi <- area  





#---- Remoteness
library(dplyr)
library(janitor)

# The ALA service wants a Date added to the dataframe,
# since remoteness is a time-dependent variable. 
locsub <- filter(locs, lon, lat) %>%
  mutate(eventDate = "2016-6-1")

write.csv(locsub, "data/NSW_fuel_locations_lonlatonly.csv", row.names=FALSE)

# remoteness index from ALA
#http://spatial.ala.org.au/webportal//#
remo <- read.csv("data/NSW_fuel_ALA_remoteness_locations.csv", stringsAsFactors = FALSE) %>%
  clean_names %>%
  dplyr::select(locality, latitude_original, longitude_original,
                remoteness_index, distance_to_coast) %>%
  rename(Address = locality,
         lat=latitude_original,
         lon=longitude_original,
         remoteness=remoteness_index,
         dist_to_coast=distance_to_coast)
write.csv(remo, "data/NSW_fuel_ALA_remoteness_locations_cleaned.csv", row.names=F)

# And merge onto 'locs'.
remo_m <- dplyr::select(remo, Address, remoteness, dist_to_coast)
locs <- left_join(locs, remo_m, by="Address")


saveRDS(locs, "cache/locs.rds")
# Figure
# windows(6,5)
# library(oz)
# oz(sections=c(4, 13:15))
# cols <- colorRampPalette(c("yellow","darkorange","red"))(10)
# with(remo, points(lon,lat, pch=19, col=cols[cut(log(remoteness+1), 10)]))


# # Figure
# library(oz)
# 
# oz(sections=c(4,13:15))
# with(locs, points(lon,lat,pch=19, col=rev(heat.colors(10))[cut(log(area_voronoi),10)]))

#----- Final merge
fuel <- left_join(fuel, locs, by="Address")


