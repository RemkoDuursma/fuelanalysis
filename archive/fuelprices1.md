---
title: "Petrol prices in New South Wales - Part 1"
author: "Remko Duursma"
date: "19 December 2017"
output: html_document
---










### *How do petrol prices vary across the state?*

The price of fuel at service stations varies tremendously, both in space - across the large Australian state of New South Wales, and time - with surprising patterns. In this post I begin my quest to understand how fuel price varies, what factors explain its fluctuation over time and spatial variation. Obviously it would be nice to be able to know where to drive for cheaper fuel (what kind of areas are cheaper?), and when to fill up (should I wait until Thursday?).

Since August 2016, the NSW government runs the [FuelCheck service](https://www.fuelcheck.nsw.gov.au/app), which allows monitoring of fuel prices *in real time*. Several apps tap into this publicly available API, allowing users to find the cheapest fuel in the neighborhood, or inspect some simple graphs of fuel price over time.

As an additional service, (nearly) daily prices of all types of fuel, all brands of service stations, and **all locations** across the state can be downloaded from the [NSW Data portal](https://data.nsw.gov.au/), currently from August 2016 to October 2017. The dataset contains over one million records, for over 2000 service stations, and 11 fuel types. 


![](img/fuelcheck_web.png)

### Approach

This is the first in a series of posts on this hobby project to find out if we can *predict fuel prices in space and time*. What I want to know is:

- Is fuel cheaper on a particular day of the week? It is a widely-held belief that fuel prices are more expensive on the day that everyone receives their weekly paycheck (Thursday), but do the data support this? 

- Across the state, how does fuel price vary and why? Quick inspection of the data shows that remote areas are more expensive; how do we summarize that, and what else matters? As roughly 5 out of 7.5 million people in NSW live in the Sydney metropolitan area - that is two thirds of the population on 1.5% of the land area - I will look at Sydney and non-Sydney data separately for much of this analysis.

Instead of just showing results, these posts are very much about getting into the details of the R code to generate them. 

But before we get started, here is a taste of what's to come:





The data show a fluctuation in fuel prices [that is known](https://www.accc.gov.au/consumers/petrol-diesel-lpg/petrol-price-cycles#petrol-prices-in-sydney), but are not at all weekly. 






# Getting the Data

The FuelCheck service in New South Wales, Australia, provides real-time data on fuel prices at service stations across the state. [This page](https://data.nsw.gov.au/data/dataset/fuel-check) contains information, as well as monthly files containing fuel prices for all service stations, for all fuel types. The first step is to download all `xlsx` files, and save them locally. We then use `readxl` to read them all, and `dplyr` to tidy things up.


```r
pacman::p_load(readxl, dplyr, lubridate, ggmap,
               SDraw, deldir, sp, rgeos, geosphere,
               janitor, magicaxis)
```

And then


```r
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
```

You can skip this step as I have bundled the clean dataset in the R package `fuelpricensw`, which is available on [this Github repos](https://github.com/RemkoDuursma/fuelpricensw). 


```r
devtools::install_github("remkoduursma/fuelpricensw")
library(fuelpricensw)
data(fuel)
```



## Geocoding

The fuel price dataset contains street addresses of all service stations, but we would like to include latitude and longitude, so that other spatial attributes can easily be looked up. Here I used Google's geocode service, as made easily available in the `ggmap` package.


```r
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
```

Get rid of USA addresses. When geocode does not find a good match, Google returns some random address in the USA. Better approaches exist here, like the geonames service to find the country code.


```r
locs <- read.csv(latcache, stringsAsFactors = FALSE) %>%
  filter(lon > 120) %>%
  dplyr::select(-Address_geo)
```


## Distance to nearest competitor


```r
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
```


The distance to the nearest next service station peaks at 1km (1000m). The most remote service station appears to be 28 Columbus Street, Ivanhoe NSW 2878, though recall geocoding failed for some stations, so we don't have distance to all stations.


```r
hist(log10(locs$dist_1), breaks=100, axes=FALSE,
     main="", col="cornflowerblue",
     xlab="Distance to nearest service station (m)")
magicaxis::magaxis(side=1, unlog=1)
axis(2)
```

![plot of chunk hist_dist_1](figure/hist_dist_1-1.png)

## Area served : Voronoi polygons



```r
library(deldir)
voro_plain <- deldir(locs$lon, locs$lat)
```


```r
par(mar=c(3,3,1,1))
plot(voro_plain, wlines="tess", wpoints="real", 
     lty=1, col=c("black","grey","red","black","black"),
     cex=0.6, xlab="", ylab="")
box()
```

![plot of chunk voro_plain_fig](figure/voro_plain_fig-1.png)



```r
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
```



```r
par(mar=c(0,0,0,0))
plot(voro_buffer)
with(locs, points(lon, lat, pch=19, cex=0.5,
                  col=rev(heat.colors(10))[cut(log(area_voronoi),10)]))
```

![plot of chunk buffer_voronoi_fig](figure/buffer_voronoi_fig-1.png)



## Remoteness, distance to coast

Eventually I want to build a model that predicts fuel price based on location, and time of year. To do so, we have to start adding some features of interest. The Atlas of Living Australia provides a 'remoteness index', which seems interesting since at first sight fuel prices are much higher for more remote locations. Though the ALA provides API services, I did this the quick way by visiting [this page](http://spatial.ala.org.au/webportal/), uploading a CSV with lat and long, and downloading a CSV file with a remoteness index, and the distance to coast. You can read more about how the (unitless) remoteness index is [calculated here](http://spatial.ala.org.au/layers/more/aria).


```r
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
```



```r
locs2 <- dplyr::select(fuel, Address, Brand) %>% 
  distinct %>% left_join(locs, by="Address")

with(locs2, plot(dist_to_coast, remoteness, pch=19,
                col=as.factor(Brand)))
```

![plot of chunk remote_plot](figure/remote_plot-1.png)



```r
# Figure
library(oz)
oz(sections=c(4, 13:15))
cols <- colorRampPalette(c("yellow","darkorange","red"))(10)
with(locs, points(lon,lat, pch=19, col=cols[cut(log(remoteness+1), 10)]))
```


## Combined dataset


```r
fuel <- left_join(fuel, locs, by="Address")
```


















