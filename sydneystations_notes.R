

library(rgdal)
shp <- readOGR("data/syd_shp")
syd <- subset(shp, GCC_NAME16 == "Greater Sydney")

syd_xy <- ggplot2::fortify(syd)



library(ggmap)

syd_map <- get_map(c(lon = 150.9, lat = -33.8), 
                              source = "google", zoom=10, maptype="road")

ggmap(syd_map) + geom_path(aes(x=long, y=lat), data=syd_xy)

library(dplyr)


syd_map <- get_map(c(lon = 150.8, lat = -33.75), 
                   source = "google", zoom=9, maptype="road")

windows()
ggmap(syd_map) + geom_path(aes(x=long, y=lat), 
                            data=filter(syd_xy, group == "1.1"))


# ring polygon
with(syd_xy, plot(long, lat, pch=16, cex=0.2, col=group))


proj4string(coorsx) <- proj4string(syd)


library(sf)

coorsx_sf <- st_as_sfc(coorsx)
  
shp_sf <- st_read("data/syd_shp")
syd_sf <- subset(shp_sf, GCC_NAME16 == "Greater Sydney")
syd_sf2 <- st_as_sfc(syd_sf)

ii <- st_contains(  syd_sf, coorsx_sf)

