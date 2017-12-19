
pacman::p_load(readxl, dplyr, lubridate, ggplot2, 
               gplots, zoo, ggmap, data.table, janitor)

if(!dir.exists("cache"))dir.create("cache")


ala <- read.csv("NSW_fuel_ALA_remoteness_locations_cleaned.csv",
                stringsAsFactors = FALSE)
ala_m <- dplyr::select(ala, -lat, -lon)


fuel <- fread("NSW_fuelprices_with_latlon.csv") %>%
  clean_names %>%
  left_join(ala_m, by="address") %>%
  mutate(date = as.Date(date),
         datetime = ymd_hms(datetime))


fuel91s <- filter(fuel, fuelcode == "U91") %>%
  group_by(address) %>%
  summarize(price = median(price, na.rm=TRUE),
            remote=median(remoteness),
            dist_to_coast=median(dist_to_coast))

with(fuel91s, plot(log(remote+1), price,
                   ylim=c(100,150)))
abline(lm(price ~ I(log(remote+1)), data=fuel91s))



library(sp)
library(rgeos)
library(geosphere)
locs <- ala[,c("address","lon","lat")]
coordinates(locs) <- ~lon+lat
d <- distm(locs)

# nxn distance matrix
mdist <- gDistance(locs, byid=TRUE)

# Dist to nearest service station
min2 <- function(x)min(x[x > 0])  # exclude self; x > 0 
locs$dist_1 <- apply(mdist, 1, min2)

# Figure
hist(log10(locs$dist_1), breaks=100, axes=F,
     xlab="Distance to nearest service station (some degree unit)")
library(magicaxis)
magaxis(side=1, unlog=1)
axis(2)


# how many other service stations <5km away (i.e. circle with diam=10km)
countd <- function(x, d)length(x[x < d])
locs$nr5 <- apply(d, 1, countd, d=5000)

# Figure
hist(locs$nr5, main="")
title(main="how many other service stations\n <5km away (i.e. circle with diam=10km)")

# then finally (to add lon lat back in!)
locs <- as.data.frame(locs)


# probably focus on only top four fuel types
sort(table(fuel$fuelcode))

# probably use brand as a predictor, but combine all the minor ones (?), with forcats
sort(table(fuel$brand))


#-- New subset, only major fuels.
fuel2 <- filter(fuel, fuelcode %in% c("P95","U91","P98","E10"))
hist(fuel2$price)

# would like to use weekday as predictor, but be careful of imbalance:
table(fuel2$weekday)


# imbalance differ with brand? fuel type?

# other layers
# - population density (covered by remoteness though)
# - income levels
# - distance to major road (pennant hills effect)




# how about some voronois
library(deldir)

coors <- fuel2[,c("address","lon", "lat")] %>%
  distinct(address, .keep_all=TRUE) %>%
  rename(x=lon, y=lat) %>%
  filter(!is.na(x))
v <- deldir(coors)


# voronois
windows(12,10)
with(coors, plot(x,y,pch=16, col="red"))
plot(v, wlines="tess", wpoints="none", lty=1, col="grey", add=TRUE)
with(coors, points(x,y,pch=16, col="red"))
# !! problem: edge effects are enormous... should be some way to trim
# the voronois


# so instead use SDraw::voronoi.polygons(., bounding.polygon=)


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


# spatial points object with square observation window
xy  <- as.data.frame(coorsx)
library(spatstat)
pp <- ppp(xy[,1],xy[,2],
          xrange=c(min(xy$x),max(xy$x)),
          yrange=c(min(xy$y),max(xy$y)))
plot(pp)


# with NSW as observation window
library(maptools)
nsw_owin <- as(nswpg, "owin")
pp <- ppp(xy[,1],xy[,2],
          window=nsw_owin)
plot(pp)

# density plot
plot(density(pp))

# quadrant count
qc_nsw <- quadratcount(pp, nx=8, ny=5)
plot(qc_nsw)


# intensity of quadrat count
qc_nsw_int <- intensity(qc_nsw, image=TRUE)
plot(qc_nsw_int)

# Ripley's K
pp_k <- Kest(pp) # slow
plot(pp_k)

# too slow
#pp_env <- envelope(pp,Kest)
#plot(pp_env)


#?
fit1 <- ppm(pp, ~x)
lam <- predict(fit1)

#?
plot(Kscaled(pp, lam))


#---> look at strauss process



#-- Voronoi polygons
library(SDraw)
vp <- voronoi.polygons(coorsx)

z <- gIntersection(vp, nswpg, byid=TRUE)

par(mar=c(0,0,0,0))
plot(z)


plot_i <- function(i){
  p <- coors[i,]
  coordinates(p) <- ~x+y
  g <- gContains(z, p, byid=T)
  plot(z)
  plot(z[which(g)], add=TRUE, col="red")
  points(p, col="blue", pch=19, cex=2)
}
par(mar=c(0,0,0,0))
plot_i(860)




get_area <- function(point, spoly){
  g <- gContains(spoly, point, byid=T)
  if(any(g)){
    pol <- spoly[which(g)]
    if(!length(which(g)))browser()
    if(!is.null(pol))gArea(pol)
  } else {
    NA
  }
}

newp <- coors
coordinates(newp) <- ~x+y

area <- c()
for(i in 1:length(newp))area[i] <- get_area(newp[i,], spoly=z)  
coors$area <- area  
  

# Figure
library(oz)

oz(sections=c(4,13:15))
with(coors, points(x,y,pch=19, col=rev(heat.colors(10))[cut(log(area),10)]))



