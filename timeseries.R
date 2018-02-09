
load("cache/fuel_objects.RData")

pacman::p_load(dplyr)

fuelu91 <- filter(fuel, FuelCode == "U91")

fuelu91s <- split(fuelu91, fuelu91$Address)

x <- fuelu91s[[200]]
#with(x, plot(DateTime, Price))

z <- x
z <- z[order(z$DateTime),]
z$up <- c(NA, diff(z$Price) > 10)

z$up_pre <- FALSE
z$up_pre[which(z$up)-1] <- TRUE

with(z, plot(DateTime, Price, pch=19, type='o'))
with(z[z$up,], points(DateTime, Price, col="red", pch=19))
with(z[z$up_pre,], points(DateTime, Price, col="blue", pch=19))

# Filter single spikes:
with(z[70:100,], plot(DateTime, Price, pch=19, type='o'))
with(z[z$up,], points(DateTime, Price, col="red", pch=19))
with(z[z$up_pre,], points(DateTime, Price, col="blue", pch=19))

ii <- which(z$up)
pre <- ii-1
post <- ii+1
single_spike <- ii[z$up[ii] & z$up_pre[pre] & z$up_pre[post]]
z$up[single_spike] <- FALSE
z$up_pre[single_spike - 1] <- FALSE

with(z, plot(DateTime, Price, pch=19, type='o'))
with(z[z$up,], points(DateTime, Price, col="red", pch=19))
with(z[z$up_pre,], points(DateTime, Price, col="blue", pch=19))





x <- fuelu91s[[200]]
#with(x, plot(DateTime, Price))

make_cycle_dfr <- function(stationdata, plotit=FALSE){

  z <- stationdata
  
  # Find price hikes, when > 10ct increase since last record.
  # This assumes absence of lengthy NAs, and that price hikes are 
  # always > 10ct (TRUE for metropolitan areas)
  z <- z[order(z$DateTime),]
  z$up <- c(NA, diff(z$Price) > 10)

  # Which data points occurred just before a price hike  
  z$up_pre <- FALSE
  z$up_pre[which(z$up)-1] <- TRUE
  
  # Frequently a single spike; bad data for some reason.
  # This filters those points that went up, down, up.
  ii <- which(z$up)
  pre <- ii-1
  post <- ii+1
  single_spike <- ii[z$up[ii] & z$up_pre[pre] & z$up_pre[post]]
  z$up[single_spike] <- FALSE
  z$up_pre[single_spike - 1] <- FALSE
  
  
  ups <- which(z$up)
  l <- list()
  for(i in seq_along(ups[-length(ups)])){
    
    dat <- z[ups[i]:ups[i+1],]
    price_pre <- z$Price[ups[i]-1]
    price_peak <- z$Price[ups[i]]
    price_low <- z$Price[ups[i+1] - 1]
    date_peak <- as.Date(z$DateTime)[ups[i]]
    date_pre <- as.Date(z$DateTime)[ups[i]-1]
    date_low <- as.Date(z$DateTime)[ups[i+1] - 1]
    ndays <- as.numeric(date_low - date_pre)
    
    l[[i]] <- data.frame(Address = unique(stationdata$Address),
                          price_pre = price_pre,
                         price_peak = price_peak,
                         price_hike = price_peak - price_pre,
                         price_low = price_low,
                         date_peak = date_peak,
                         date_pre = date_pre,
                         date_low = date_low,
                         ndays_cycle = ndays,
                         dpricedt = (price_low - price_peak) / ndays,
                         ndata = nrow(dat)
                         )
  }
  dfr <- do.call(rbind, l)

  
  if(plotit){
    
    
    with(z, plot(as.Date(DateTime), Price, pch=19, col="grey"))  
      if(!is.null(dfr)){
      with(dfr, points(date_peak, price_peak, pch=19, col="red"))  
      with(dfr, points(date_pre, price_pre, pch=19, col="blue"))
      
      for(j in 1:nrow(dfr)){
        xx <- c(dfr$date_peak[j], dfr$date_low[j])
        yy <- c(dfr$price_peak[j], dfr$price_low[j])
        lines(xx,yy)
      }
      }
  } else {
    return(dfr)
  }
}


with(fuelu91s[[201]], plot(DateTime, Price))

res <-make_cycle_dfr(fuelu91s[[201]], plotit=T)



library(ggmap)

syd_map <- get_map(c(lon = 151, lat=-33.8), zoom=10)

syd_vert <- read.table(text="
                   lon lat
                   150.65 -34.1
                   150.65 -33.55
                   150.9 -33.55
                   151 -33.7
                   151.35 -33.7
                   151.27 -34.15
                   150.95 -33.95
                   150.8 -34.1",header=TRUE)

ggmap(syd_map) + 
  geom_polygon(aes(x=lon,y=lat), data=syd_vert, alpha=0.2)


library(sp)

in_syd <- point.in.polygon(locs$lon, locs$lat,
                           syd_vert$lon, syd_vert$lat)
locsyd <- locs[in_syd == 1,]

fuelsyd <- filter(fuel, Address %in% locsyd$Address)

fuelu91syd <- filter(fuelsyd, FuelCode == "U91")

fuelu91syds <- split(fuelu91syd, fuelu91syd$Address)


plot1 <- function(i){
  make_cycle_dfr(fuelu91syds[[i]], plotit=TRUE)
  title(main=paste(i, " -- ", locsyd$Address[i]), 
        cex.main=0.8, font.main=1)
}
plot1(149)


pdf("syd_timeseries_cycled.pdf", width=9, height=6)
for(i in 1:length(fuelu91syds)){
  message(i)
  plot1(i)
}
dev.off()



cycsyd <- lapply(fuelu91syds, make_cycle_dfr) %>%
  bind_rows %>% left_join(locsyd, by="Address") %>%
  filter(ndata > 10, price_hike < 40)


cycsyd_a <- group_by(cycsyd, Address) %>%
  summarize(price_hike_median = median(price_hike),
            dpricedt = mean(dpricedt),
            lon = mean(lon), lat=mean(lat), 
            nr_5km = mean(nr_5km)
            )


with(cycsyd, plot(lat, price_hike))

with(cycsyd_a, plot(nr_5km, price_hike_median))
with(cycsyd_a, plot(nr_5km, dpricedt))

hist(cycsyd$dpricedt, xlim=c(-2,0), breaks=500)



# interesting : 1, 185, 208, 279
dat <- fuelu91syds[[279]]
dat <- dat[order(dat$DateTime),]
dat <- group_by(dat, Date) %>% summarize(Price = mean(Price)) %>%
  as_data_frame

difs <- c(NA, diff(dat$Price))
with(dat, plot(Date, Price, type='o', pch=21, bg="white"))
with(dat[difs>0,], points(Date, Price, pch=19))


# sp <- with(dat, spline(x=Date, y=Price, method="natural"))
# with(sp, lines(x,y, col="red"))


# TRUE if x[i] > x[i-1] AND x[i] > x[i + 1]
# (vectorized)
local_maximum <- function(x){
  c(NA, x[2:length(x)] > x[1:(length(x)-1)]) &
    c(x[1:(length(x)-1)] > x[2:length(x)], NA)
}

ij <- which(local_maximum(dat$Price))
with(dat[ij,], points(Date, Price, pch=19, col="red"))


# TRUE if x[i-1] > x[i+1] AND x[i] < x[i-1] AND x[i] < x[i+1]
#x <- c(1,2,4,3,2,1,0,6,3.2,4,3,2,8,7,6,3.4,4,3)
local_minimum <- function(x){
  c(x[2:length(x)] > x[1:(length(x)-1)], NA) & 
    c(NA, x[2:length(x)] < x[1:(length(x)-1)])
}

lt <- local_minimum(dat$Price)
with(dat[lt,], points(Date, Price, pch=17, col="forestgreen"))

# ij <- which(local_trough(x))
# plot(x, col=c("blue","red")[as.factor(local_trough(x))])


make_cycledf <- function(stationdata, plotit=FALSE){

  zall <- stationdata[order(stationdata$DateTime),]
  
  z <- group_by(zall, Date) %>% summarize(Price = last(Price)) %>%
    as_data_frame
  
  l <- list()
  
  #incs <- c(NA, diff(z$Price))
  #z <- z[incs <= 0 | incs > 4,]
  
  # steep sudden decrease; adds noise and likely glitches
  spike_down <- which(diff(z$Price) < -10)
  if(length(spike_down))z <- z[-spike_down,]
  
  #... again seems to proper fix it
  spike_down <- which(diff(z$Price) < -10)
  if(length(spike_down))z <- z[-spike_down,]
  
  # local minimum glitches when no price change
  dzero <- which(diff(z$Price) == 0)
  if(length(dzero))z <- z[-dzero,]
  
  # price increases of less than 1ct,
  # perhaps artefact due to averaging by day?
  d <- diff(z$Price)
  dtiny <- which(d > 0 & d < 1)
  if(length(dtiny))z <- z[-dtiny,]
  
  # not sure why, but needs one more go
  d <- diff(z$Price)
  dtiny <- which(d > 0 & d < 1)
  if(length(dtiny))z <- z[-dtiny,]
  
  
  if(nrow(z) < 20)return(invisible(NULL))
  
  imax <- which(local_maximum(z$Price))
  imin <- which(local_minimum(z$Price))
  
  for(i in 1:length(imax)){
    
    # find next local minimum
    next_imin <- suppressWarnings(min(imin[imin > imax[i]]))
    if(!is.finite(next_imin))next
    prev_imin <- suppressWarnings(max(imin[imin < imax[i]]))
    if(!is.finite(prev_imin))prev_imin <- NA
    
    dat <- z[imax[i]:next_imin,]
    
    price_pre <- unique(z$Price[prev_imin])
    price_peak <- dat$Price[1]
    price_low <- dat$Price[nrow(dat)]
    date_pre <- unique(z$Date[prev_imin])
    date_peak <- dat$Date[1]
    date_low <- dat$Date[nrow(dat)]
    ndays <- as.numeric(date_low - date_peak)
    ndata <- nrow(subset(z, Date >= date_peak & Date <= date_low))
    
    l[[i]] <- data.frame(Address = unique(zall$Address),
                         price_pre = price_pre,
                         price_peak = price_peak,
                         price_hike = price_peak - price_pre,
                         price_low = price_low,
                         date_peak = date_peak,
                         date_pre = date_pre,
                         date_low = date_low,
                         ndays_cycle = ndays,
                         ndata_cycle = ndata,
                         dpricedt = (price_low - price_peak) / ndays
    )
  }
  
  out <- bind_rows(l)

  if(plotit){
    
    with(z, plot(Date, Price, pch=19, col="grey"))  
    
    if(!is.null(out)){
      with(out, points(date_peak, price_peak, pch=19, col="red"))  
      with(out, points(date_pre, price_pre, pch=19, col="blue"))
      
      for(j in 1:nrow(out)){
        xx <- c(out$date_peak[j], out$date_low[j])
        yy <- c(out$price_peak[j], out$price_low[j])
        lines(xx,yy)
        
        xx <- c(out$date_pre[j], out$date_peak[j])
        yy <- c(out$price_pre[j], out$price_peak[j])
        lines(xx,yy, lty=5)
      }
    }
  } 
  
return(invisible(out))
  
}

stationdata <- fuelu91syds[[544]]

x <- make_cycledf(fuelu91syds[[544]], plotit=T)



#!!!! 463 is interesting; massive price hike every day from morning to evening


plot1 <- function(i){
  m <- make_cycledf(fuelu91syds[[i]], plotit=TRUE)
  if(!is.null(m)){
    title(main=paste(i, " -- ", locsyd$Address[i]), 
          cex.main=0.8, font.main=1)
  }
}
plot1(1)


pdf("syd_timeseries_cycled.pdf", width=9, height=6)
for(i in 1:length(fuelu91syds)){
  message(i)
  plot1(i)
}
dev.off()


fuelkey <- dplyr::select(fuel, Address, Brand) %>% distinct

locsyd2 <- left_join(locsyd, fuelkey, by="Address")


cycsyd <- lapply(fuelu91syds, make_cycledf) %>%
  bind_rows %>% left_join(locsyd2, by="Address") %>%
  filter(ndata_cycle > 5, price_hike > 5) %>% 
  mutate(Brand2 = forcats::fct_lump(as.factor(Brand), 6))


cycsyd_a <- group_by(cycsyd, Address) %>%
  summarize(price_hike_median = median(price_hike, na.rm=T),
            price_peak_median = median(price_peak, na.rm=T),
            price_low_median = median(price_low, na.rm=T),
            ndays_cycle_median = median(ndays_cycle),
            dpricedt = mean(dpricedt),
            lon = mean(lon), lat=mean(lat), 
            nr_5km = mean(nr_5km),
            Brand = first(Brand),
            Brand2 = first(Brand2)
  )


group_by(cycsyd_a, Brand) %>%
  filter(n() > 25) %>% ungroup %>%
  mutate(Brand = reorder(Brand, price_hike_median, 
                         function(x)-median(x, na.rm=TRUE))) %>%
  ggplot(aes(x=Brand, y=price_hike_median)) +
  geom_boxplot() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="", y="Median price hike ($ cent)") + 
  ylim(c(0,40))




with(cycsyd_a, plot(nr_5km, price_hike_median))




library(ggmap)
syd_map <- get_map(c(lon = 151, lat=-33.8), zoom=10)


windows(10,8)
ggmap(syd_map) + 
  geom_polygon(aes(x=lon,y=lat), data=syd_vert, alpha=0.15) +
  geom_point(aes(x=lon, y=lat, fill=Brand2), 
             col="dimgrey",
             data=cycsyd_a, size=3, shape=21) + 
  scale_fill_manual(values=RColorBrewer::brewer.pal(7,"Set3")) 




# one outlier (few data points?) messes up the scale
windows(10,8)
ggmap(syd_map) + 
  geom_polygon(aes(x=lon,y=lat), data=syd_vert, alpha=0.15) +
  geom_point(aes(x=lon, y=lat, fill=price_low_median), 
             col="dimgrey",
             data=cycsyd_a, size=3, shape=21) + 
  scale_fill_gradientn(colours=rev(heat.colors(10))) 



windows(10,8)
ggmap(syd_map) + 
  geom_polygon(aes(x=lon,y=lat), data=syd_vert, alpha=0.15) +
  geom_point(aes(x=lon, y=lat, fill=dpricedt), 
             col="dimgrey",
             data=cycsyd_a, size=3, shape=21) + 
  scale_fill_gradientn(colours=rev(heat.colors(10))) 


palette(scales::alpha(RColorBrewer::brewer.pal(7,"Set3"), 0.7))
mns <- seq(as.Date("2016-9-1"), as.Date("2018-8-1"), by="1 month")
with(cycsyd, plot(date_peak, price_hike,
                  xlab="", ylab="Price hike ($ cent)",
                  ylim=c(0,40),
                  panel.first=abline(v=mns, col="grey"),
                  axes=FALSE,
                  pch=19, col=Brand2))
axis(2, las=2)
axis.Date(1, at=mns, format="%b-'%y", las=3, cex.axis=0.7)
box()



# Some functions for working with logical data. Not actually used above...
locsyd[279,]
grep("Mcgraths", locsyd$Address, ignore.case = T)

vec <- c(T, F, F, T, T, F, T, F, F, F, T, T, T, F, T)

equal_to_previous <- function(x){
  out <- x[2:length(x)] == x[1:(length(x)-1)]
  
c(NA, out)  
}

equal_to_next <- function(x){
  out <- x[1:(length(x)-1)] == x[2:length(x)]
  
  c(out, NA)  
}

isolated <- function(x){
  !equal_to_previous(x) & !equal_to_next(x)
}

neighboured <- function(x){
  equal_to_previous(x) & equal_to_next(x)
}

first_in_run <- function(x){
  !equal_to_previous(x) & equal_to_next(x)
}
  
last_in_run <- function(x){
  equal_to_previous(x) & !equal_to_next(x)
}





 
library(zoo)
fuelu91syd_ave <- mutate(fuelu91syd,
                         Brand2 = forcats::fct_lump(as.factor(Brand), 6)) %>% 
  group_by(Brand2, Date) %>%
  summarize(Price = mean(Price, na.rm=TRUE)) %>% ungroup %>%
  group_by(Brand2) %>% 
  mutate(Price_k3 = rollmean(Price, k=3, align="center", na.rm=TRUE, na.pad=TRUE))

# library(yarrr)
# unname(piratepal(palette="xmen")[1:7])


#devtools::install_github("dill/beyonce")
library(beyonce)


windows(10,8)
ggplot(fuelu91syd_ave, aes(x=Date, y=Price_k3, col=Brand2)) +
  geom_line(lwd=0.9) +
  theme_bw() +
  scale_colour_manual(values=c(beyonce_palette(127), "goldenrod"))








