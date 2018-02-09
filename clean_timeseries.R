load("cache/fuel_objects.RData")


#----- load_packages -----
# fuel, locs only, though!

library(ggmap)
library(sp)
library(dplyr)


#----- prepare_data -----
# Get Sydney stations. Only do U91 (most commonly reported fuel type)
syd_map <- get_map(c(lon = 151, lat=-33.8), zoom=10)

# Manually entered polygon for 'Sydney', excluding blue mountains etc.
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


# Select stations in this polygon
in_syd <- sp::point.in.polygon(locs$lon, locs$lat,
                           syd_vert$lon, syd_vert$lat)
locsyd <- locs[in_syd == 1,]

# Sydney data
fuelsyd <- filter(fuel, Address %in% locsyd$Address)

# ... U91 only
fuelu91syd <- filter(fuelsyd, FuelCode == "U91")

# List of dataframes. Easier to work with.
fuelu91syds <- split(fuelu91syd, fuelu91syd$Address)


#----- cycled_functions -----

# TRUE if x[i] > x[i-1] AND x[i] > x[i + 1]
local_maximum <- function(x){
  c(NA, x[2:length(x)] > x[1:(length(x)-1)]) &
    c(x[1:(length(x)-1)] > x[2:length(x)], NA)
}

# TRUE if x[i-1] > x[i+1] AND x[i] < x[i-1] AND x[i] < x[i+1]
local_minimum <- function(x){
  c(x[2:length(x)] > x[1:(length(x)-1)], NA) & 
    c(NA, x[2:length(x)] < x[1:(length(x)-1)])
}


# Convoluted, but hey it works!
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

#----- make_cycled_data -----


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


#----- cycled_figures -----

# figure
x <- make_cycledf(fuelu91syds[[544]], plotit=TRUE)


# plot all
plot1 <- function(i){
  m <- make_cycledf(fuelu91syds[[i]], plotit=TRUE)
  if(!is.null(m)){
    title(main=paste(i, " -- ", locsyd$Address[i]), 
          cex.main=0.8, font.main=1)
  }
}

pdf("syd_timeseries_cycled.pdf", width=9, height=6)
for(i in 1:length(fuelu91syds)){
  message(i)
  plot1(i)
}
dev.off()

group_by(cycsyd_a, Brand) %>%
  filter(n() > 25) %>% ungroup %>%
  mutate(Brand = reorder(Brand, price_hike_median, 
                         function(x)-median(x, na.rm=TRUE))) %>%
  ggplot(aes(x=Brand, y=price_hike_median)) +
  geom_boxplot() + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="", y="Median price hike ($ cent)") + 
  ylim(c(0,40))




ggmap(syd_map) + 
  geom_polygon(aes(x=lon,y=lat), data=syd_vert, alpha=0.15) +
  geom_point(aes(x=lon, y=lat, fill=Brand2), 
             col="dimgrey",
             data=cycsyd_a, size=3, shape=21) + 
  scale_fill_manual(values=RColorBrewer::brewer.pal(7,"Set3")) 


# one outlier (few data points?) messes up the scale
cycsyd_a_2 <- filter(cycsyd_a, price_low_median < 125)
ggmap(syd_map) + 
  geom_polygon(aes(x=lon,y=lat), data=syd_vert, alpha=0.15) +
  geom_point(aes(x=lon, y=lat, fill=price_low_median), 
             col="dimgrey",
             data=cycsyd_a_2, size=3, shape=21) + 
  scale_fill_gradientn(colours=rev(heat.colors(10))) 


# density map of fuel stations
# based on https://app.dominodatalab.com/u/seanlorenz/ggmap-demo/view/ggmap-demo-heat.R
ggmap(syd_map) + 
  geom_density2d(data = cycsyd_a, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = cycsyd_a, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

library(viridis)
ggmap(syd_map) + 
  stat_summary_2d(data = cycsyd_a, aes(x=lon, y=lat, z=price_low_median)) + 
  scale_fill_viridis()


ggmap(syd_map) + 
  stat_summary_2d(data = cycsyd_a, aes(x=lon, y=lat, z=price_peak_median)) + 
  scale_fill_viridis()


ggplot(cycsyd_a, aes(x=price_low_median, price_peak_median, col=Brand2)) +
  geom_point() +
  scale_colour_manual(values=RColorBrewer::brewer.pal(7,"Set3")) +
  theme_bw()



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


ggplot(fuelu91syd_ave, aes(x=Date, y=Price_k3, col=Brand2)) +
  geom_line(lwd=0.9) +
  theme_bw() +
  scale_colour_manual(values=c(beyonce_palette(127), "goldenrod"))






seifa <- read.csv("data/aus_seifa_2011_by_postcode.csv")

cycsyd_a$POA <- as.numeric(gsub(".+(NSW )([0-9]{4})","\\2", as.character(cycsyd_a$Address)))

cycsyd_a <- left_join(cycsyd_a, seifa, by="POA")


filter(cycsyd_a, price_low_median < 125) %>%
  ggplot(aes(x=seifa_score, y=price_low_median, col=Brand2)) + 
    geom_point() +
    geom_smooth(se=FALSE) +
    scale_colour_manual(values=RColorBrewer::brewer.pal(7,"Set3")) +
  theme_bw()



       
       
       