

# devtools::install_github("remkoduursma/fuelpricensw")
library(fuelpricensw)
data(fuel)

locs <- readRDS("cache/locs.rds")

library(dplyr)

fuel_sub <- filter(fuel, FuelCode %in% c("E10","P95","P98","U91"))

fuel_sub_a <- group_by(fuel_sub, Address, FuelCode, Date) %>%
  summarize(Price = mean(Price, na.rm=TRUE),
            ServiceStationName = first(ServiceStationName),
            Brand = first(Brand)) %>%
  ungroup


fuelx <- group_by(fuel_sub_a, Address, FuelCode) %>%
  summarize(Price = median(Price, na.rm=TRUE),
            ServiceStationName = first(ServiceStationName),
            Brand = first(Brand),
            n = n()) %>%
  ungroup %>%
  left_join(locs, by="Address")
  

boxplot(Price ~ FuelCode, data=fuelx)

# figure
library(ggplot2)
plot_address_time <- function(here){
  
  filter(fuel_sub, Address == here) %>%
  ggplot(aes(x=Date, y=Price, col=FuelCode)) + geom_line() +
    theme_bw() + geom_point() +
    scale_colour_manual(values=gplots::rich.colors(5)) +
    labs(title=sprintf("%s - %s", which(locs$Address == here), here))
}

plot_address_time("1403 Princes Hwy, Heathcote NSW 2233")


plot_address_time(sample(locs$Address,1))


#--- plotting

library(ggmap)
qmplot(lon, lat, data = locs)

get_googlemap("sydney australia", zoom = 10) %>% ggmap()

syd <- c(left = 150.6, bottom = -34.25, right =151.4, top = -33.5)
mp <- get_stamenmap(syd, zoom = 10, maptype = "toner-lite")


ggmap(mp) + geom_point(aes(x=lon, y=lat, colour=remoteness), data=locs)


#---- hierarchical clustering

# Wide format
library(reshape2)
d98 <- filter(fuel_sub_a, FuelCode == "P98") %>% 
  dcast(Date ~ Address, value.var="Price")

# Data only
m98 <- d98[,-1]

# Remove columns with only few observations
ni <- sapply(m98, function(x)sum(!is.na(x)))
m98 <- m98[, ni > 10]




# figure; demonstrate NA filling
# x3 <- na.fill(x, c("extend",NA,"extend"))
# x3 <- na.spline(x3)
# plot(x)
# lines(x3)

library(zoo)
impute_na <- function(x){
  x <- na.fill(x, c("extend",NA,"extend"))
  x <- na.spline(x)
return(x)
}

m98 <- apply(m98, 2, impute_na)

# Finally, transpose because that makes more sense!
m98 <- t(m98)

# but really have to get rid of more data...
# plot(d98[,6])
# lines(m98[,5])


library(cluster)

dian1 <- diana(m98)
pltree(dian1, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# and see dendextend package
# dend1 <- as.dendrogram(dian1)
# plot(dend1)

clust <- cutree(dian1, k = 6)

library(factoextra)
fviz_cluster(list(data = m98, cluster = clust)) 


km1 <- kmeans(m98, centers = 5)


cl <- data.frame(Address = rownames(m98), cluster = km1$cluster)
locs2 <- left_join(locs, cl, by="Address")

with(locs2, plot(lon, lat, col=cluster))

ggmap(mp) + geom_point(aes(x=lon, y=lat, colour=cluster), data=locs2)




fuelx2 <- filter(fuelx, FuelCode == "P98") %>% left_join(cl, by="Address")

fuelx2_syd <- filter(fuelx2, lon > syd["left"], 
                     lon < syd["right"],
                     lat > syd["bottom"],
                     lat < syd["top"])

fuelx2_syd$Price_sc <- as.vector(scale(fuelx2_syd$Price))

library(gtools)
fuelx2_syd$Price_cut <- quantcut(fuelx2_syd$Price, q = 5)

#!!
ggmap(mp) + 
  stat_density2d(aes(x=lon, y=lat, fill = ..level..), 
                 geom="polygon", 
                 alpha=0.2,
                 data=fuelx2_syd) + 
  scale_fill_gradient2("Service\nStations", low = "white", mid = "yellow", high = "red") +
  geom_point(aes(x=lon, y=lat, colour=Price_cut), data=fuelx2_syd) +
  scale_colour_manual("Price", values=heat.colors(5))

+
  scale_colour_gradient(low = "blue", high="red")



ggplot(fuelx2_syd, aes(x=Brand, y=Price)) + geom_boxplot()



#----- wavelets
d1 <- filter(fuel_sub, Address == locs$Address[325], FuelCode == "E10")
d2 <- filter(fuel_sub, Address == locs$Address[1104], FuelCode == "E10")
  

library(wavelets)

x <- dwt(d1$Price, filter="haar")
plot(x)

y <- dwt(d2$Price)
plot(y)

summary(x)


library(WaveletComp)

f <- filter(fuel_sub, Address == locs$Address[325], FuelCode == "E10") %>%
  rename(date = Date) %>%
  dplyr::select(date, Price)

a <- analyze.wavelet(f, 2)

windows(8,8)
wt.image(a)

wt.avg(a)

reconstruct(a)
