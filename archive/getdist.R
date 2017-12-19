

loc  <- read.csv("Fuel_NSW_addresses_latlon.csv")

library(dplyr)
library(janitor)
locsub <- filter(loc, lon, lat) %>%
  mutate(eventDate = "2016-6-1")

write.csv(locsub, "NSW_fuel_locations_lonlatonly.csv", row.names=FALSE)

# remoteness index from ALA
#http://spatial.ala.org.au/webportal//#
remo <- read.csv("NSW_fuel_ALA_remoteness_locations.csv") %>%
  clean_names %>%
  dplyr::select(locality, latitude_original, longitude_original,
                remoteness_index, distance_to_coast) %>%
  rename(address =locality,
         lat=latitude_original,
         lon=longitude_original,
         remoteness=remoteness_index,
         dist_to_coast=distance_to_coast)
write.csv(remo, "NSW_fuel_ALA_remoteness_locations_cleaned.csv", row.names=F)


windows(6,5)
library(oz)
oz(sections=c(4, 13:15))
cols <- colorRampPalette(c("yellow","darkorange","red"))(10)
with(remo, points(lon,lat, pch=19, col=cols[cut(log(remoteness+1), 10)]))



