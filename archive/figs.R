

library(leaflet)
library(htmltools)

pal <- colorBin(
  palette = "YlOrRd",
  bins=c(seq(100,140,by=5),300),
  domain = fuel91s$Price)

station_popup <- paste0(fuel91s$Address, 
                        "<br><strong>Price U91 (median): </strong>", 
                        round(fuel91s$Price,1)) %>% 
  lapply(HTML)

leaflet(data=fuel91s) %>%
  addTiles() %>%
  addCircleMarkers(color = ~pal(Price), 
                   label=station_popup,
                   radius=2.5, opacity=0.8, fillOpacity=0.4)


