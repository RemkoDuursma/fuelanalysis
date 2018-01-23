

metro <- filter(fuel, Address %in% c("741 George St, Windsor South NSW 2756",
                                  "552 Pennant Hills Road, West Pennant Hills NSW 2125"))

remot <- filter(fuel, Address %in% c("1 Blende St, Broken Hill NSW 2880",
                                     "119 Bourke St Cnr Erskine St, Dubbo NSW 2830"))


library(ggplot2)


g1 <- filter(metro, FuelCode == "U91") %>%
  ggplot(aes(x=Date, y=Price, col=Address)) +
  geom_line() +
  geom_point(size=1) +
  theme_bw() +
  theme(legend.position="none")
                                  
g2 <- filter(remot, FuelCode == "U91") %>%
  ggplot(aes(x=Date, y=Price, col=Address)) +
  geom_line() +
  geom_point(size=1) +
  theme_bw() +
  theme(legend.position="none")

library(gridExtra)
grid.arrange(g1, g2, ncol=2)

brok <- filter(fuel, grepl("Bourke", Address))

filter(brok, FuelCode == "U91") %>% 
  ggplot(aes(x=Date, y=Price)) +
  geom_point(size=1.5, col="red") +
  geom_line(col="darkgrey") + 
  theme_bw() +
  facet_wrap(~Address)