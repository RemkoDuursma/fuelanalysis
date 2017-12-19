

# devtools::install_github("remkoduursma/fuelpricensw")
library(fuelpricensw)
data(fuel)

locs <- readRDS("cache/locs.rds")

fuel <- left_join(fuel, locs, by="Address")
