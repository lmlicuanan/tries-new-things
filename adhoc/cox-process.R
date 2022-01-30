library("spocc")

df <- occ(query = "Bradypus variegatus", from = "gbif",
          date = c("2000-01-01", "2019-12-31"),
          gbifopts = list(country = "CR"),
          has_coords = TRUE, limit = 1000)

names(df)
d <- occ2df(df)
summary(d)

library(sp)
dpts <- SpatialPoints(d[, c("longitude", "latitude")])

library(tmap)
tmap_mode("view")
tm_basemap(leaflet::providers$OpenStreetMap) +
  tm_shape(dpts) + tm_dots()

library(raster)
library(httr)
set_config(config(ssl_verifypeer = 0L))
rmonth <- getData(name = "worldclim", var = "tmin", res = 10)
rcov <- mean(rmonth)

library(rnaturalearth)
map <- ne_countries(type = "countries", country = "Costa Rica", scale = "medium")
resolution <- 0.1
r <- raster(map, resolution = resolution)
(nrow <- nrow(r))
(ncol <- ncol(r))
nrow*ncol

r[] <- 0
tab <- table(cellFromXY(r, dpts))
r[as.numeric(names(tab))] <- tab

grid <- rasterToPolygons(r)
grid <- grid[as.vector(t(matrix(1:nrow(grid), nrow = ncol, ncol = nrow))), ]
grid$id <- 1:nrow(grid)
grid$Y <- grid$layer
grid$cellarea <- resolution*resolution

grid$cov <- extract(rcov, coordinates(grid))
