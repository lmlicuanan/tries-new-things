httr::set_config(httr::config(ssl_verifypeer = FALSE))

suppressPackageStartupMessages({
  library(osmdata)
  library(tidyverse)
  library(sf)
  library(tmap)
  library(gstat)
  library(stars)
})

study_region.bb <- getbb("Makati", format_out = "polygon")

building.ql <- 
  study_region.bb %>% opq() %>% 
  add_osm_feature(key = "building")

road.ql <- 
  study_region.bb %>% opq() %>% 
  add_osm_feature(key = "highway")

boundary.ql <- 
  study_region.bb %>% opq() %>% 
  add_osm_feature(key = "admin_level", value = "10")

amenity.ql <- 
  study_region.bb %>% opq() %>% 
  add_osm_feature(key = "amenity")

building.sf <- building.ql %>% osmdata_sf()
road.sf     <- road.ql %>% osmdata_sf()
boundary.sf <- boundary.ql %>% osmdata_sf()
amenity.sf  <- amenity.ql %>% osmdata_sf()
mc_point.sf <- st_as_sf(st_sample(building.sf$osm_polygons, 100))

mc_point_params.sf <- aggregate(
  amenity.sf$osm_points %>% 
    filter(amenity == "bank") %>% 
    transmute(nearby_banks = 1),
  mc_point.sf,
  FUN = sum,
  join = function(x, y) st_is_within_distance(x, y, dist = 1000)
) 

ggplot() +
  # geom_sf(data = boundary.sf$osm_multipolygons) +
  geom_sf(data = building.sf$osm_polygons) +
  geom_sf(data = road.sf$osm_lines) +
  geom_sf(data = amenity.sf$osm_points %>% filter(amenity == "bank"), col = "red") +
  geom_sf(data = mc_point_params.sf, aes(colour = nearby_banks, size = nearby_banks)) +
  scale_colour_viridis_c() +
  scale_size_continuous(guide = "none")

httr::set_config(httr::config(ssl_verifypeer = TRUE))

##################################################### SCRATCH

crs = st_crs("EPSG:32632")
no2 = read_csv(system.file("external/no2.csv", package = "gstat"))
no2.sf = st_as_sf(
  no2, crs = "OGC:CRS84",
  coords = c("station_longitude_deg", "station_latitude_deg")
) %>% st_transform(crs)

data(air, package = "spacetime") # this loads German boundaries into DE_NUTS1
de <- st_transform(st_as_sf(DE_NUTS1), crs)

ggplot() + geom_sf(data = de) +  geom_sf(data = no2.sf, mapping = aes(col = NO2))

st_bbox(de) %>%
  st_as_stars(dx = 10000) %>%
  st_crop(de) -> grd

i = idw(NO2~1, no2.sf, grd)

ggplot() + geom_stars(data = i, aes(fill = var1.pred, x = x, y = y)) + 
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf)

v = variogram(NO2~1, no2.sf)
plot(v, plot.numbers = TRUE)
v0 = variogram(NO2~1, no2.sf, cutoff = 100000, width = 10000)
plot(v0, plot.numbers = TRUE)
v.m = fit.variogram(v, vgm(1, "Exp", 50000, 1))
plot(v, v.m, plot.numbers = TRUE)
k = krige(NO2~1, no2.sf, grd, v.m)

ggplot() + geom_stars(data = k, aes(fill = var1.pred, x = x, y = y)) + 
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf)

a = aggregate(no2.sf["NO2"], by = de, FUN = mean)
b = krige(formula = NO2~1, locations = no2.sf, newdata = de, model = v.m)
b$sample = a$NO2
b$kriging = b$var1.pred

b %>% select(sample, kriging) %>% 
  pivot_longer(1:2, names_to = "var", values_to = "NO2") -> b2
b2$var = factor(b2$var, levels = c("sample", "kriging"))
ggplot() + geom_sf(data = b2, mapping = aes(fill = NO2)) + facet_wrap(~var) +
  scale_fill_gradientn(colors = sf.colors(20))

SE = function(x) sqrt(var(x)/length(x))
a = aggregate(no2.sf["NO2"], de, SE)

b$sample = a$NO2
b$kriging = sqrt(b$var1.var)

b %>% select(sample, kriging) %>% 
  pivot_longer(1:2, names_to = "var", values_to = "Standard_error") -> b2
b2$var = factor(b2$var, levels = c("sample", "kriging"))
ggplot() + geom_sf(data = b2, mapping = aes(fill = Standard_error)) + facet_wrap(~var, as.table = FALSE) +
  scale_fill_gradientn(colors = sf.colors(20))

#####################################################
#retrieve bounding box for region of interest
iz_bbox <- getbb("Makati", format_out = "polygon")

#retrieve level 8 administrative boundaries 
iz_boundary <- opq(iz_bbox) %>%
  add_osm_feature(key = "admin_level", value = "10") %>%
  osmdata_sf()

iz_boundary

#select only df multipolygons
iz_polys <- iz_boundary$osm_multipolygons

#remove digits from any distirct name 
iz_polys$name <- gsub('[[:digit:]]+', '', iz_polys$name)
#remove . from any district name
iz_polys$name <- gsub("[.]", '', iz_polys$name)
#trim whitespace
iz_polys$name  <- trimws(iz_polys$name, "both")
#factorize 
iz_polys$name <- as.factor(iz_polys$name)

#calculate polygon areas for later analysis and append to new column
iz_polys$poly_area <- st_area(iz_polys)

#remove original osmdata object
rm(iz_boundary)

ggplot(iz_polys) +
  geom_sf()

iz_buildings <- opq(iz_bbox) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

build_polys <- iz_buildings$osm_polygons
rm(iz_buildings)

#drop unecessary columns
build_polys <- build_polys %>%
  select(osm_id, geometry)

ggplot() +
  geom_sf() +
  geom_sf(data = build_polys)

#calculate surface area of buildings
build_polys$area <- sf::st_area(build_polys)
#calculate centroids
build_cents <- sf::st_centroid(build_polys)

#create a shape object out of original bounding polygon
iz_bbox_geom <- 
  sfheaders::sf_polygon(as.data.frame(iz_bbox),
                        x="V1",
                        y="V2"
  )
#make sure that the projection matches the other data
st_crs(iz_bbox_geom) <- 4326

#plot
ggplot(iz_polys) +
  geom_sf() +
  geom_sf(data=iz_bbox_geom, col="red")

#filtering join with a points df, polygon df
clipped <- st_join(build_cents, iz_bbox_geom, join = st_within)

clipped %>%
  filter(id == 1) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data=iz_bbox_geom, color = 'red', fill = NA)

clipped <- clipped %>%
  filter(id == 1)
joined <- st_join(clipped, iz_polys)

#aggregating and summing total building area
density_calc <- aggregate(joined$area, list(joined$osm_id.y),
                          FUN = sum)
#rename columns
colnames(density_calc) <- c("osm_id", "area")

#create final df that contains district polygons and building area
bounds_blds_sf <- merge(iz_polys, density_calc) 
#calculate building density
bounds_blds_sf <- bounds_blds_sf %>%
  mutate(bounds_blds_sf, b_dens = area/poly_area * 100)

tmap_mode('plot')

tm_basemap("Stamen.TonerLite") +
  tm_shape(bounds_blds_sf) +
  tm_polygons(col="b_dens",
              id="name",
              title= "Building Density as % of Land Area",
              alpha=.8) 
