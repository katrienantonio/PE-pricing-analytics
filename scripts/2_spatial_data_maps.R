## -----------------------------------------------------------------------------------
library(rgdal)
readShapefile = function(){
  belgium_shape <- readOGR(dsn = path.expand(paste(getwd(), "./shape file Belgie postcodes", sep = "")), 
                           layer = "npc96_region_Project1")
  belgium_shape <- spTransform(belgium_shape, CRS("+proj=longlat +datum=WGS84"))
  belgium_shape$id <- row.names(belgium_shape)
  return(belgium_shape)
}

belgium_shape = readShapefile()
class(belgium_shape)


## -----------------------------------------------------------------------------------
str(belgium_shape@data)

## -----------------------------------------------------------------------------------
fortify(belgium_shape) %>% slice(1:3)

## -----------------------------------------------------------------------------------
plot.eda.map <- ggplot(belgium_shape, aes(long, lat, group = group)) + geom_polygon(fill = NA, colour = "black", size = 0.1) + theme_bw()
plot.eda.map

## -----------------------------------------------------------------------------------
library(mapview)
mapview(belgium_shape)


## -----------------------------------------------------------------------------------
library(sf)
belgium_shape_sf <- st_read('./shape file Belgie postcodes/npc96_region_Project1.shp', quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, CRS("+proj=longlat +datum=WGS84"))


## -----------------------------------------------------------------------------------
class(belgium_shape_sf)
belgium_shape_sf %>% as_tibble() %>% select(-geometry) %>% slice(1:3) 

## -----------------------------------------------------------------------------------
ggplot(belgium_shape_sf) +
  geom_sf() +
  ggtitle("Welcome to Belgium!") +
  theme_bw()

## -----------------------------------------------------------------------------------
library(tmap)
# qtm(belgium_shape_sf) # does not work
# shapefile slightly corrupted!

# slightly smooth the shapefile
simple_shp <- st_simplify(belgium_shape_sf, 
                          dTolerance = 0.00001)

# and plot
qtm(simple_shp)

## -----------------------------------------------------------------------------------
tm_shape(simple_shp) +
  tm_borders(col = KULbg, lwd = 0.5) +
  tm_layout(main.title = 'Welcome to Belgium!', legend.outside = TRUE, frame = FALSE) 


## -----------------------------------------------------------------------------------
post_expo <- mtpl %>% group_by(pc) %>% summarize(num = n(), total_expo = sum(expo)) 

post_expo %>% slice(1:5) 


## -----------------------------------------------------------------------------------
library(dplyr)
belgium_shape@data <- left_join(belgium_shape@data, post_expo, by = c("POSTCODE" = "pc"))
belgium_shape@data %>% slice(1:3) 


## -----------------------------------------------------------------------------------
post_expo %>% filter(pc == 1301) 


## -----------------------------------------------------------------------------------
belgium_shape@data$freq <- belgium_shape@data$total_expo/belgium_shape@data$Shape_Area


## -----------------------------------------------------------------------------------
belgium_shape@data$freq_class <- cut(belgium_shape@data$freq, breaks = quantile(belgium_shape@data$freq, c(0,0.2,0.8,1), na.rm = TRUE), right = FALSE, include.lowest = TRUE, labels = c("low","average","high"))

belgium_shape@data %>% slice(1:3) 


## -----------------------------------------------------------------------------------
belgium_shape_f <- fortify(belgium_shape)


## -----------------------------------------------------------------------------------
belgium_shape_f <- left_join(belgium_shape_f, belgium_shape@data)

## -----------------------------------------------------------------------------------
plot.eda.map <- ggplot(belgium_shape_f, aes(long, lat, group = group)) + geom_polygon(aes(fill = belgium_shape_f$freq_class), colour = "black", size = 0.1)
plot.eda.map <- plot.eda.map + theme_bw() + labs(fill = "Relative\nfrequency") + scale_fill_brewer(palette = "Blues", na.value = "white")
plot.eda.map